:- module(musicxml, 
    [   write_musicxml/4, 
        write_musicxml/2,
        write_counterpoint_xml/4,
        write_counterpoint_xml/5,
        write_second_species_xml/5,
        write_second_species_xml/6,
        write_third_species_xml/5,
        write_third_species_xml/6,
        write_fourth_species_xml/5,
        write_fourth_species_xml/6,
        fourth_score_attacks/3,
        fifth_score_attacks/3,
        write_fifth_species_xml/5,
        write_fifth_species_xml/6,
        write_solutions_musicxml/6,
        write_solutions_musicxml/7,
        valid_diminution_measures/1,
        write_diminution_xml/3,
        write_diminution_xml/4,
        write_diminution_solutions_xml/3,
        write_diminution_solutions_xml/4,
        gen_filename/4
    ]).

:- use_module(library(sgml)).
:- use_module(library(sgml_write)).

gen_filename(Path,Prefix,Ext, Filename) :- get_time(TS), stamp_date_time(TS,Date,local), 
							date_time_value(year,Date,Y),						
							date_time_value(month,Date,M),
							date_time_value(day,Date,D),
							date_time_value(hour,Date,H),				
							date_time_value(minute,Date,Min),						
							date_time_value(second,Date,S),
							Secs is floor(S*100),
							atomics_to_string([Y,M,D,H,Min,Secs],'-',Filesuffix),
							atom_string(SuffixAtom,Filesuffix),
                            re_replace(" "/g, "_", Prefix, PrefixTransformed),
                            atom_string(Atom1,PrefixTransformed),
							atom_concat(Path,Atom1, Atom2),
                            atom_concat(Atom2, SuffixAtom, Atom3),
                            atom_concat(Atom3,Ext,Filename).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write_musicxml(+Phrases, +Filename, +Title, +Subtitle)
%
% Writes a MusicXML file for the given list of Phrases.
% The XML includes:
%   - A <work-title> for the main Title
%   - A <movement-title> for the Subtitle
%   - A single part (id="P1")
%   - Each Phrase is placed in its own <measure>, with a forward-repeat barline
%     at the start and a backward-repeat barline at the end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_musicxml(Phrases, Filename, Title, Subtitle) :-
    setup_call_cleanup(
        open(Filename, write, Stream, [encoding(utf8)]),
        (xml_header(Stream), write_score(Phrases, Title, Subtitle, Stream)),
        close(Stream)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Default version: if no Title and Subtitle are provided,
% use default values.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_musicxml(Phrases, Filename) :-
    DefaultTitle = "Untitled",
    DefaultSubtitle = "",
    write_musicxml(Phrases, Filename, DefaultTitle, DefaultSubtitle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% xml_header(+Stream)
%
% Writes the XML declaration.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
xml_header(Stream) :-
    format(Stream, '<?xml version="1.0" encoding="UTF-8"?>~n', []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write_score(+Phrases, +Title, +Subtitle, +Stream)
%
% Writes the overall MusicXML score in score-partwise format.
% Inserts the Title and Subtitle.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_score(Phrases, Title, Subtitle, Stream) :-
    format(Stream, '<score-partwise version="3.1">~n', []),
    % Insert work title (the overall title) and movement title (the subtitle)
    format(Stream, '  <work>~n', []),
    xml_escape(Title, EscapedTitle),
    format(Stream, '    <work-title>~s</work-title>~n', [EscapedTitle]),
    format(Stream, '  </work>~n', []),
    xml_escape(Subtitle, EscapedSubtitle),
    format(Stream, '  <movement-title>~s</movement-title>~n', [EscapedSubtitle]),
    % Write the part list.
    format(Stream, '  <part-list>~n', []),
    format(Stream, '    <score-part id="P1"><part-name>Music</part-name></score-part>~n', []),
    format(Stream, '  </part-list>~n', []),
    % Write the single part (id="P1").
    format(Stream, '  <part id="P1">~n', []),
    write_measures(Phrases, 1, Stream),
    format(Stream, '  </part>~n', []),
    format(Stream, '</score-partwise>~n', []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write_measures(+Phrases, +StartNumber, +Stream)
%
% Writes each phrase as a measure, placing forward and backward repeat barlines
% at the start and end of each measure respectively.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_measures([], _, _).
write_measures([Phrase|Rest], Num, Stream) :-
    format(Stream, '    <measure number="~w">~n', [Num]),
    % Left barline with repeat (forward)
    format(Stream, '      <barline location="left"><repeat direction="forward"/></barline>~n', []),
    ( Num =:= 1 ->
        format(Stream, '      <attributes><divisions>1</divisions><time><senza-misura/></time><clef><sign>G</sign><line>2</line></clef></attributes>~n', [])
    ; true
    ),
    write_notes(Phrase, Stream),
    % Right barline with repeat (backward)
    format(Stream, '      <barline location="right"><repeat direction="backward"/></barline>~n', []),
    format(Stream, '    </measure>~n', []),
    NextNum is Num + 1,
    write_measures(Rest, NextNum, Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write_notes(+Phrase, +Stream)
%
% Writes each note in the phrase.
% (This example assumes that each note is rendered as a whole note.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_notes([], _).
write_notes([Note|Rest], Stream) :-
    note_to_musicxml(Note, NoteXML),
    format(Stream, '      ~s~n', [NoteXML]),
    write_notes(Rest, Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% note_to_musicxml(+NoteTriple, -XML)
%
% Converts a note triple [Name,Acc,Oct] into a MusicXML <note> element.
% The note is rendered as a whole note by default.
%   - <step> is the uppercase letter of the note name.
%   - <alter> is included if Acc is nonzero.
%   - <octave> is the octave.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
note_to_musicxml([Name, Acc, Oct], XML) :-
    atom_chars(Name, [Char]),
    upcase_atom(Char, Upper),
    ( Acc =:= 0 ->
            Alter = ""
    ; Acc =:= 1 ->
            Alter = "<alter>1</alter>"
    ; Acc =:= -1 ->
            Alter = "<alter>-1</alter>"
    ),
    format(string(XML),
            '<note><pitch><step>~w</step>~w<octave>~w</octave></pitch><duration>4</duration><type>whole</type></note>',
            [Upper, Alter, Oct]).

% Escape XML text content; accept atoms and strings.
xml_escape(Text, Escaped) :-
    text_to_string(Text, String),
    string_codes(String, Codes),
    phrase(xml_text(Codes), EscapedCodes),
    string_codes(Escaped, EscapedCodes).

text_to_string(Text, Text) :- string(Text), !.
text_to_string(Text, String) :- atom_string(Text, String).

xml_text([]) --> [].
xml_text([C|Cs]) --> xml_char(C), xml_text(Cs).
xml_char(0'&) --> "&amp;", !.
xml_char(0'<) --> "&lt;", !.
xml_char(0'>) --> "&gt;", !.
xml_char(C) --> [C].

% Write equal-length upper and lower voices on one treble staff or a piano
% grand staff. Both voices start together and use whole notes.
write_counterpoint_xml(Upper, Lower, Filename, Layout) :-
    write_counterpoint_xml(Upper, Lower, Filename, Layout, "Counterpoint").

write_counterpoint_xml(Upper, Lower, Filename, Layout, Title) :-
    memberchk(Layout, [single, piano, guitar]),
    is_list(Upper), is_list(Lower), Upper = [_|_],
    same_length(Upper, Lower),
    maplist(valid_xml_note, Upper), maplist(valid_xml_note, Lower),
    xml_escape(Title, SafeTitle),
    prepare_layout_notes(Layout, Upper, WrittenUpper),
    prepare_layout_notes(Layout, Lower, WrittenLower),
    setup_call_cleanup(
        open(Filename, write, Stream, [encoding(utf8)]),
        write_counterpoint_score(Stream, WrittenUpper, WrittenLower, Layout, SafeTitle),
        close(Stream)).

valid_xml_note([Name, Acc, Oct]) :-
    memberchk(Name, [c,d,e,f,g,a,b]),
    integer(Acc), between(-1, 1, Acc),
    integer(Oct), between(0, 9, Oct).

write_counterpoint_score(Stream, Upper, Lower, Layout, Title) :-
    xml_header(Stream),
    format(Stream, '<score-partwise version="3.1">~n', []),
    format(Stream, '  <work><work-title>~s</work-title></work>~n', [Title]),
    layout_part_name(Layout, PartName),
    format(Stream, '  <part-list><score-part id="P1"><part-name>~w</part-name></score-part></part-list>~n', [PartName]),
    format(Stream, '  <part id="P1"><measure number="1">~n', []),
    format(Stream, '    <attributes><divisions>1</divisions><time><senza-misura/></time>', []),
    ( Layout = piano ->
        format(Stream, '<staves>2</staves><clef number="1"><sign>G</sign><line>2</line></clef><clef number="2"><sign>F</sign><line>4</line></clef>', []),
        LowerStaff = 2
    ; write_single_clef(Stream, Layout), LowerStaff = 1
    ),
    format(Stream, '</attributes>~n', []),
    write_voice(Upper, 1, 1, Stream),
    length(Upper, Count), Duration is Count * 4,
    format(Stream, '    <backup><duration>~d</duration></backup>~n', [Duration]),
    write_voice(Lower, 2, LowerStaff, Stream),
    format(Stream, '  </measure></part>~n</score-partwise>~n', []).

write_voice([], _, _, _).
write_voice([[Name,Acc,Oct]|Notes], Voice, Staff, Stream) :-
    upcase_atom(Name, Step),
    format(Stream, '    <note><pitch><step>~w</step>', [Step]),
    ( Acc =:= 0 -> true ; format(Stream, '<alter>~d</alter>', [Acc]) ),
    format(Stream, '<octave>~d</octave></pitch><duration>4</duration><voice>~d</voice><type>whole</type><staff>~d</staff></note>~n', [Oct,Voice,Staff]),
    write_voice(Notes, Voice, Staff, Stream).

% Position is the position of the counterpoint relative to the cantus.
write_second_species_xml(Cantus, Counterpoint, Position, Filename, Layout) :-
    write_second_species_xml(Cantus, Counterpoint, Position, Filename, Layout,
                             "Second-species counterpoint").

write_second_species_xml(Cantus, Counterpoint, Position, Filename, Layout, Title) :-
    memberchk(Position, [above,below]), memberchk(Layout, [single,piano,guitar]),
    is_list(Cantus), Cantus = [_|_], is_list(Counterpoint),
    length(Cantus, N), length(Counterpoint, M), M =:= 2*N-1,
    maplist(valid_xml_note, Cantus), maplist(valid_xml_note, Counterpoint),
    xml_escape(Title, SafeTitle),
    prepare_layout_notes(Layout, Cantus, WrittenCantus),
    prepare_layout_notes(Layout, Counterpoint, WrittenCounterpoint),
    setup_call_cleanup(open(Filename, write, Stream, [encoding(utf8)]),
        write_second_score(Stream, WrittenCantus, WrittenCounterpoint, Position, Layout, SafeTitle),
        close(Stream)).

write_second_score(Stream, Cantus, Counterpoint, Position, Layout, Title) :-
    xml_header(Stream),
    format(Stream, '<score-partwise version="3.1">~n', []),
    format(Stream, '  <work><work-title>~s</work-title></work>~n', [Title]),
    layout_part_name(Layout, PartName),
    format(Stream, '  <part-list><score-part id="P1"><part-name>~w</part-name></score-part></part-list>~n', [PartName]),
    format(Stream, '  <part id="P1">~n', []),
    write_second_measures(Stream, Cantus, Counterpoint, Position, Layout, 1),
    format(Stream, '  </part>~n</score-partwise>~n', []).

write_second_measures(Stream, [C|Cs], Notes, Position, Layout, Number) :-
    ( Cs = [] -> Notes = [N], Added = [N], Type = whole
    ; Notes = [N1,N2|Rest], Added = [N1,N2], Type = half ),
    format(Stream, '    <measure number="~d">~n', [Number]),
    ( Number =:= 1 -> write_second_attributes(Stream, Layout) ; true ),
    ( Position = above -> Upper = Added, UpperType = Type,
                          Lower = [C], LowerType = whole
    ; Upper = [C], UpperType = whole, Lower = Added, LowerType = Type ),
    ( Layout = piano -> LowerStaff = 2 ; LowerStaff = 1 ),
    write_rhythmic_voice(Upper, UpperType, 1, 1, Stream),
    format(Stream, '      <backup><duration>4</duration></backup>~n', []),
    write_rhythmic_voice(Lower, LowerType, 2, LowerStaff, Stream),
    format(Stream, '    </measure>~n', []),
    ( Cs = [] -> true
    ; NextNumber is Number+1,
      write_second_measures(Stream, Cs, Rest, Position, Layout, NextNumber) ).

write_second_attributes(Stream, Layout) :-
    format(Stream, '      <attributes><divisions>1</divisions><time><beats>4</beats><beat-type>4</beat-type></time>', []),
    ( Layout = piano ->
        format(Stream, '<staves>2</staves><clef number="1"><sign>G</sign><line>2</line></clef><clef number="2"><sign>F</sign><line>4</line></clef>', [])
    ; write_single_clef(Stream, Layout) ),
    format(Stream, '</attributes>~n', []).

write_rhythmic_voice([], _, _, _, _).
write_rhythmic_voice([[Name,Acc,Oct]|Notes], Type, Voice, Staff, Stream) :-
    ( Type = whole -> Duration = 4 ; Type = half -> Duration = 2 ; Duration = 1 ),
    upcase_atom(Name, Step),
    format(Stream, '      <note><pitch><step>~w</step>', [Step]),
    ( Acc =:= 0 -> true ; format(Stream, '<alter>~d</alter>', [Acc]) ),
    format(Stream, '<octave>~d</octave></pitch><duration>~d</duration><voice>~d</voice><type>~w</type><staff>~d</staff></note>~n', [Oct,Duration,Voice,Type,Staff]),
    write_rhythmic_voice(Notes, Type, Voice, Staff, Stream).

% Four quarter notes against each cantus whole note, ending in two wholes.
write_third_species_xml(Cantus, Counterpoint, Position, Filename, Layout) :-
    write_third_species_xml(Cantus, Counterpoint, Position, Filename, Layout,
                            "Third-species counterpoint").

write_third_species_xml(Cantus, Counterpoint, Position, Filename, Layout, Title) :-
    memberchk(Position, [above,below]), memberchk(Layout, [single,piano,guitar]),
    is_list(Cantus), Cantus = [_|_], is_list(Counterpoint),
    length(Cantus, N), length(Counterpoint, M), M =:= 4*N-3,
    maplist(valid_xml_note, Cantus), maplist(valid_xml_note, Counterpoint),
    xml_escape(Title, SafeTitle),
    prepare_layout_notes(Layout, Cantus, WrittenCantus),
    prepare_layout_notes(Layout, Counterpoint, WrittenCounterpoint),
    setup_call_cleanup(open(Filename, write, Stream, [encoding(utf8)]),
        write_third_score(Stream, WrittenCantus, WrittenCounterpoint, Position, Layout, SafeTitle),
        close(Stream)).

write_third_score(Stream, Cantus, Counterpoint, Position, Layout, Title) :-
    xml_header(Stream),
    format(Stream, '<score-partwise version="3.1">~n', []),
    format(Stream, '  <work><work-title>~s</work-title></work>~n', [Title]),
    layout_part_name(Layout, PartName),
    format(Stream, '  <part-list><score-part id="P1"><part-name>~w</part-name></score-part></part-list>~n', [PartName]),
    format(Stream, '  <part id="P1">~n', []),
    write_third_measures(Stream, Cantus, Counterpoint, Position, Layout, 1),
    format(Stream, '  </part>~n</score-partwise>~n', []).

write_third_measures(Stream, [C|Cs], Notes, Position, Layout, Number) :-
    ( Cs = [] -> Notes = [N], Added = [N], Type = whole
    ; Notes = [N1,N2,N3,N4|Rest], Added = [N1,N2,N3,N4], Type = quarter ),
    format(Stream, '    <measure number="~d">~n', [Number]),
    ( Number =:= 1 -> write_second_attributes(Stream, Layout) ; true ),
    ( Position = above -> Upper = Added, UpperType = Type,
                          Lower = [C], LowerType = whole
    ; Upper = [C], UpperType = whole, Lower = Added, LowerType = Type ),
    ( Layout = piano -> LowerStaff = 2 ; LowerStaff = 1 ),
    write_rhythmic_voice(Upper, UpperType, 1, 1, Stream),
    format(Stream, '      <backup><duration>4</duration></backup>~n', []),
    write_rhythmic_voice(Lower, LowerType, 2, LowerStaff, Stream),
    format(Stream, '    </measure>~n', []),
    ( Cs = [] -> true
    ; NextNumber is Number+1,
      write_third_measures(Stream, Cs, Rest, Position, Layout, NextNumber) ).

% Validate the tie structure and return the actual note attacks. Rest and
% tied continuations are excluded from the returned list.
fourth_score_attacks(Cantus, Measures, Attacks) :-
    is_list(Cantus), length(Cantus, N), N >= 3,
    maplist(valid_xml_note, Cantus), is_list(Measures), same_length(Cantus, Measures),
    Measures = [[rest,First]|Rest], valid_xml_note(First),
    fourth_score_tail(Rest, First, AttacksTail), Attacks = [First|AttacksTail].

fourth_score_tail([[Final]], _, [Final]) :- valid_xml_note(Final).
fourth_score_tail([[Strong,Weak],[Final]], _, [Strong,Weak,Final]) :-
    valid_xml_note(Strong), valid_xml_note(Weak), valid_xml_note(Final).
fourth_score_tail([[tie(Held),Weak]|Rest], Previous, [Weak|Attacks]) :-
    Held == Previous, valid_xml_note(Weak), Rest = [_|_],
    fourth_score_tail(Rest, Weak, Attacks).

write_fourth_species_xml(Cantus, Measures, Position, Filename, Layout) :-
    write_fourth_species_xml(Cantus, Measures, Position, Filename, Layout,
                             "Fourth-species counterpoint").

write_fourth_species_xml(Cantus, Measures, Position, Filename, Layout, Title) :-
    memberchk(Position, [above,below]), memberchk(Layout, [single,piano,guitar]),
    fourth_score_attacks(Cantus, Measures, _), xml_escape(Title, SafeTitle),
    prepare_layout_notes(Layout, Cantus, WrittenCantus),
    prepare_layout_notes(Layout, Measures, WrittenMeasures),
    setup_call_cleanup(open(Filename, write, Stream, [encoding(utf8)]),
        once(write_fourth_score(Stream, WrittenCantus, WrittenMeasures, Position, Layout, SafeTitle)),
        close(Stream)).

write_fourth_score(Stream, Cantus, Measures, Position, Layout, Title) :-
    xml_header(Stream),
    format(Stream, '<score-partwise version="3.1">~n', []),
    format(Stream, '  <work><work-title>~s</work-title></work>~n', [Title]),
    layout_part_name(Layout, PartName),
    format(Stream, '  <part-list><score-part id="P1"><part-name>~w</part-name></score-part></part-list>~n', [PartName]),
    format(Stream, '  <part id="P1">~n', []),
    write_fourth_measures(Stream, Cantus, Measures, Position, Layout, 1),
    format(Stream, '  </part>~n</score-partwise>~n', []).

write_fourth_measures(_, [], [], _, _, _).
write_fourth_measures(Stream, [C|Cs], [Measure|Ms], Position, Layout, Number) :-
    format(Stream, '    <measure number="~d">~n', [Number]),
    ( Number =:= 1 -> write_second_attributes(Stream, Layout) ; true ),
    ( Layout = piano -> LowerStaff = 2 ; LowerStaff = 1 ),
    ( Position = above ->
        write_fourth_voice(Stream, Measure, Ms, 1, 1),
        format(Stream, '      <backup><duration>4</duration></backup>~n', []),
        write_rhythmic_voice([C], whole, 2, LowerStaff, Stream)
    ; write_rhythmic_voice([C], whole, 1, 1, Stream),
      format(Stream, '      <backup><duration>4</duration></backup>~n', []),
      write_fourth_voice(Stream, Measure, Ms, 2, LowerStaff)
    ),
    format(Stream, '    </measure>~n', []),
    NextNumber is Number+1,
    write_fourth_measures(Stream, Cs, Ms, Position, Layout, NextNumber).

write_fourth_voice(Stream, [rest,Weak], Remaining, Voice, Staff) :-
    format(Stream, '      <note><rest/><duration>2</duration><voice>~d</voice><type>half</type><staff>~d</staff></note>~n', [Voice,Staff]),
    fourth_next_tie(Remaining, Tie),
    write_fourth_note(Stream, Weak, half, Tie, Voice, Staff).
write_fourth_voice(Stream, [tie(Held),Weak], Remaining, Voice, Staff) :-
    write_fourth_note(Stream, Held, half, stop, Voice, Staff),
    fourth_next_tie(Remaining, Tie),
    write_fourth_note(Stream, Weak, half, Tie, Voice, Staff).
write_fourth_voice(Stream, [Strong,Weak], _, Voice, Staff) :-
    valid_xml_note(Strong), valid_xml_note(Weak),
    write_fourth_note(Stream, Strong, half, none, Voice, Staff),
    write_fourth_note(Stream, Weak, half, none, Voice, Staff).
write_fourth_voice(Stream, [Last], _, Voice, Staff) :-
    write_fourth_note(Stream, Last, whole, none, Voice, Staff).

fourth_next_tie([[tie(_),_]|_], start) :- !.
fourth_next_tie(_, none).

write_fourth_note(Stream, [Name,Acc,Oct], Type, Tie, Voice, Staff) :-
    ( Type = whole -> Duration = 4 ; Type = half -> Duration = 2 ; Duration = 1 ), upcase_atom(Name, Step),
    format(Stream, '      <note><pitch><step>~w</step>', [Step]),
    ( Acc =:= 0 -> true ; format(Stream, '<alter>~d</alter>', [Acc]) ),
    format(Stream, '<octave>~d</octave></pitch><duration>~d</duration>', [Oct,Duration]),
    ( Tie = none -> true ; format(Stream, '<tie type="~w"/>', [Tie]) ),
    format(Stream, '<voice>~d</voice><type>~w</type><staff>~d</staff>', [Voice,Type,Staff]),
    ( Tie = none -> true ; format(Stream, '<notations><tied type="~w"/></notations>', [Tie]) ),
    format(Stream, '</note>~n', []).

% Validate the supported florid form and return actual attacks (not rests or
% tied continuations). All measures have four quarter-note duration units.
fifth_score_attacks(Cantus, Measures, Attacks) :-
    is_list(Cantus), length(Cantus, N), N >= 3,
    maplist(valid_xml_note, Cantus), is_list(Measures), same_length(Cantus, Measures),
    Measures = [[rest(half),note(First,half,start)]|Rest], valid_xml_note(First),
    fifth_score_tail(Rest, First, AttacksTail), Attacks = [First|AttacksTail].

fifth_score_tail([[note(Final,whole,none)]], _, [Final]) :- valid_xml_note(Final).
fifth_score_tail([Measure|Rest], Previous, Attacks) :-
    Rest = [_|_], ( Rest = [[note(_,whole,none)]] -> WeakTie = none ; WeakTie = start ),
    ( Measure = [note(Held,half,stop), note(Weak,half,WeakTie)],
      Held == Previous, valid_xml_note(Weak), Attacks = [Weak|Tail]
    ; Measure = [note(Held,quarter,stop), note(Extra,quarter,none), note(Weak,half,WeakTie)],
      Held == Previous, valid_xml_note(Extra), valid_xml_note(Weak), Attacks = [Extra,Weak|Tail]
    ),
    fifth_score_tail(Rest, Weak, Tail).

write_fifth_species_xml(Cantus, Measures, Position, Filename, Layout) :-
    write_fifth_species_xml(Cantus, Measures, Position, Filename, Layout,
                            "Fifth-species counterpoint").

write_fifth_species_xml(Cantus, Measures, Position, Filename, Layout, Title) :-
    memberchk(Position, [above,below]), memberchk(Layout, [single,piano,guitar]),
    fifth_score_attacks(Cantus, Measures, _), xml_escape(Title, SafeTitle),
    prepare_layout_notes(Layout, Cantus, WrittenCantus),
    prepare_layout_notes(Layout, Measures, WrittenMeasures),
    setup_call_cleanup(open(Filename, write, Stream, [encoding(utf8)]),
        once(write_fifth_score(Stream, WrittenCantus, WrittenMeasures, Position, Layout, SafeTitle)),
        close(Stream)).

write_fifth_score(Stream, Cantus, Measures, Position, Layout, Title) :-
    xml_header(Stream),
    format(Stream, '<score-partwise version="3.1">~n', []),
    format(Stream, '  <work><work-title>~s</work-title></work>~n', [Title]),
    layout_part_name(Layout, PartName),
    format(Stream, '  <part-list><score-part id="P1"><part-name>~w</part-name></score-part></part-list>~n', [PartName]),
    format(Stream, '  <part id="P1">~n', []),
    write_fifth_measures(Stream, Cantus, Measures, Position, Layout, 1),
    format(Stream, '  </part>~n</score-partwise>~n', []).

write_fifth_measures(_, [], [], _, _, _).
write_fifth_measures(Stream, [C|Cs], [Measure|Ms], Position, Layout, Number) :-
    format(Stream, '    <measure number="~d">~n', [Number]),
    ( Number =:= 1 -> write_second_attributes(Stream, Layout) ; true ),
    ( Layout = piano -> LowerStaff = 2 ; LowerStaff = 1 ),
    ( Position = above ->
        write_fifth_voice(Stream, Measure, 1, 1),
        format(Stream, '      <backup><duration>4</duration></backup>~n', []),
        write_rhythmic_voice([C], whole, 2, LowerStaff, Stream)
    ; write_rhythmic_voice([C], whole, 1, 1, Stream),
      format(Stream, '      <backup><duration>4</duration></backup>~n', []),
      write_fifth_voice(Stream, Measure, 2, LowerStaff)
    ),
    format(Stream, '    </measure>~n', []),
    NextNumber is Number+1,
    write_fifth_measures(Stream, Cs, Ms, Position, Layout, NextNumber).

write_fifth_voice(_, [], _, _).
write_fifth_voice(Stream, [rest(half)|Events], Voice, Staff) :-
    format(Stream, '      <note><rest/><duration>2</duration><voice>~d</voice><type>half</type><staff>~d</staff></note>~n', [Voice,Staff]),
    write_fifth_voice(Stream, Events, Voice, Staff).
write_fifth_voice(Stream, [note(Pitch,Type,Tie)|Events], Voice, Staff) :-
    write_fourth_note(Stream, Pitch, Type, Tie, Voice, Staff),
    write_fifth_voice(Stream, Events, Voice, Staff).

% Write several solutions for the same cantus as sequential, labeled sections
% in one score. Species is first, second, third, fourth, or fifth.
write_solutions_musicxml(Cantus, Species, Position, Solutions, Filename, Layout) :-
    write_solutions_musicxml(Cantus, Species, Position, Solutions, Filename, Layout,
                             "Counterpoint solutions").

write_solutions_musicxml(Cantus, Species, Position, Solutions, Filename, Layout, Title) :-
    memberchk(Species, [first,second,third,fourth,fifth]),
    memberchk(Position, [above,below]), memberchk(Layout, [single,piano,guitar]),
    is_list(Cantus), Cantus = [_|_], maplist(valid_xml_note, Cantus),
    is_list(Solutions), Solutions = [_|_],
    text_to_string(Title, TitleString), atom_string(TitleAtom, TitleString),
    solution_sections(Solutions, Cantus, Species, Position, Layout, 1, 1, RawMeasures),
    remove_redundant_score_attributes(RawMeasures, none, Measures, _),
    layout_part_name(Layout, PartName),
    DOM = [element('score-partwise', [version='3.1'],
            [element(work, [], [element('work-title', [], [TitleAtom])]),
             element('part-list', [], [element('score-part', [id='P1'],
                 [element('part-name', [], [PartName])])]),
             element(part, [id='P1'], Measures)])],
    setup_call_cleanup(open(Filename, write, Stream, [encoding(utf8)]),
                       xml_write(Stream, DOM, [layout(true)]), close(Stream)).

solution_sections([], _, _, _, _, _, _, []).
solution_sections([Solution|Rest], Cantus, Species, Position, Layout,
                  SectionNumber, MeasureNumber, All) :-
    solution_measures(Cantus, Species, Position, Solution, Layout, Raw),
    number_section(Raw, SectionNumber, MeasureNumber, NextMeasure, Numbered),
    NextSection is SectionNumber + 1,
    solution_sections(Rest, Cantus, Species, Position, Layout,
                      NextSection, NextMeasure, Remaining),
    append(Numbered, Remaining, All).

% Reuse each single-solution serializer so rhythm and tie semantics stay in
% sync. Build and validate every section before opening the destination.
solution_measures(Cantus, Species, Position, Solution, Layout, Measures) :-
    valid_solution_shape(Species, Cantus, Solution),
    prepare_layout_notes(Layout, Cantus, WrittenCantus),
    prepare_layout_notes(Layout, Solution, WrittenSolution),
    with_output_to(string(XML),
        (current_output(Stream), once(solution_score(Stream, WrittenCantus, Species, Position, WrittenSolution, Layout)))),
    setup_call_cleanup(open_string(XML, Input),
        load_structure(Input, [element('score-partwise',_,Children)], [dialect(xml),space(remove)]),
        close(Input)),
    memberchk(element(part,_,Measures), Children), Measures = [_|_].

valid_solution_shape(first, Cantus, Solution) :-
    is_list(Solution), same_length(Cantus, Solution), maplist(valid_xml_note, Solution).
valid_solution_shape(second, Cantus, Solution) :-
    is_list(Solution), length(Cantus,N), length(Solution,M), M =:= 2*N-1,
    maplist(valid_xml_note, Solution).
valid_solution_shape(third, Cantus, Solution) :-
    is_list(Solution), length(Cantus,N), length(Solution,M), M =:= 4*N-3,
    maplist(valid_xml_note, Solution).
valid_solution_shape(fourth, Cantus, Solution) :- fourth_score_attacks(Cantus, Solution, _).
valid_solution_shape(fifth, Cantus, Solution) :- fifth_score_attacks(Cantus, Solution, _).

solution_score(Stream, Cantus, first, Position, Solution, Layout) :-
    ( Position = above -> Upper = Solution, Lower = Cantus
    ; Upper = Cantus, Lower = Solution ),
    write_counterpoint_score(Stream, Upper, Lower, Layout, "").
solution_score(Stream, Cantus, second, Position, Solution, Layout) :-
    write_second_score(Stream, Cantus, Solution, Position, Layout, "").
solution_score(Stream, Cantus, third, Position, Solution, Layout) :-
    write_third_score(Stream, Cantus, Solution, Position, Layout, "").
solution_score(Stream, Cantus, fourth, Position, Solution, Layout) :-
    write_fourth_score(Stream, Cantus, Solution, Position, Layout, "").
solution_score(Stream, Cantus, fifth, Position, Solution, Layout) :-
    write_fifth_score(Stream, Cantus, Solution, Position, Layout, "").

number_section(Measures, Section, First, Next, Numbered) :-
    format(atom(Label), 'Solution ~d', [Section]),
    number_measures(Measures, First, Label, Next, Numbered).

number_measures([element(measure,_,Content)|Rest], Number, Label, Next,
                [element(measure,[number=NumberAtom],NewContent)|Numbered]) :-
    atom_number(NumberAtom, Number),
    ( Label = none -> Labeled0 = Content ; add_solution_label(Content, Label, Labeled0) ),
    add_system_break(Number, Labeled0, Labeled),
    ( Rest = [] -> append(Labeled, [element(barline,[location=right],
                                  [element('bar-style',[],['light-light'])])], NewContent)
    ; NewContent = Labeled ),
    Following is Number+1,
    ( Rest = [] -> Next = Following, Numbered = []
    ; number_measures(Rest, Following, none, Next, Numbered) ).

% Request a new system after every eight measures in the combined score.
add_system_break(Number, Content, [element(print,['new-system'=yes],[])|Content]) :-
    Number > 1, 1 is Number mod 8, !.
add_system_break(_, Content, Content).

% Attributes precede the direction in MusicXML measure order.
add_solution_label([element(attributes,A,C)|Rest], Label,
                   [element(attributes,A,C),Direction|Rest]) :- !,
    solution_direction(Label, Direction).
add_solution_label(Content, Label, [Direction|Content]) :-
    solution_direction(Label, Direction).

solution_direction(Label, element(direction,[],
    [element('direction-type',[],[element(words,[],[Label])])])).

% A standalone section repeats initial divisions, time and clefs. Remove an
% attributes element only when it exactly matches the most recent one; keep
% any later change so that meaningful notation changes remain intact.
remove_redundant_score_attributes([], Previous, [], Previous).
remove_redundant_score_attributes([element(measure,A,Content)|Rest], Previous,
                                  [element(measure,A,Clean)|Measures], Final) :-
    remove_duplicate_attributes(Content, Previous, Clean, Current),
    remove_redundant_score_attributes(Rest, Current, Measures, Final).

remove_duplicate_attributes([], Previous, [], Previous).
remove_duplicate_attributes([element(attributes,A,Children)|Rest], Previous, Clean, Final) :- !,
    Attribute = element(attributes,A,Children),
    ( Attribute == Previous -> Clean = Tail
    ; Clean = [Attribute|Tail] ),
    remove_duplicate_attributes(Rest, Attribute, Tail, Final).
remove_duplicate_attributes([Item|Rest], Previous, [Item|Clean], Final) :-
    remove_duplicate_attributes(Rest, Previous, Clean, Final).

% Guitar output sounds one octave below the internal notes. Encode that
% sounding pitch directly, with an octave-below clef and no second transpose;
% MuseScore then displays it at the original written pitch.
layout_part_name(guitar, 'Guitar') :- !.
layout_part_name(_, 'Piano').

write_single_clef(Stream, guitar) :- !,
    format(Stream, '<clef><sign>G</sign><line>2</line><clef-octave-change>-1</clef-octave-change></clef>', []).
write_single_clef(Stream, single) :-
    format(Stream, '<clef><sign>G</sign><line>2</line></clef>', []).

prepare_layout_notes(guitar, Input, Written) :- !, guitar_sounding_term(Input, Written).
prepare_layout_notes(_, Notes, Notes).

guitar_sounding_term([Name,Acc,Oct], [Name,Acc,SoundingOct]) :-
    atom(Name), integer(Acc), integer(Oct), !,
    Oct > 0, SoundingOct is Oct-1.
guitar_sounding_term(tie(Note), tie(Sounding)) :- !, guitar_sounding_term(Note, Sounding).
guitar_sounding_term(note(Note,Duration,Tie), note(Sounding,Duration,Tie)) :- !,
    guitar_sounding_term(Note, Sounding).
guitar_sounding_term(rest(Duration), rest(Duration)) :- !.
guitar_sounding_term(rest, rest) :- !.
guitar_sounding_term([], []) :- !.
guitar_sounding_term([Head|Tail], [SoundingHead|SoundingTail]) :-
    guitar_sounding_term(Head, SoundingHead), guitar_sounding_term(Tail, SoundingTail).

% Validate complete measures and pair each tie with the immediately following
% continuation before either output writer opens a file.
valid_diminution_measures(Measures) :-
    is_list(Measures), Measures = [_|_],
    maplist(valid_diminution_measure, Measures),
    append(Measures, Events), valid_diminution_ties(Events).
valid_diminution_measure(Measure) :-
    is_list(Measure), Measure = [_|_],
    maplist(diminution_event_units, Measure, Units), sum_list(Units, 8).
diminution_event_units(note(Note,Duration,Tie), Units) :-
    valid_xml_note(Note), memberchk(Tie, [none,start,stop]),
    diminution_duration_units(Duration, Units).
diminution_duration_units(whole, 8).
diminution_duration_units(half, 4).
diminution_duration_units(quarter, 2).
diminution_duration_units(eighth, 1).
valid_diminution_ties([]).
valid_diminution_ties([note(_,_,none)|Rest]) :- valid_diminution_ties(Rest).
valid_diminution_ties([note(N,_,start),note(N2,_,stop)|Rest]) :-
    N == N2, valid_diminution_ties(Rest).

write_diminution_xml(Measures, Filename, Layout) :-
    write_diminution_xml(Measures, Filename, Layout, 'Diminution').
write_diminution_xml(Measures, Filename, Layout, Title) :-
    memberchk(Layout, [single,guitar]), once(valid_diminution_measures(Measures)),
    xml_escape(Title, SafeTitle), prepare_layout_notes(Layout, Measures, Written),
    setup_call_cleanup(open(Filename, write, Stream, [encoding(utf8)]),
        (xml_header(Stream),
         format(Stream, '<score-partwise version="3.1"><work><work-title>~s</work-title></work>~n', [SafeTitle]),
         layout_part_name(Layout, Name),
         format(Stream, '<part-list><score-part id="P1"><part-name>~w</part-name></score-part></part-list><part id="P1">~n', [Name]),
         write_diminution_measures(Stream, Written, Layout, 1),
         format(Stream, '</part></score-partwise>~n', []), !), close(Stream)).
write_diminution_measures(_, [], _, _).
write_diminution_measures(Stream, [Measure|Rest], Layout, Number) :-
    format(Stream, '<measure number="~d">~n', [Number]),
    ( Number > 1, 1 is Number mod 8 -> format(Stream, '<print new-system="yes"/>~n', []) ; true ),
    ( Number =:= 1 ->
        format(Stream, '<attributes><divisions>2</divisions><time><beats>4</beats><beat-type>4</beat-type></time>', []),
        write_single_clef(Stream, Layout), format(Stream, '</attributes>~n', [])
    ; true ),
    maplist(write_diminution_note(Stream), Measure),
    format(Stream, '</measure>~n', []), Next is Number+1,
    write_diminution_measures(Stream, Rest, Layout, Next).
write_diminution_note(Stream, note([Name,Acc,Oct],Type,Tie)) :-
    diminution_duration_units(Type, Duration), upcase_atom(Name, Step),
    format(Stream, '<note><pitch><step>~w</step>', [Step]),
    ( Acc =:= 0 -> true ; format(Stream, '<alter>~d</alter>', [Acc]) ),
    format(Stream, '<octave>~d</octave></pitch><duration>~d</duration>', [Oct,Duration]),
    ( Tie = none -> true ; format(Stream, '<tie type="~w"/>', [Tie]) ),
    format(Stream, '<voice>1</voice><type>~w</type>', [Type]),
    ( Tie = none -> true ; format(Stream, '<notations><tied type="~w"/></notations>', [Tie]) ),
    format(Stream, '</note>~n', []).

% Combine independently validated diminution variations in one score.
% Each solution retains the notation of the standalone writer.
write_diminution_solutions_xml(Solutions, Filename, Layout) :-
    write_diminution_solutions_xml(Solutions, Filename, Layout, 'Diminution solutions').
write_diminution_solutions_xml(Solutions, Filename, Layout, Title) :-
    memberchk(Layout, [single,guitar]), is_list(Solutions), Solutions = [_|_],
    text_to_string(Title, TitleString), atom_string(TitleAtom, TitleString),
    diminution_solution_sections(Solutions, Layout, 1, 1, RawMeasures),
    remove_redundant_score_attributes(RawMeasures, none, Measures, _),
    layout_part_name(Layout, PartName),
    DOM = [element('score-partwise', [version='3.1'],
            [element(work, [], [element('work-title', [], [TitleAtom])]),
             element('part-list', [], [element('score-part', [id='P1'],
                 [element('part-name', [], [PartName])])]),
             element(part, [id='P1'], Measures)])],
    setup_call_cleanup(open(Filename, write, Stream, [encoding(utf8)]),
                       once(xml_write(Stream, DOM, [layout(true)])), close(Stream)).

diminution_solution_sections([], _, _, _, []).
diminution_solution_sections([Solution|Rest], Layout, Section, Number, All) :-
    once(valid_diminution_measures(Solution)),
    prepare_layout_notes(Layout, Solution, Written),
    with_output_to(string(XML),
        (current_output(Stream), format(Stream, '<part>', []),
         once(write_diminution_measures(Stream, Written, Layout, 1)),
         format(Stream, '</part>', []))),
    setup_call_cleanup(open_string(XML, Input),
        load_structure(Input, [element(part,_,Raw)], [dialect(xml),space(remove)]),
        close(Input)),
    maplist(remove_section_breaks, Raw, Unbroken),
    number_section(Unbroken, Section, Number, NextNumber, Numbered),
    NextSection is Section+1,
    diminution_solution_sections(Rest, Layout, NextSection, NextNumber, Remaining),
    append(Numbered, Remaining, All).

% The standalone writer's system breaks use local measure numbers; the shared
% section numbering inserts breaks using the combined score's measure numbers.
remove_section_breaks(element(measure,A,Content), element(measure,A,Clean)) :-
    exclude(is_section_system_break, Content, Clean).
is_section_system_break(element(print,Attributes,_)) :-
    memberchk('new-system'=yes, Attributes).
