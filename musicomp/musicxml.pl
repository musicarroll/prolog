:- module(musicxml, 
    [   write_musicxml/4, 
        write_musicxml/2,
        gen_filename/4
    ]).

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
    open(Filename, write, Stream, [encoding(utf8)]),
    xml_header(Stream),
    write_score(Phrases, Title, Subtitle, Stream),
    close(Stream).

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
    format(Stream, '    <work-title>~s</work-title>~n', [Title]),
    format(Stream, '  </work>~n', []),
    format(Stream, '  <movement-title>~s</movement-title>~n', [Subtitle]),
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
