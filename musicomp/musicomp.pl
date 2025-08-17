:- use_module(library(solution_sequences)).
:- use_module(musicxml).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Duration mapping: maps a symbolic duration to ticks.
%
note_duration_value(quarter, 480).
note_duration_value(half,    960).
note_duration_value(whole,   1920).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modified note_events/3 predicate.
%
% note_events(+Duration, +Note, -Events)
%
% For a given note, this predicate produces:
%   - A note-on event (delta time 0) and
%   - A note-off event after a delay equal to the given Duration (in ticks).
%
note_events(Duration, Note, Events) :-
    note_to_midi(Note, Midi),
    encode_vlq(0, DeltaOn),       % note-on immediately
    OnEvent = [144, Midi, 64],      % 144 = 0x90 (note on, channel 0)
    note_duration_value(Duration, Ticks),
    encode_vlq(Ticks, DeltaOff),  % note-off after Ticks
    OffEvent = [128, Midi, 64],     % 128 = 0x80 (note off, channel 0)
    append(DeltaOn, OnEvent, Temp1),
    append(Temp1, DeltaOff, Temp2),
    append(Temp2, OffEvent, Events).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper predicate for maplist: passes Duration to note_events/3.
%
note_events_with_duration(Duration, Note, Events) :-
    note_events(Duration, Note, Events).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert an entire phrase (list of note triples) into a flat list
% of MIDI event bytes using a given Duration for each note.
%
phrase_to_midi_events(Duration, Phrase, Events) :-
    maplist(note_events_with_duration(Duration), Phrase, ListOfEventLists),
    flatten(ListOfEventLists, FlatEvents),
    EndEvent = [0, 255, 47, 0],
    append(FlatEvents, EndEvent, Events).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Build a complete MIDI file (a list of bytes) for a given phrase.
%
% midi_file(+Duration, +Phrase, -MidiBytes)
%
% Builds a minimal SMF Type 0 MIDI file. The header remains fixed,
% and the track data is built from the events generated with the
% specified note Duration.
%
midi_file(Duration, Phrase, MidiBytes) :-
    % Build track data using the chosen Duration
    phrase_to_midi_events(Duration, Phrase, TrackData),
    length(TrackData, TrackLength),
    track_length_bytes(TrackLength, TrackLengthBytes),
    % MIDI Header chunk:
    Header = [77,84,104,100,    % "MThd"
              0,0,0,6,          % header length = 6 bytes
              0,0,              % format 0
              0,1,              % one track
              1,224],           % division = 480 ticks per quarter note (0x01,0xE0)
    % Track chunk header ("MTrk")
    TrackHeader = [77,84,114,107], % "MTrk"
    append(TrackHeader, TrackLengthBytes, Temp),
    append(Temp, TrackData, TrackChunk),
    append(Header, TrackChunk, MidiBytes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a phrase as a MIDI file with a specified note Duration.
%
% write_midi(+Phrase, +Filename, +Duration)
%
% Generates a MIDI file from the given Phrase using the specified Duration
% (one of quarter, half, or whole) for every note and writes it as a binary file.
%
write_midi(Phrase, Filename, Duration) :-
    midi_file(Duration, Phrase, MidiBytes),
    open(Filename, write, Stream, [type(binary)]),
    maplist(put_byte(Stream), MidiBytes),
    close(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Default version: whole notes are used when no Duration is specified.
%
write_midi(Phrase, Filename) :-
    write_midi(Phrase, Filename, whole).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert a note triple to a MIDI note number.
%
% We assume our internal note_value/2 predicate computes a value such that:
%    note_value([c,0,4]) = 48.
% To map this to standard MIDI (with middle C = 60) we add 12.
%
note_to_midi(Note, Midi) :-
    canonical_note(Note, Canon),
    note_value(Canon, Value),
    Midi is Value + 12.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Encode a delta-time (an integer) in MIDI’s variable-length format.
%
% We define explicit clauses for the tick values we expect:
%
% 0     -> [0]
% 480   -> [131,96]    % 480 decimal = 0x1E0; VLQ: 0x83 0x60
% 960   -> [135,64]    % 960 = 7*128 + 64, so VLQ: 0x87 (7+128) then 0x40 (64)
% 1920  -> [143,0]     % 1920 = 15*128 + 0, so VLQ: 0x8F (15+128) then 0x00
%
encode_vlq(0, [0]).
encode_vlq(480, [131,96]).
encode_vlq(960, [135,64]).
encode_vlq(1920, [143,0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert an integer into 4 bytes (big-endian) for the track chunk length.
%
track_length_bytes(Length, [B1,B2,B3,B4]) :-
    B1 is (Length >> 24) /\ 0xFF,
    B2 is (Length >> 16) /\ 0xFF,
    B3 is (Length >> 8)  /\ 0xFF,
    B4 is Length         /\ 0xFF.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert a phrase (list of note triples) into MIDI event bytes,
% but do NOT append the End-of-Track event.
%
phrase_to_midi_events_no_eot(Duration, Phrase, Events) :-
    maplist(note_events_with_duration(Duration), Phrase, ListOfEventLists),
    flatten(ListOfEventLists, Events).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert a list of phrases into a single flat list of MIDI event bytes.
%
% For each phrase, we obtain its event list (without its own end-of-track),
% then concatenate them all, and finally append a single End-of-Track event.
%
phrases_to_midi_events(Duration, Phrases, Events) :-
    maplist(phrase_to_midi_events_no_eot(Duration), Phrases, ListOfEventLists),
    flatten(ListOfEventLists, FlatEvents),
    % Optionally, you could insert a rest (delay) between phrases here.
    EndEvent = [0, 255, 47, 0],  % Standard End-of-Track
    append(FlatEvents, EndEvent, Events).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Build a complete MIDI file (a list of bytes) for a list of phrases.
%
% This predicate is analogous to your midi_file/3, but for a list of phrases.
%
midi_file_phrases(Duration, Phrases, MidiBytes) :-
    phrases_to_midi_events(Duration, Phrases, TrackData),
    length(TrackData, TrackLength),
    track_length_bytes(TrackLength, TrackLengthBytes),
    % MIDI Header chunk (14 bytes):
    Header = [77,84,104,100,    % "MThd"
              0,0,0,6,          % header length = 6 bytes
              0,0,              % format 0
              0,1,              % one track
              1,224],           % division = 480 ticks per quarter note (0x01,0xE0)
    % Track chunk header ("MTrk")
    TrackHeader = [77,84,114,107],  % "MTrk"
    append(TrackHeader, TrackLengthBytes, Temp),
    append(Temp, TrackData, TrackChunk),
    append(Header, TrackChunk, MidiBytes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a list of phrases as a single MIDI file using a given note Duration.
%
% write_midi_phrases(+Phrases, +Filename, +Duration)
%
write_midi_phrases(Phrases, Filename, Duration) :-
    midi_file_phrases(Duration, Phrases, MidiBytes),
    open(Filename, write, Stream, [type(binary)]),
    maplist(put_byte(Stream), MidiBytes),
    close(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Default version: if no Duration is specified, use whole notes.
%
write_midi_phrases(Phrases, Filename) :-
    write_midi_phrases(Phrases, Filename, whole).


% Allowed note names (using lowercase atoms)
note_name(c).
note_name(d).
note_name(e).
note_name(f).
note_name(g).
note_name(a).
note_name(b).

% Allowed accidentals:
% -1 represents flat, 0 represents natural, and 1 represents sharp.
accidental(-1).
accidental(0).
accidental(1).

% Allowed octave numbers (for a piano, the octave numbers run from 0 to 8)
octave(0).
octave(1).
octave(2).
octave(3).
octave(4).
octave(5).
octave(6).
octave(7).
octave(8).

% A note is represented as a triple [Name, Accidental, Octave].
% The predicate is_note/1 checks that:
%  - Name is one of the allowed note names,
%  - Accidental is one of -1, 0, or 1,
%  - Octave is an allowed octave,
%  - and that the note falls within the actual piano range.
is_note([Name, Acc, Oct]) :-
    note_name(Name),
    accidental(Acc),
    octave(Oct),
    valid_piano_note(Name, Oct).

% valid_piano_note/2 makes sure the note is actually on a piano:
%
% In octave 0, only A and B exist (A0 and B0).
valid_piano_note(Name, 0) :-
    member(Name, [a, b]).
% In octave 8, only C exists (C8).
valid_piano_note(c, 8).
% In octaves 1 through 7, all note names are allowed.
valid_piano_note(_, Oct) :-
    Oct >= 1,
    Oct =< 7.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% New predicates for interval calculation

% Map natural note names to their semitone offsets within an octave.
natural_value(c, 0).
natural_value(d, 2).
natural_value(e, 4).
natural_value(f, 5).
natural_value(g, 7).
natural_value(a, 9).
natural_value(b, 11).

% Compute the semitone value of a note.
% note_value([Name, Acc, Oct], Value) computes:
% Value = (Natural semitone offset) + (Accidental) + (12 * Octave)
note_value([Name, Acc, Oct], Value) :-
    natural_value(Name, Base),
    Value is Base + Acc + 12 * Oct.

% interval/3 determines the absolute number of semitones between two notes.
interval(Note1, Note2, Interval) :-
    note_value(Note1, Value1),
    note_value(Note2, Value2),
    Interval is abs(Value1 - Value2).

% interval_name(Semitones, Name)
interval_name(0, 'prime').
interval_name(1, 'minor second').
interval_name(2, 'major second').
interval_name(3, 'minor third').
interval_name(4, 'major third').
interval_name(5, 'perfect fourth').
interval_name(6, 'tritone').
interval_name(7, 'perfect fifth').
interval_name(8, 'minor sixth').
interval_name(9, 'major sixth').
interval_name(10, 'minor seventh').
interval_name(11, 'major seventh').
interval_name(12, 'octave').

% Compound intervals (an octave added: 12 + interval)
interval_name(13, 'minor ninth').     % octave (12) + minor second (1)
interval_name(14, 'major ninth').     % octave (12) + major second (2)
interval_name(15, 'minor tenth').     % octave (12) + minor third (3)
interval_name(16, 'major tenth').     % octave (12) + major third (4)
interval_name(17, 'eleventh').        % octave (12) + perfect fourth (5)
interval_name(18, 'augmented eleventh'). % octave (12) + tritone (6)
interval_name(19, 'perfect twelfth'). % octave (12) + perfect fifth (7)
interval_name(20, 'minor thirteenth').% octave (12) + minor sixth (8)
interval_name(21, 'major thirteenth').% octave (12) + major sixth (9)
interval_name(22, 'augmented thirteenth'). % octave (12) + minor seventh (10) augmented


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Scale definitions

% For our purposes the diatonic (major or natural minor) scale is defined by its set of intervals (in semitones from the root).
scale_degrees(major, [0,2,4,5,7,9,11]).
scale_degrees(minor, [0,2,3,5,7,8,10]).

% in_scale(+Note, +Root, +ScaleType)
% Succeeds if the note’s pitch (mod 12) relative to the Root
% is one of the allowed degrees for ScaleType.
in_scale(Note, Root, ScaleType) :-
    note_value(Note, Value),
    note_value(Root, RValue),
    Diff is (Value - RValue) mod 12,
    scale_degrees(ScaleType, Degrees),
    member(Diff, Degrees).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Candidate notes for phrase generation
% We limit candidates to those notes that are valid, in the scale,
% and lie within a “practical” range relative to the Root.

candidate_note(Root, ScaleType, Note) :-
    is_note(Note),
    in_scale(Note, Root, ScaleType),
    note_value(Root, RV),
    note_value(Note, NV),
    NV >= RV - 12,  % allow up to one octave below
    NV =< RV + 16.  % and up to a tenth (about 16 semitones) above

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Canonical note mapping
%
% Maps a note to its canonical representation.
%
% [c,-1,Oct]  (C flat)       -> [b,0,Oct-1]
% [e,1,Oct]   (E sharp)       -> [f,0,Oct]
% [f,-1,Oct]  (F flat)        -> [e,0,Oct]
% [b,1,Oct]   (B sharp)       -> [c,0,Oct+1]
%
canonical_note([c, -1, Oct], [b, 0, Oct1]) :-
    Oct1 is Oct - 1, !.
canonical_note([e, 1, Oct], [f, 0, Oct]) :- !.
canonical_note([f, -1, Oct], [e, 0, Oct]) :- !.
canonical_note([b, 1, Oct], [c, 0, Oct1]) :-
    Oct1 is Oct + 1, !.
canonical_note(Note, Note).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Updated phrase_notes/3 to apply canonical mapping
%
% phrase_notes(+Notes, +Root, +ScaleType)
%
% Generates a list of candidate notes (for the middle of the phrase)
% by choosing notes that are valid, in the given scale, and within a
% limited range. Before a note is added to the phrase, it is mapped to
% its canonical form.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
phrase_notes([], _, _).
phrase_notes([N|Ns], Root, ScaleType) :-
    candidate_note(Root, ScaleType, Temp),
    canonical_note(Temp, N),
    phrase_notes(Ns, Root, ScaleType).

% phrase_length(-Phrase)
% Nondeterministically chooses a phrase (list) length between 8 and 12.
phrase_length(Phrase) :-
    between(8, 12, _),  % used only for backtracking on length
    length(Phrase, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Checks on the complete phrase

% range_within(+Phrase)
% The difference between the highest and lowest note is at most 16 semitones.
range_within(Phrase) :-
    findall(V, (member(Note, Phrase), note_value(Note, V)), Vs),
    min_list(Vs, Min),
    max_list(Vs, Max),
    Range is Max - Min,
    Range =< 16.

% last_preceded_by_second(+Phrase)
% The penultimate note must be only a second (1 or 2 semitones) away from the last.
last_preceded_by_second(Phrase) :-
    append(_, [Penultimate, Last], Phrase),
    interval(Penultimate, Last, I),
    member(I, [1,2]).

% single_highest(+Phrase)
% There is exactly one note whose value equals the maximum value.
single_highest(Phrase) :-
    findall(V, (member(Note, Phrase), note_value(Note, V)), Vs),
    max_list(Vs, Max),
    aggregate_all(count, (member(V, Vs), V =:= Max), Count),
    Count =:= 1.

% sign(+Number, -Sign)
% Succeeds with Sign = 1 if Number > 0, -1 if Number < 0, 0 if Number = 0.
sign(X, 1) :- X > 0.
sign(X, -1) :- X < 0.
sign(0, 0).

% check_leaps(+Phrase)
% Whenever an interval larger than a third (> 4 semitones) occurs,
% the following interval must be a second (1 or 2 semitones) in the opposite direction.
check_leaps([_, _]).  % fewer than three notes: nothing to check.
check_leaps([A, B, C | Rest]) :-
    note_value(A, V1),
    note_value(B, V2),
    note_value(C, V3),
    Interval1 is abs(V2 - V1),
    ( Interval1 > 4 ->
         Interval2 is abs(V3 - V2),
         member(Interval2, [1,2]),
         sign(V2 - V1, Dir1),
         sign(V3 - V2, Dir2),
         Dir1 =\= Dir2
       ; true
    ),
    check_leaps([B, C | Rest]).

% check_sixths(+Phrase)
%
% For any two consecutive notes A and B, if the interval between them
% (in semitones, computed as V(B)-V(A)) is such that |Diff| is either 8 or 9,
% then the only acceptable case is Diff = 8 (an upward minor sixth).
% Any downward sixth (Diff = -8 or -9) or an upward major sixth (Diff = 9)
% is disallowed.
%
check_sixths([]).
check_sixths([_]).
check_sixths([A,B|Rest]) :-
    note_value(A, V1),
    note_value(B, V2),
    Diff is V2 - V1,
    ( (abs(Diff) =:= 8 ; abs(Diff) =:= 9) ->
          Diff =:= 8
      ; true
    ),
    check_sixths([B|Rest]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Putting it all together: isometric_phrase/3
%
% isometric_phrase(+ScaleType, +Root, -Phrase)
%
% Generates a phrase (a list of 8–12 notes) such that:
%   - Every note is valid and belongs to the given ScaleType with Root.
%   - The phrase begins and ends with Root.
%   - The overall range (lowest to highest) is ≤ a 10th (16 semitones).
%   - The last note is reached by a step (second) from its predecessor.
%   - There is only one highest note.
%   - Any leap of more than a third is immediately followed by a step in the opposite direction.
%   - Any upward sixth is minor (8 semitones), never major (9 semitones).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Updated no_consecutive_repeats/1 using canonical forms.
%
% no_consecutive_repeats(+Phrase)
%
% Succeeds if no two successive notes (after canonical mapping) are equal.
%
no_consecutive_repeats([]).
no_consecutive_repeats([_]).
no_consecutive_repeats([Note1, Note2 | Rest]) :-
    canonical_note(Note1, Canon1),
    canonical_note(Note2, Canon2),
    Canon1 \= Canon2,
    no_consecutive_repeats([Note2 | Rest]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% No consecutive sevenths
%
% no_sevenths(+Phrase)
%
% Succeeds if for every pair of consecutive notes in the phrase,
% the canonical forms of the notes do not form an interval of a seventh.
% (A 7th is either 10 semitones (minor seventh) or 11 semitones (major seventh).)
%
no_sevenths([]).
no_sevenths([_]).
no_sevenths([Note1, Note2 | Rest]) :-
    canonical_note(Note1, C1),
    canonical_note(Note2, C2),
    interval(C1, C2, I),
    \+ member(I, [10, 11]),
    no_sevenths([Note2 | Rest]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% No interval larger than an octave
%
% no_interval_exceeds_octave(+Phrase)
%
% Succeeds if for every pair of consecutive notes (after applying
% canonical_note/2), the interval between them is no more than 12 semitones.
%
no_interval_exceeds_octave([]).
no_interval_exceeds_octave([_]).
no_interval_exceeds_octave([Note1, Note2 | Rest]) :-
    canonical_note(Note1, Canon1),
    canonical_note(Note2, Canon2),
    interval(Canon1, Canon2, I),
    I =< 12,
    no_interval_exceeds_octave([Note2 | Rest]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_tritones(+Phrase)
%
% Succeeds if no two consecutive notes (after canonical mapping)
% form an interval of 6 semitones (a tritone). If any such pair is found,
% the predicate fails.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_tritones([]).
check_tritones([_]).
check_tritones([A, B | Rest]) :-
    canonical_note(A, CA),
    canonical_note(B, CB),
    interval(CA, CB, I),
    I =\= 6,
    check_tritones([B|Rest]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_consecutive_thirds(+Phrase)
%
% Fails if any three consecutive notes produce two successive intervals
% that are both thirds (either 3 or 4 semitones).
%
check_consecutive_thirds([]).
check_consecutive_thirds([_]).
check_consecutive_thirds([_, _]).  % fewer than three notes: nothing to check.
check_consecutive_thirds([A, B, C | Rest]) :-
    canonical_note(A, CA),
    canonical_note(B, CB),
    canonical_note(C, CC),
    interval(CA, CB, I1),
    interval(CB, CC, I2),
    ( (member(I1, [3,4]), member(I2, [3,4]))
      -> fail  % If both intervals are thirds, reject.
      ;  true ),
    check_consecutive_thirds([B, C | Rest]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_repeated_two_note_patterns(+Phrase)
%
% Fails if two immediately successive two-note patterns (non-overlapping) are identical.
% In other words, if for four consecutive notes [A, B, C, D],
% the canonical versions of the two-note pattern [A, B] equal those of [C, D],
% then the phrase is rejected.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_repeated_two_note_patterns([]).
check_repeated_two_note_patterns([_,_,_]).  % fewer than 4 notes: nothing to check.
check_repeated_two_note_patterns([A, B, C, D | Rest]) :-
    canonical_note(A, CA),
    canonical_note(B, CB),
    canonical_note(C, CC),
    canonical_note(D, CD),
    ( (CA == CC, CB == CD)
      -> fail  % The two-note pattern [A,B] is repeated immediately.
      ;  true ),
    check_repeated_two_note_patterns([B, C, D | Rest]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_leading_tone_rule(+Phrase, +Root)
%
% For the given Root note, compute its pitch class.
% Then, for every occurrence in the Phrase of a note whose canonical pitch class
% equals (Root’s pitch class – 1) mod 12 (i.e. the leading tone),
% ensure that the interval from the previous note to that note is a step (1 or 2 semitones).
% If not, then the only acceptable case is if the three–note sequence (Prev, LT, Next)
% is the exception: an upward sequence whose canonical forms are [g,0,_], [b,0,_], [c,0,_].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_leading_tone_rule(Phrase, Root) :-
    note_value(Root, RootVal),
    RootPC is RootVal mod 12,
    % The leading tone’s pitch class is one less than Root’s (modulo 12).
    LeadingPC is (RootPC + 11) mod 12,
    check_leading_tone_rule_aux(Phrase, LeadingPC).

% Auxiliary predicate that scans the phrase three notes at a time.
check_leading_tone_rule_aux([], _).
check_leading_tone_rule_aux([_], _).
check_leading_tone_rule_aux([_X, _Y], _).
check_leading_tone_rule_aux([Prev, LT, Next | Rest], LeadingPC) :-
    canonical_note(LT, CLT),
    note_value(CLT, LTVal),
    ( (LTVal mod 12) =:= LeadingPC ->
         % LT is a leading tone. Compute the interval from Prev to LT.
         canonical_note(Prev, CPrev),
         note_value(CPrev, PrevVal),
         Diff is LTVal - PrevVal,
         ( (abs(Diff) =:= 1 ; abs(Diff) =:= 2)
           -> true  % Reached by a step: OK.
           ;  % Otherwise, allow only if the triple is the exception.
              canonical_note(Prev, CP),
              canonical_note(LT, CB),
              canonical_note(Next, CC),
              % Check that CP, CB, and CC (ignoring octave) are g, b, and c, respectively.
              CP = [g, 0, _],
              CB = [b, 0, _],
              CC = [c, 0, _],
              note_value(CP, PV),
              note_value(CB, BV),
              note_value(CC, CV),
              PV < BV, BV < CV
         )
      ; true
    ),
    check_leading_tone_rule_aux([LT, Next | Rest], LeadingPC).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Updated isometric_phrase/3 with the new interval rules added.
%
% isometric_phrase(+ScaleType, +Root, -Phrase)
%
% Generates a phrase (a list of 8–12 notes) that satisfies all constraints:
%   - Valid notes in the specified scale with the given Root.
%   - Begins and ends with Root.
%   - Overall range ≤ a 10th (16 semitones).
%   - The last note is reached by a step (second) from its predecessor.
%   - Exactly one highest note.
%   - Any leap of more than a third is immediately followed by a step in the opposite direction.
%   - Any upward sixth is minor (8 semitones), never major (9 semitones).
%   - No two successive notes are equivalent (when compared canonically).
%   - No two consecutive notes form a seventh (10 or 11 semitones apart).
%   - No consecutive notes form an interval larger than an octave.
%   - No two consecutive notes form a tritone (6 semitones).
%   - No two successive interval skips are both thirds.
%   - No immediately repeated two-note patterns.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isometric_phrase(ScaleType, Root, Phrase) :-
    % Choose a phrase length between 8 and 12.
    between(8, 12, L),
    length(Phrase, L),
    % The phrase must start and end on the Root.
    Phrase = [Root | Rest],
    append(Middle0, [Root], Rest),
    % Generate the intermediate notes from candidates in the scale (with canonical mapping).
    phrase_notes(Middle0, Root, ScaleType),
    % Check the overall constraints.
    range_within(Phrase),
    last_preceded_by_second(Phrase),
    single_highest(Phrase),
    check_leaps(Phrase),
    check_sixths(Phrase),
    no_consecutive_repeats(Phrase),
    no_sevenths(Phrase),
    no_interval_exceeds_octave(Phrase),
    check_tritones(Phrase),
    check_consecutive_thirds(Phrase),
    check_repeated_two_note_patterns(Phrase),
    check_leading_tone_rule(Phrase, Root).

% (Optionally, if you use a global variable for storing generated phrases, you
% might have a directive like the following at the top of your file.)
:- nb_setval(generated_phrases, []).

% isometric_phrase(+ScaleType, +Root, -Phrase, -History)
%
% Generates a phrase (using isometric_phrase/3) and then checks that the
% generated phrase is not already present in the global history. If it is new,
% it adds it to the history. The fourth argument, History, returns the updated list.
%
isometric_phrase(ScaleType, Root, Phrase, History) :-
    % Generate a candidate phrase (using your existing predicate)
    isometric_phrase(ScaleType, Root, Phrase),
    % Retrieve the global history list (the phrases generated so far)
    nb_getval(generated_phrases, PrevHistory),
    % Check that the candidate is not already in the list.
    ( member(Phrase, PrevHistory)
      -> fail  % If it is already in the list, reject it.
      ;  true  % Otherwise, continue.
    ),
    % Add the new phrase to the history.
    NewHistory = [Phrase | PrevHistory],
    nb_setval(generated_phrases, NewHistory),
    History = NewHistory.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% isometric_phrase_constrained(+ScaleType, +Root, +Second, +SecondToLast, -Phrase)
%
% Generates an isometric phrase with the following structure:
%   [Root, Second | Middle, SecondToLast, Root]
%
% where:
%   - The phrase has a length between 8 and 12.
%   - Second and SecondToLast are fixed.
%   - The free internal notes (Middle) are generated from candidate notes (in the scale).
%   - The overall phrase must satisfy the standard constraints (range, leaps, sixths,
%     no consecutive repeats, no consecutive sevenths, and no interval exceeding an octave).
%
isometric_phrase_constrained(ScaleType, Root, Second, SecondToLast, Phrase) :-
    % Ensure the given fixed notes are valid and in the scale.
    is_note(Root), is_note(Second), is_note(SecondToLast),
    in_scale(Second, Root, ScaleType),
    in_scale(SecondToLast, Root, ScaleType),
    % Choose a total phrase length between 8 and 12.
    between(8, 12, L),
    % We already have 4 fixed positions: first (Root), second (Second),
    % penultimate (SecondToLast), and last (Root). The remaining (free) positions
    % are the internal notes that we generate.
    FreeLen is L - 4,
    length(Middle, FreeLen),
    % Generate free internal notes (apply candidate_note and canonical mapping).
    phrase_notes(Middle, Root, ScaleType),
    % Construct the full phrase.
    append([Root, Second], Middle, Temp1),
    append(Temp1, [SecondToLast, Root], Phrase),
    % Now enforce all the overall constraints.
    range_within(Phrase),
    last_preceded_by_second(Phrase),
    single_highest(Phrase),
    check_leaps(Phrase),
    check_sixths(Phrase),
    no_consecutive_repeats(Phrase),
    no_sevenths(Phrase),
    no_interval_exceeds_octave(Phrase),
    check_tritones(Phrase),
    check_consecutive_thirds(Phrase),
    check_repeated_two_note_patterns(Phrase),
    check_leading_tone_rule(Phrase, Root).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% isometric_phrase_constrained(+ScaleType, +Root, +Second, +SecondToLast, -Phrase, -History)
%
% Generates a constrained phrase with the structure:
%   [Root, Second | Middle, SecondToLast, Root]
%
% where:
%   - The phrase has a length between 8 and 12.
%   - Second and SecondToLast are fixed.
%   - The internal notes (Middle) are generated from candidate notes (in the scale).
%   - The overall phrase satisfies all the standard constraints.
%
% This version also uses a global history (generated_phrases) to ensure that a
% phrase is only returned once. If the generated phrase is already in the history,
% the predicate fails (forcing backtracking).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isometric_phrase_constrained(ScaleType, Root, Second, SecondToLast, Phrase, History) :-
    % Generate a candidate phrase using the constrained predicate.
    isometric_phrase_constrained(ScaleType, Root, Second, SecondToLast, Phrase),
    % Retrieve the global history list (the phrases generated so far).
    nb_getval(generated_phrases, PrevHistory),
    % Check that the candidate is not already in the history.
    ( member(Phrase, PrevHistory)
      -> fail  % If already present, reject this candidate.
      ;  true  % Otherwise, proceed.
    ),
    % Add the new phrase to the history.
    NewHistory = [Phrase | PrevHistory],
    nb_setval(generated_phrases, NewHistory),
    History = NewHistory.

gen_phrases(midi,Num):-  findnsols(Num, Phrase, isometric_phrase_constrained(major, [c,0,4], [d,0,4], [b,0,3], Phrase), Phrases),  
                    write_midi_phrases(Phrases, 'combined_output.mid').

gen_phrases(xml,Num):-  findnsols(Num, Phrase, isometric_phrase_constrained(major, [c,0,4], [d,0,4], [b,0,3], Phrase), Phrases),  
                        write_musicxml(Phrases, 'combined_output.xml').


gen_phrases(xml,Num,History):-  findnsols(Num, Phrase,  
    isometric_phrase_constrained(major, [c,0,4], [d,0,4], [b,0,3], Phrase, History), Phrases),  
                        write_musicxml(Phrases, 'combined_output.xml').

gen_phrases(xml,Num,Root,Second,SecondToLast,History):-  
    findnsols(Num, Phrase,  
        isometric_phrase_constrained(major, Root, Second, SecondToLast, Phrase, History), Phrases),  
    write_musicxml(Phrases, 'combined_output.xml').

    gen_phrases(xml,Num,Root,Second,SecondToLast,History, Title, SubTitle):-  
        gen_filename('',Title,'.xml',Filename),
        findnsols(Num, Phrase,  
            isometric_phrase_constrained(major, Root, Second, SecondToLast, Phrase, History), Phrases),  
        write_musicxml(Phrases, Filename, Title, SubTitle).
    