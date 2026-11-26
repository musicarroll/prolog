% Variations of an isometric melody. A measure is a list of
% note([Name,Accidental,Octave], Duration, Tie) events, with Duration whole,
% half or quarter and Tie none, start or stop.
%
% diminution(+Scale, +Root, +Phrase, +Mode, -Measures)
% Mode is halves, quarters, syncopated, or mixed. Each original pitch sounds
% on its corresponding downbeat; the final pitch occupies a whole measure.
diminution(Scale, Root, Phrase, Mode, Measures) :-
    memberchk(Mode, [halves,quarters,syncopated,mixed]),
    scale_degrees(Scale, _),
    is_note(Root), is_list(Phrase), Phrase = [_,_|_],
    maplist(is_note, Phrase), maplist(diminution_in_scale(Root,Scale), Phrase),
    maplist(note_value, Phrase, Values), min_list(Values, Min), max_list(Values, Max),
    ( Mode = syncopated ->
        diminution_syncopated(Phrase, Root, Scale, Min, Max, Measures)
    ; ( Mode = mixed,
        diminution_mixed_sync(Phrase, Root, Scale, Min, Max, Measures)
      ; diminution_measures(Phrase, Root, Scale, Mode, Min, Max, none, [], patterns(none,[]), Measures, Kinds),
        ( Mode = mixed -> memberchk(halves,Kinds), memberchk(quarters,Kinds),
          include(=(half_eighths),Kinds,EighthBars), length(EighthBars,Count), Count =< 1
        ; true ) ),
      diminution_attacks(Measures, Attacks), mostly_steps(Attacks),
      diminution_no_triad(Attacks), diminution_bar_recovery(Measures),
      check_repeated_two_note_patterns(Attacks),
      diminution_different_quarter_patterns(Measures),
      diminution_unique_quarter_measures(Measures)
    ),
    diminution_attacks(Measures, SoundingAttacks),
    last_preceded_by_second(SoundingAttacks).

diminution_in_scale(Root, Scale, Note) :- in_scale(Note, Root, Scale).

% Pitches stay inside the source melody's range, protecting its high/low profile.
diminution_measures([Last], _, _, _, _, _, none, History, _,
                    [[note(Last,whole,none)]], []) :-
    append(History,[Last],Notes), check_repeated_two_note_patterns(Notes).
diminution_measures([Anchor,Next|Rest], Root, Scale, Mode, Min, Max, Pending,
                    History, patterns(PreviousPattern,Seen), [Measure|Measures], [Kind|Kinds]) :-
    diminution_kind(Mode, Kind),
    diminution_durations(Kind, Durations), length(Durations, Total), Count is Total-1,
    diminution_inner(Count, Anchor, Root, Scale, Kind, Min, Max, Pending, 1, Inner),
    last(Inner, Last), note_value(Last, LV), note_value(Next, NV),
    Delta is NV-LV,
    ( Rest = [] -> memberchk(Delta, [-2,-1,1,2]) ; true ),
    ( abs(Delta) > 2 -> Cross = leap(Delta) ; Cross = none ),
    Notes = [Anchor|Inner],
    ( Kind = quarters -> diminution_no_triad(Notes) ; true ),
    append(History,Notes,Window), check_repeated_two_note_patterns(Window),
    diminution_recent_notes(Window,Recent),
    maplist(diminution_event, Durations, Notes, Measure),
    diminution_measure_pattern(Measure, Pattern),
    ( Pattern = none -> NextSeen = Seen
    ; Pattern \== PreviousPattern, maplist(note_value, Notes, PitchValues),
      \+ memberchk(PitchValues, Seen), NextSeen = [PitchValues|Seen] ),
    diminution_measures([Next|Rest], Root, Scale, Mode, Min, Max, Cross,
                        Recent, patterns(Pattern,NextSeen), Measures, Kinds).

% Keep the last three attacks to detect a two-note repeat over a bar line.
diminution_recent_notes(Notes, Recent) :-
    reverse(Notes, Reversed), diminution_take_three(Reversed, Last),
    reverse(Last, Recent).
diminution_take_three([A,B,C|_], [A,B,C]) :- !.
diminution_take_three(Notes, Notes).

diminution_measure_pattern(Measure, Pattern) :-
    ( Measure = [note(A,quarter,none),note(B,quarter,none),
                 note(C,quarter,none),note(D,quarter,none)] ->
        quarter_measure_pattern([A,B,C,D],Pattern)
    ; Pattern = none ).

diminution_different_quarter_patterns([]).
diminution_different_quarter_patterns([_]).
diminution_different_quarter_patterns([First,Second|Rest]) :-
    diminution_measure_pattern(First, P1), diminution_measure_pattern(Second, P2),
    ( P1 = none ; P2 = none ; P1 \== P2 ), !,
    diminution_different_quarter_patterns([Second|Rest]).

% Avoid returning to an identical four-quarter figure later in the line.
diminution_unique_quarter_measures(Measures) :-
    findall(Values,
        (member(M,Measures), diminution_measure_pattern(M,P), P \== none,
         findall(V,(member(note(N,_,_),M),note_value(N,V)),Values)), All),
    sort(All,Unique), same_length(All,Unique).

diminution_kind(halves, halves).
diminution_kind(quarters, quarters).
diminution_kind(mixed, halves).
diminution_kind(mixed, quarters).
diminution_kind(mixed, half_quarters).
diminution_kind(mixed, half_eighths).
diminution_durations(halves, [half,half]).
diminution_durations(quarters, [quarter,quarter,quarter,quarter]).
diminution_durations(half_quarters, [half,quarter,quarter]).
diminution_durations(half_eighths, [half,quarter,eighth,eighth]).
diminution_event(Duration, Note, note(Note,Duration,none)).

% Avoid two successive thirds in the same direction, which outline a triad.
diminution_no_triad([A,B,C|Rest]) :- !,
    note_value(A, X), note_value(B, Y), note_value(C, Z),
    D1 is Y-X, D2 is Z-Y,
    \+ (memberchk(D1,[3,4,-3,-4]), memberchk(D2,[3,4,-3,-4]), D1*D2 > 0),
    diminution_no_triad([B,C|Rest]).
diminution_no_triad(_).

diminution_inner(0, _, _, _, _, _, _, _, _, []) :- !.
diminution_inner(Count, Prev, Root, Scale, Kind, Min, Max, Pending, Index, [N|Ns]) :-
    diminution_next_note(Prev, Root, Scale, Min, Max, N),
    note_value(Prev, PV), note_value(N, V), Diff is V-PV,
    % From the first quarter note of a bar, do not leap upward.
    ( Kind = quarters, Index =:= 1, Diff > 0 -> Diff =< 2 ; true ),
    ( Index =:= 1, Pending = leap(Leap) -> abs(Diff) =< 2, Leap*Diff < 0 ; true ),
    Remaining is Count-1, NextIndex is Index+1,
    diminution_inner(Remaining, N, Root, Scale, Kind, Min, Max, none, NextIndex, Ns).

diminution_next_note(Previous, Root, Scale, Min, Max, Note) :-
    note_value(Previous, PV),
    member(Offset, [1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,7,-7,8,-8,9,-9,10,-10,11,-11,12,-12]),
    Value is PV+Offset, Value >= Min, Value =< Max, Value >= 9, Value =< 96,
    Oct is Value div 12, PC is Value mod 12,
    canonical_pitch_class(PC, Name, Acc), Note = [Name,Acc,Oct],
    in_scale(Note, Root, Scale).

% Syncopations alternate freely with ordinary half-note motion. A syncopated
% anchor is anticipated in the preceding measure and tied to its downbeat.
diminution_syncopated(Phrase, Root, Scale, Min, Max, Measures) :-
    diminution_sync_bars(Phrase, Root, Scale, Min, Max, none, Measures),
    member(M, Measures), member(note(_,half,start), M),
    ( Phrase = [_,_,_|_] ->
        member([_,note(_,half,none)], Measures)
    ; true ).

diminution_sync_bars([Last], _, _, _, _, Tie, [[note(Last,whole,Tie)]]).
diminution_sync_bars([A,B|Rest], Root, Scale, Min, Max, Tie,
                     [[note(A,half,Tie),note(Weak,half,WeakTie)]|Measures]) :-
    ( Weak = B, WeakTie = start, NextTie = stop
    ; diminution_next_note(A, Root, Scale, Min, Max, Weak),
      WeakTie = none, NextTie = none ),
    diminution_sync_bars([B|Rest], Root, Scale, Min, Max, NextTie, Measures).

% In mixed mode, a syncopated line may also replace an untied weak half
% with two quarters, preserving the original anchor and any incoming tie.
diminution_mixed_sync(Phrase, Root, Scale, Min, Max, Measures) :-
    diminution_syncopated(Phrase, Root, Scale, Min, Max, HalfBars),
    diminution_mix_sync_bars(HalfBars, Root, Scale, Min, Max, Measures, Changed),
    memberchk(yes, Changed).
diminution_mix_sync_bars([], _, _, _, _, [], []).
diminution_mix_sync_bars([Bar|Bars], Root, Scale, Min, Max, [New|Rest], [Change|Changes]) :-
    ( Bar = [note(A,half,Tie),note(B,half,none)],
      diminution_next_note(B,Root,Scale,Min,Max,C),
      New = [note(A,half,Tie),note(B,quarter,none),note(C,quarter,none)], Change = yes
    ; New = Bar, Change = no ),
    diminution_mix_sync_bars(Bars,Root,Scale,Min,Max,Rest,Changes).

% An attack which leaps over a bar line is resolved by a contrary step.
diminution_bar_recovery([]).
diminution_bar_recovery([_]).
diminution_bar_recovery([A,B|Rest]) :-
    last(A,note(Prev,_,_)), B = [note(Next,_,Tie)|Following],
    ( Tie = stop -> true
    ; note_value(Prev,X), note_value(Next,Y), Delta is Y-X,
      ( abs(Delta) =< 2 -> true
      ; Following = [note(Resolution,_,_)|_], note_value(Resolution,Z),
        Step is Z-Y, abs(Step) =< 2, Delta*Step < 0 ) ),
    diminution_bar_recovery([B|Rest]).

diminution_attacks(Measures, Notes) :-
    findall(N, (member(M,Measures),member(note(N,_,Tie),M),Tie \== stop), Notes).

% Render tied fragments as one MIDI attack; ticks are based on 480 per quarter.
write_diminution_midi(Measures, Filename) :-
    once(diminution_midi_bytes(Measures, Bytes)),
    setup_call_cleanup(open(Filename, write, Stream, [type(binary)]),
                       maplist(put_byte(Stream), Bytes), close(Stream)).
diminution_midi_bytes(Measures, Bytes) :-
    valid_diminution_measures(Measures), diminution_attacks(Measures, Notes),
    maplist(is_note, Notes), append(Measures, Events),
    diminution_midi_events(Events, EventBytes),
    append(EventBytes, [0,255,47,0], Data), length(Data, Length),
    track_length_bytes(Length, LengthBytes),
    append([[77,84,104,100,0,0,0,6,0,0,0,1,1,224],
            [77,84,114,107],LengthBytes,Data], Bytes).
diminution_midi_events([], []).
diminution_midi_events([note(N,D,start),note(N,D2,stop)|Rest], Bytes) :-
    diminution_ticks(D,T1), diminution_ticks(D2,T2), Ticks is T1+T2,
    diminution_midi_note(N, Ticks, Prefix), diminution_midi_events(Rest, Tail),
    append(Prefix, Tail, Bytes).
diminution_midi_events([note(N,D,none)|Rest], Bytes) :-
    diminution_ticks(D,Ticks), diminution_midi_note(N, Ticks, Prefix),
    diminution_midi_events(Rest, Tail), append(Prefix, Tail, Bytes).
diminution_ticks(eighth, 240).
diminution_ticks(quarter, 480).
diminution_ticks(half, 960).
diminution_ticks(whole, 1920).
diminution_midi_note(Note, Ticks, Bytes) :-
    note_to_midi(Note, Midi), diminution_vlq(Ticks, Delta),
    append([[0,144,Midi,64],Delta,[128,Midi,64]], Bytes).
diminution_vlq(N, Bytes) :-
    Low is N /\ 127, High is N >> 7,
    ( High =:= 0 -> Bytes = [Low]
    ; diminution_vlq(High, Prefix), append(Prefix, [Low], Raw),
      diminution_vlq_prefix(Raw, Bytes) ).
diminution_vlq_prefix([Last], [Last]).
diminution_vlq_prefix([H|T], [B|Rest]) :- B is H \/ 128, diminution_vlq_prefix(T, Rest).
