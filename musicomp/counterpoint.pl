:- use_module(library(random)).
:- use_module(library(solution_sequences)).

% First-species, note-against-note counterpoint. Load through musicomp.pl.
% first_species(+Scale, +Cantus, +Position, -Counterpoint)
% Scale is major or minor; Position is above or below. The cantus begins and
% ends on its tonic and approaches it by step. Notes use [name, accidental, octave].
first_species(Scale, Cantus, Counterpoint) :-
    first_species(Scale, Cantus, above, Counterpoint).

first_species(Scale, Cantus, Position, Counterpoint) :-
    sampled_species(first, Scale, Cantus, Position, Counterpoint).

first_species_base(Scale, Cantus, Position, Counterpoint) :-
    memberchk(Scale, [major, minor]),
    memberchk(Position, [above, below]),
    is_list(Cantus), length(Cantus, Length), Length >= 3,
    maplist(is_note, Cantus),
    Cantus = [Root|_], last(Cantus, Final),
    note_value(Root, RootValue), note_value(Final, RootValue),
    cantus_scale_notes(Cantus, Root, Scale),
    append(_, [Penultimate, Final], Cantus),
    interval(Penultimate, Final, LastStep), memberchk(LastStep, [1,2]),
    species_notes(Cantus, Root, Scale, Position, Counterpoint),
    mostly_steps(Counterpoint),
    range_within(Counterpoint),
    unique_extremes(Counterpoint).

cantus_scale_notes([N,Final], Root, Scale) :-
    species_scale_note(N, Root, Scale, penultimate),
    in_scale(Final, Root, Scale).
cantus_scale_notes([N,Next|Rest], Root, Scale) :-
    Rest = [_|_], in_scale(N, Root, Scale),
    cantus_scale_notes([Next|Rest], Root, Scale).

species_scale_note(N, Root, Scale, _) :- in_scale(N, Root, Scale).
species_scale_note(N, Root, minor, penultimate) :-
    note_value(N, V), note_value(Root, RV), V mod 12 =:= (RV + 11) mod 12.

mostly_steps(Notes) :-
    findall(D, (append(_, [A,B|_], Notes), interval(A, B, D)), Intervals),
    include(is_step, Intervals, Steps),
    length(Intervals, Count), length(Steps, StepCount), StepCount * 2 > Count.
is_step(D) :- D =< 2.


% Construct left to right, pruning violations as soon as they occur.
species_notes([C|Cs], Root, Scale, Position, [N|Ns]) :-
    species_candidate(C, Root, Scale, Position, start, N),
    species_tail(Cs, Root, Scale, Position, C, N, none, Ns).

species_tail([C], Root, Scale, Position, PrevC, PrevN, PrevLeap, [N]) :-
    species_candidate(C, Root, Scale, Position, end, N),
    species_transition(PrevC, C, PrevN, N, PrevLeap, _),
    second_cadence(PrevC, C, PrevN, N, Root).
species_tail([C,Next|Cs], Root, Scale, Position, PrevC, PrevN, PrevLeap, [N|Ns]) :-
    ( Cs = [] -> Place = penultimate ; Place = middle ),
    length([C,Next|Cs], Remaining),
    contour_candidate(Remaining, PrevN,
                      species_candidate(C, Root, Scale, Position, Place), N),
    species_transition(PrevC, C, PrevN, N, PrevLeap, Leap),
    species_tail([Next|Cs], Root, Scale, Position, C, N, Leap, Ns).

species_candidate(C, Root, Scale, Position, Place, N) :-
    cooperative_search_tick,
    note_value(C, CV),
    species_distance(Place, Position, Distance),
    ( Position = above -> V is CV + Distance ; V is CV - Distance ),
    V >= 9, V =< 96,
    Oct is V div 12, PC is V mod 12,
    canonical_pitch_class(PC, Name, Acc), N = [Name,Acc,Oct],
    species_scale_note(N, Root, Scale, Place).

species_distance(start, Position, D) :-
    memberchk(Position, [above,below]),
    ( nb_current(musicomp_forced_opening, Forced) -> D = Forced
    ; opening_order(Distances), member(D, Distances) ).
species_distance(end, above, D) :- member(D, [0,12,7]).
species_distance(end, below, D) :- member(D, [0,12]).
species_distance(middle, _, D) :- member(D, [3,4,7,8,9,12,15,16,19]).
species_distance(penultimate, _, D) :- member(D, [3,4,7,8,9,12,15,16,19]).

species_transition(C1, C2, N1, N2, PreviousLeap, Leap) :-
    note_value(C1, A), note_value(C2, B),
    note_value(N1, X), note_value(N2, Y),
    DC is B-A, DN is Y-X, Abs is abs(DN),
    memberchk(Abs, [1,2,3,4,5,7,8,12]),
    ( Abs =:= 8 -> DN > 0 ; true ),
    ( PreviousLeap = leap(Old) -> Abs =< 2, Old*DN < 0 ; true ),
    ( Abs > 4 -> Leap = leap(DN) ; Leap = none ),
    % Avoid crossing, overlap, and similar motion into a perfect interval.
    ( X >= A -> Y >= A, B =< X ; Y =< A, B >= X ),
    NewInterval is abs(Y-B) mod 12,
    ( memberchk(NewInterval, [0,7]) -> DC*DN =< 0 ; true ).

% Second species: two half notes against each cantus whole note, ending with
% one whole note. Counterpoint contains 2*N-1 pitches for N cantus notes.
second_species(Scale, Cantus, Counterpoint) :-
    second_species(Scale, Cantus, above, Counterpoint).

second_species(Scale, Cantus, Position, Counterpoint) :-
    sampled_species(second, Scale, Cantus, Position, Counterpoint).

second_species_base(Scale, Cantus, Position, Counterpoint) :-
    memberchk(Scale, [major, minor]), memberchk(Position, [above, below]),
    is_list(Cantus), length(Cantus, Length), Length >= 3,
    maplist(is_note, Cantus), Cantus = [Root|_], last(Cantus, Final),
    note_value(Root, RootValue), note_value(Final, RootValue),
    cantus_scale_notes(Cantus, Root, Scale),
    append(_, [Penultimate, Final], Cantus),
    interval(Penultimate, Final, Step), memberchk(Step, [1,2]),
    Cantus = [First|Rest],
    species_candidate(First, Root, Scale, Position, start, Down),
    second_species_measures(First, Rest, Root, Scale, Position, Down, none, Counterpoint),
    mostly_steps(Counterpoint), range_within(Counterpoint), unique_extremes(Counterpoint).

second_species_measures(C, [Next|Rest], Root, Scale, Position, Down, PreviousLeap,
                        [Down, Weak|Notes]) :-
    ( Rest = [] -> WeakPlace = penultimate, NextPlace = end
    ; WeakPlace = middle, NextPlace = middle ),
    length([C,Next|Rest], Remaining),
    ( Rest = [] -> second_weak_candidate(C, Root, Scale, Position, WeakPlace, Weak)
    ; contour_candidate(Remaining, Down,
                        second_weak_candidate(C, Root, Scale, Position, WeakPlace), Weak) ),
    species_transition(C, C, Down, Weak, PreviousLeap, WeakLeap),
    ( Rest = [] -> species_candidate(Next, Root, Scale, Position, NextPlace, NextDown)
    ; NextRemaining is Remaining-1,
      contour_candidate(NextRemaining, Weak,
                        species_candidate(Next, Root, Scale, Position, NextPlace), NextDown) ),
    species_transition(C, Next, Weak, NextDown, WeakLeap, NextLeap),
    second_passing_rule(C, Down, Weak, NextDown),
    second_downbeat_rule(C, Next, Down, NextDown),
    ( Rest = [] ->
        second_cadence(C, Next, Weak, NextDown, Root), Notes = [NextDown]
    ; second_species_measures(Next, Rest, Root, Scale, Position, NextDown, NextLeap, Notes)
    ).

% Weak beats can be consonant or dissonant; no unison or crossing inside.
second_weak_candidate(C, Root, Scale, Position, Place, N) :-
    note_value(C, CV), between(1, 19, Distance),
    ( Position = above -> V is CV + Distance ; V is CV - Distance ),
    V >= 9, V =< 96, Oct is V div 12, PC is V mod 12,
    canonical_pitch_class(PC, Name, Acc), N = [Name,Acc,Oct],
    species_scale_note(N, Root, Scale, Place).

second_passing_rule(C, Down, Weak, NextDown) :-
    interval(C, Weak, Vertical),
    ( memberchk(Vertical, [3,4,7,8,9,12,15,16,19]) -> true
    ; note_value(Down, D), note_value(Weak, W), note_value(NextDown, N),
      In is W-D, Out is N-W,
      AbsIn is abs(In), AbsOut is abs(Out),
      memberchk(AbsIn, [1,2]), memberchk(AbsOut, [1,2]), In*Out > 0
    ).

% Disallow consecutive perfect intervals and similar motion into a perfect
% fifth, octave, or unison between successive downbeats.
second_downbeat_rule(C1, C2, N1, N2) :-
    interval(C1, N1, I1), interval(C2, N2, I2),
    P1 is I1 mod 12, P2 is I2 mod 12,
    \+ (memberchk(P1, [0,7]), P1 =:= P2),
    note_value(C1, CV1), note_value(C2, CV2),
    note_value(N1, NV1), note_value(N2, NV2),
    CantusMotion is CV2-CV1, CounterMotion is NV2-NV1,
    \+ (memberchk(P2, [0,7]), CantusMotion*CounterMotion > 0).

second_cadence(C, Final, Weak, Last, Root) :-
    note_value(C, CV), note_value(Final, FV),
    note_value(Weak, WV), note_value(Last, LV), note_value(Root, RV),
    DC is FV-CV, DN is LV-WV,
    AbsDN is abs(DN), memberchk(AbsDN, [1,2]), DC*DN < 0,
    CClass is (CV-RV) mod 12, WClass is (WV-RV) mod 12,
    LastClass is (LV-RV) mod 12,
    memberchk([CClass,WClass,LastClass], [[2,11,0],[11,2,0],[2,5,7]]).

% Third species: four quarter notes per cantus whole note, followed by a
% whole-note tonic. The added voice contains 4*N-3 pitches for N cantus notes.
third_species(Scale, Cantus, Counterpoint) :-
    third_species(Scale, Cantus, above, Counterpoint).

% Every sampled solution gets a fresh opening and contour draw.
third_species(Scale, Cantus, Position, Counterpoint) :-
    sampled_species(third, Scale, Cantus, Position, Counterpoint).


third_species_base(Mode, Scale, Cantus, Position, Counterpoint) :-
    memberchk(Scale, [major, minor]), memberchk(Position, [above, below]),
    is_list(Cantus), length(Cantus, Length), Length >= 3,
    maplist(is_note, Cantus), Cantus = [Root|_], last(Cantus, Final),
    note_value(Root, RootValue), note_value(Final, RootValue),
    cantus_scale_notes(Cantus, Root, Scale),
    append(_, [Penultimate, Final], Cantus),
    interval(Penultimate, Final, Step), memberchk(Step, [1,2]),
    Cantus = [First|Rest],
    third_opening(First, Root, Scale, Position, Down),
    note_value(Down, OpeningValue),
    third_species_measures(Mode, First, Rest, Root, Scale, Position, Down, none, none,
                           bounds(OpeningValue,OpeningValue,none), Counterpoint),
    mostly_steps(Counterpoint), range_within(Counterpoint), unique_extremes(Counterpoint).

third_species_measures(Mode, C, [Next|Rest], Root, Scale, Position, Down, PreviousLeap,
                       PreviousPattern, Bounds0, [Down, Beat2, Beat3, Beat4|Notes]) :-
    length([C,Next|Rest], Remaining),
    contour_mode(Mode, Remaining, DirectionMode),
    third_direction_candidate(DirectionMode, C, Root, Scale, Position, middle, Down, Beat2),
    extend_counterpoint_bounds(Bounds0, Beat2, Bounds1),
    species_transition(C, C, Down, Beat2, PreviousLeap, Leap2),
    third_direction_candidate(DirectionMode, C, Root, Scale, Position, middle, Beat2, Beat3),
    extend_counterpoint_bounds(Bounds1, Beat3, Bounds2),
    species_transition(C, C, Beat2, Beat3, Leap2, Leap3),
    third_dissonance_rule(C, Down, Beat2, Beat3),
    ( Rest = [] -> Beat4Place = penultimate, NextPlace = end
    ; Beat4Place = middle, NextPlace = middle ),
    third_direction_candidate(DirectionMode, C, Root, Scale, Position, Beat4Place, Beat3, Beat4),
    extend_counterpoint_bounds(Bounds2, Beat4, Bounds3),
    species_transition(C, C, Beat3, Beat4, Leap3, Leap4),
    third_accent_rule(C, Beat2, Beat3, Beat4),
    no_repeated_quarter_motif(Down, Beat2, Beat3, Beat4),
    quarter_measure_pattern([Down,Beat2,Beat3,Beat4], Pattern),
    Pattern \== PreviousPattern,
    third_dissonance_rule(C, Beat2, Beat3, Beat4),
    third_target_bounds(Mode, Remaining, [Down,Beat2,Beat3,Beat4], Bounds3, TargetBounds),
    ( Rest = [] ->
        species_candidate(Next, Root, Scale, Position, NextPlace, NextDown)
    ; NextRemaining is Remaining-1,
      contour_mode(Mode, NextRemaining, NextDirectionMode),
      direction_candidate(NextDirectionMode, Beat4,
          species_candidate(Next, Root, Scale, Position, NextPlace), NextDown)
    ),
    extend_counterpoint_bounds(TargetBounds, NextDown, Bounds4),
    species_transition(C, Next, Beat4, NextDown, Leap4, NextLeap),
    third_dissonance_rule(C, Beat3, Beat4, NextDown),
    second_downbeat_rule(C, Next, Down, NextDown),
    ( Rest = [] ->
        second_cadence(C, Next, Beat4, NextDown, Root), Notes = [NextDown]
    ; third_species_measures(Mode, Next, Rest, Root, Scale, Position, NextDown, NextLeap, Pattern,
                             Bounds4, Notes)
    ).

% Beat 1 is already consonant by species_candidate/6. Beat 3 is consonant,
% unless beats 2 and 4 are consonant and beat 3 is approached by step.
% The separate dissonance rule also governs how that dissonance resolves.
third_accent_rule(Cantus, Beat2, Beat3, Beat4) :-
    ( third_consonant(Cantus, Beat3) -> true
    ; third_consonant(Cantus, Beat2),
      third_consonant(Cantus, Beat4),
      interval(Beat2, Beat3, Approach), memberchk(Approach, [1,2])
    ).

third_consonant(Cantus, Note) :-
    interval(Cantus, Note, Semitones),
    memberchk(Semitones, [0,3,4,7,8,9,12,15,16,19]).

% A dissonance must be approached and left by step: either passing in the
% same direction, or returning to its preceding consonant neighbor. Require
% its preceding note to be consonant, preventing consecutive dissonances.
third_dissonance_rule(C, Previous, Note, Next) :-
    interval(C, Note, Vertical),
    ( memberchk(Vertical, [3,4,7,8,9,12,15,16,19]) -> true
    ; interval(C, Previous, PreviousVertical),
      memberchk(PreviousVertical, [0,3,4,7,8,9,12,15,16,19]),
      note_value(Previous, P), note_value(Note, N), note_value(Next, Q),
      In is N-P, Out is Q-N, AbsIn is abs(In), AbsOut is abs(Out),
      memberchk(AbsIn, [1,2]), memberchk(AbsOut, [1,2]),
      ( In*Out > 0 ; P =:= Q )
    ).

% Try small melodic motions first, both to favor stepwise lines and avoid
% exploring large leaps before the common cases.
third_note_candidate(C, Root, Scale, Position, Place, Previous, N) :-
    member(Offset, [1,-1,2,-2,3,-3,4,-4,5,-5,7,-7,8,12,-12]),
    third_note_offset(C, Root, Scale, Position, Place, Previous, Offset, N).

% Filter offset direction before pitch, scale and range calculations.
third_direction_candidate(Mode, C, Root, Scale, Position, Place, Previous, N) :-
    direction_order(Mode, Directions), member(Direction, Directions),
    member(Offset, [1,-1,2,-2,3,-3,4,-4,5,-5,7,-7,8,12,-12]),
    ( Direction = up -> Offset > 0 ; Offset < 0 ),
    third_note_offset(C, Root, Scale, Position, Place, Previous, Offset, N).

third_note_offset(C, Root, Scale, Position, Place, Previous, Offset, N) :-
    cooperative_search_tick,
    note_value(Previous, PV), V is PV+Offset,
    V >= 9, V =< 96, note_value(C, CV),
    ( Position = above -> Distance is V-CV ; Distance is CV-V ),
    Distance >= 1, Distance =< 19,
    Oct is V div 12, PC is V mod 12,
    canonical_pitch_class(PC, Name, Acc), N = [Name,Acc,Oct],
    species_scale_note(N, Root, Scale, Place).

third_opening(C, Root, Scale, Position, N) :-
    ( (nb_current(musicomp_forced_opening, _)
      ; nb_current(musicomp_contour, contour(_,_,_,_,random))) ->
        species_candidate(C, Root, Scale, Position, start, N)
    ; ( Position = above -> member(Distance,[7,12,0])
      ; Position = below, member(Distance,[0,12,7]) ),
      note_value(C, CV),
      ( Position = above -> V is CV+Distance ; V is CV-Distance ),
      V >= 9, V =< 96, Oct is V div 12, PC is V mod 12,
      canonical_pitch_class(PC, Name, Acc), N = [Name,Acc,Oct],
      in_scale(N, Root, Scale)
    ).

% Highest and lowest sounding pitches must occur once, except that an
% extreme may occur at both endpoints if it occurs nowhere in between.
unique_extremes(Notes) :-
    Notes = [_|_],
    maplist(note_value, Notes, Values),
    min_list(Values, Min), max_list(Values, Max),
    unique_extreme(Values, Min), unique_extreme(Values, Max).

unique_extreme(Values, Extreme) :-
    include(=(Extreme), Values, Occurrences),
    ( Occurrences = [_] -> true
    ; Occurrences = [_,_], Values = [Extreme|_], last(Values, Extreme)
    ).

% Reject the same two sounding pitches on beats 1–2 and 3–4 of a measure.
no_repeated_quarter_motif(A, B, C, D) :-
    note_value(A, VA), note_value(B, VB),
    note_value(C, VC), note_value(D, VD),
    ( VA =\= VC ; VB =\= VD ), !.

% Fourth species uses measures [rest, Preparation], [tie(Held), Resolution]
% and a final [Tonic]. The penultimate measure may instead be [Strong, Weak]
% with no tie, using second-species motion.
fourth_species(Scale, Cantus, Counterpoint) :-
    fourth_species(Scale, Cantus, above, Counterpoint).

fourth_species(Scale, Cantus, Position, Measures) :-
    sampled_species(fourth, Scale, Cantus, Position, Measures).

fourth_species_base(Scale, Cantus, Position, Measures) :-
    memberchk(Scale, [major,minor]), memberchk(Position, [above,below]),
    is_list(Cantus), length(Cantus, Length), Length >= 3,
    maplist(is_note, Cantus), Cantus = [Root|_], last(Cantus, Final),
    note_value(Root, RootValue), note_value(Final, RootValue),
    cantus_scale_notes(Cantus, Root, Scale),
    append(_, [Penultimate, Final], Cantus),
    note_value(Penultimate, PV), CadenceDegree is (PV-RootValue) mod 12,
    memberchk(CadenceDegree, [2,11]),
    interval(Penultimate, Final, Step), memberchk(Step, [1,2]),
    Cantus = [First, Next|Rest],
    species_candidate(First, Root, Scale, Position, start, Preparation),
    fourth_measures(First, Next, Rest, Root, Scale, Position, Preparation,
                    none, none, Tail, Attacks),
    Measures = [[rest,Preparation]|Tail],
    AttackNotes = [Preparation|Attacks],
    mostly_steps(AttackNotes), range_within(AttackNotes), unique_extremes(AttackNotes).

% For each interior measure the previous weak-beat note is held on the
% downbeat. A dissonant held note must resolve downward by step.
fourth_measures(PreviousC, C, [Next|Rest], Root, Scale, Position, Held,
                PreviousStrong, PreviousLeap, [[tie(Held),Weak]|Measures], [Weak|Attacks]) :-
    ( PreviousStrong = none -> true
    ; second_downbeat_rule(PreviousC, C, PreviousStrong, Held) ),
    ( Rest = [] -> Place = penultimate ; Place = middle ),
    length([C,Next|Rest], Remaining),
    ( Rest = [] -> species_candidate(C, Root, Scale, Position, Place, Weak)
    ; contour_candidate(Remaining, Held,
                        species_candidate(C, Root, Scale, Position, Place), Weak) ),
    species_transition(C, C, Held, Weak, PreviousLeap, Leap),
    fourth_resolution(C, Held, Weak, Position),
    ( Rest = [] -> fourth_cadential_suspension(C, Held, Weak, Position) ; true ),
    fourth_measures(C, Next, Rest, Root, Scale, Position, Weak,
                    Held, Leap, Measures, Attacks).
% Optional break in syncopation for the penultimate measure: both notes are
% newly attacked, with consonant downbeat and the usual second-species cadence.
fourth_measures(PreviousC, C, [Final], Root, Scale, Position, PreviousWeak,
                PreviousStrong, PreviousLeap, [[Strong,Weak],[Last]], [Strong,Weak,Last]) :-
    species_candidate(C, Root, Scale, Position, middle, Strong),
    species_transition(PreviousC, C, PreviousWeak, Strong, PreviousLeap, StrongLeap),
    ( PreviousStrong = none -> true
    ; second_downbeat_rule(PreviousC, C, PreviousStrong, Strong) ),
    species_candidate(C, Root, Scale, Position, penultimate, Weak),
    species_transition(C, C, Strong, Weak, StrongLeap, WeakLeap),
    species_candidate(Final, Root, Scale, Position, end, Last),
    species_transition(C, Final, Weak, Last, WeakLeap, _),
    second_cadence(C, Final, Weak, Last, Root),
    second_downbeat_rule(C, Final, Strong, Last).
fourth_measures(PreviousC, Final, [], Root, Scale, Position, PreviousWeak,
                PreviousStrong, PreviousLeap, [[Last]], [Last]) :-
    species_candidate(Final, Root, Scale, Position, end, Last),
    species_transition(PreviousC, Final, PreviousWeak, Last, PreviousLeap, _),
    second_cadence(PreviousC, Final, PreviousWeak, Last, Root),
    second_downbeat_rule(PreviousC, Final, PreviousStrong, Last).

fourth_resolution(C, Held, Weak, Position) :-
    interval(C, Held, Dissonance), interval(C, Weak, Resolution),
    ( memberchk(Dissonance, [0,3,4,7,8,9,12,15,16,19]) -> true
    ; fourth_suspension(Position, Dissonance, Resolution),
      note_value(Held, H), note_value(Weak, W), Step is H-W,
      memberchk(Step, [1,2])
    ).

% Upper: 4-3, 7-6, 9-8 (and octave compounds). Lower: 2-3,
% 4-5, 9-10. The counterpoint voice resolves downward in each case.
fourth_suspension(above, D, R) :-
    D mod 12 =:= 5, RC is R mod 12, memberchk(RC, [3,4]).
fourth_suspension(above, D, R) :-
    DC is D mod 12, RC is R mod 12, memberchk(DC, [10,11]), memberchk(RC, [8,9]).
fourth_suspension(above, D, R) :-
    D >= 13, DC is D mod 12, memberchk(DC, [1,2]), R mod 12 =:= 0.
fourth_suspension(below, D, R) :-
    DC is D mod 12, RC is R mod 12, memberchk(DC, [1,2]), memberchk(RC, [3,4]).
fourth_suspension(below, D, R) :-
    D mod 12 =:= 5, R mod 12 =:= 7.

fourth_cadential_suspension(C, Held, Weak, above) :-
    interval(C, Held, D), interval(C, Weak, R),
    DC is D mod 12, RC is R mod 12, memberchk(DC, [10,11]), memberchk(RC, [8,9]).
fourth_cadential_suspension(C, Held, Weak, below) :-
    interval(C, Held, D), interval(C, Weak, R),
    DC is D mod 12, RC is R mod 12, memberchk(DC, [1,2]), memberchk(RC, [3,4]).

% Classify each movement by direction and size. Semitone and whole-tone
% steps match; minor and major thirds match. Larger leaps use semitones.
quarter_measure_pattern([A,B,C,D], [X,Y,Z]) :-
    melodic_motion(A, B, X), melodic_motion(B, C, Y), melodic_motion(C, D, Z).

melodic_motion(A, B, motion(Direction, Size)) :-
    note_value(A, AV), note_value(B, BV), Delta is BV-AV,
    ( Delta > 0 -> Direction = up ; Delta < 0 -> Direction = down ; Direction = same ),
    Distance is abs(Delta),
    ( Distance =:= 0 -> Size = unison
    ; Distance =< 2 -> Size = step
    ; Distance =< 4 -> Size = third
    ; Size = Distance ).

% Fifth species: a conservative florid profile built from a fourth-species
% skeleton, with one or more held half notes divided into two quarter notes.
% Events are rest(Duration) or note(Pitch, Duration, Tie), where Tie is
% none, start, or stop. The final measure contains one whole note.
fifth_species(Scale, Cantus, Counterpoint) :-
    fifth_species(Scale, Cantus, above, Counterpoint).

fifth_species(Scale, Cantus, Position, Measures) :-
    sampled_species(fifth, Scale, Cantus, Position, Measures).

fifth_species_base(Scale, Cantus, Position, Measures) :-
    fourth_species_base(Scale, Cantus, Position, Skeleton),
    Cantus = [Root|_],
    fifth_decorate(Cantus, Skeleton, Root, Scale, Position, Measures, 0, Count),
    Count > 0,
    fifth_attacks(Measures, Attacks),
    mostly_steps(Attacks), range_within(Attacks), unique_extremes(Attacks),
    no_consecutive_repeats(Attacks), no_sevenths(Attacks),
    no_interval_exceeds_octave(Attacks), check_tritones(Attacks),
    check_sixths(Attacks), check_leaps(Attacks).

fifth_decorate([_|Cs], [[rest,First]|Ms], Root, Scale, Position,
               [[rest(half),note(First,half,start)]|Events], Count0, Count) :-
    fifth_interior(Cs, Ms, Root, Scale, Position, Events, Count0, Count).

fifth_interior([_], [[Final]], _, _, _, [[note(Final,whole,none)]], Count, Count).
fifth_interior([C,Next|Cs], [[tie(Held),Weak]|Ms], Root, Scale, Position,
               [Measure|Events], Count0, Count) :-
    ( Ms = [[_]] -> WeakTie = none ; WeakTie = start ),
    length([C,Next|Cs], Remaining),
    ( contour_candidate(Remaining, Held,
                        third_note_candidate(C, Root, Scale, Position, middle, Held), Ornament),
      species_candidate(C, Root, Scale, Position, middle, Ornament),
      fourth_resolution(C, Held, Ornament, Position),
      species_transition(C, C, Held, Ornament, none, Leap),
      species_transition(C, C, Ornament, Weak, Leap, _),
      Measure = [note(Held,quarter,stop), note(Ornament,quarter,none), note(Weak,half,WeakTie)],
      Count1 is Count0+1
    ; Measure = [note(Held,half,stop), note(Weak,half,WeakTie)], Count1 = Count0
    ),
    fifth_interior([Next|Cs], Ms, Root, Scale, Position, Events, Count1, Count).

fifth_attacks(Measures, Attacks) :-
    findall(Note, (member(Measure, Measures), member(note(Note,_,Tie), Measure),
                   Tie \== stop), Attacks).

% Return all preferred solutions before any fallback solutions. A fallback is
% considered only after the finite preferred search is exhausted.
prefer_voice_extremes(Generator, Cantus, Species, Counterpoint) :-
    ( call(Generator, Counterpoint),
      counterpoint_extreme_relation(Cantus, Species, Counterpoint, separate)
    ; call(Generator, Counterpoint),
      counterpoint_extreme_relation(Cantus, Species, Counterpoint, shared_measure)
    ).

% The pitches of the two voices' extremes must differ. The preference also
% requires that no measure contains both voices' high points or both lows.
counterpoint_extreme_relation(Cantus, Species, Counterpoint, Relation) :-
    counterpoint_measures(Species, Counterpoint, Measures),
    same_length(Cantus, Measures),
    maplist(note_value, Cantus, CantusValues),
    maplist(measure_values, Measures, ValuesByMeasure),
    append(ValuesByMeasure, AllValues), AllValues = [_|_],
    min_list(CantusValues, CLow), max_list(CantusValues, CHigh),
    min_list(AllValues, PLow), max_list(AllValues, PHigh),
    CLow =\= PLow, CHigh =\= PHigh,
    extreme_measure_numbers(CantusValues, ValuesByMeasure, CHigh, PHigh, HighShared),
    extreme_measure_numbers(CantusValues, ValuesByMeasure, CLow, PLow, LowShared),
    ( HighShared = false, LowShared = false -> Relation = separate
    ; Relation = shared_measure ).

extreme_measure_numbers(CantusValues, Measures, CantusExtreme, CounterExtreme, Shared) :-
    ( nth1(Index, CantusValues, CantusExtreme), nth1(Index, Measures, Values),
      memberchk(CounterExtreme, Values) -> Shared = true
    ; Shared = false ).

counterpoint_measures(first, Notes, Measures) :- maplist(wrap_note, Notes, Measures).
counterpoint_measures(second, Notes, Measures) :- group_species_notes(2, Notes, Measures).
counterpoint_measures(third, Notes, Measures) :- group_species_notes(4, Notes, Measures).
counterpoint_measures(fourth, Written, Measures) :- maplist(fourth_measure_notes, Written, Measures).
counterpoint_measures(fifth, Written, Measures) :- maplist(fifth_measure_notes, Written, Measures).

wrap_note(Note, [Note]).
group_species_notes(_, [Final], [[Final]]).
group_species_notes(Size, Notes, [Measure|Rest]) :-
    length(Measure, Size), append(Measure, Remaining, Notes), Remaining = [_|_],
    group_species_notes(Size, Remaining, Rest).

fourth_measure_notes(Written, Notes) :-
    findall(Note, (member(Item, Written),
                   (Item = tie(Note) ; Item = [_,_,_], Note = Item)), Notes).
fifth_measure_notes(Written, Notes) :-
    findall(Note, member(note(Note,_,_), Written), Notes).
measure_values(Notes, Values) :- maplist(note_value, Notes, Values).

% Randomize direction, not validity: on backtracking the other direction is
% still explored, including when a chosen direction has no valid continuation.
direction_candidate(Mode, Previous, Generator, Note) :-
    direction_order(Mode, Directions),
    member(Direction, Directions),
    call(Generator, Note),
    note_value(Previous, PV), note_value(Note, NV),
    ( Direction = up -> NV > PV ; NV < PV ).

direction_order(Mode, Directions) :-
    ( Mode = random -> random_permutation([up,down], Directions)
    ; Mode = contour(Remaining) -> contour_directions(Remaining, Directions)
    ; Directions = [up,down] ).

% Track the extrema incrementally, avoiding a scan of the growing prefix.
extend_counterpoint_bounds(bounds(Min0,Max0,Target), Note, bounds(Min,Max,Target)) :-
    note_value(Note, Value),
    ( Target = extreme(up, Limit) -> Value =< Limit
    ; Target = extreme(down, Limit) -> Value >= Limit
    ; true ),
    Min is min(Min0,Value), Max is max(Max0,Value), Max-Min =< 16.

% At the target measure, an earlier, more extreme note already makes the
% target impossible. Once reached, later notes cannot exceed that extreme.
% This applies only to bounded randomized trials, never the ordered fallback.
third_target_bounds(random, Remaining, Measure, bounds(Min,Max,none), Result) :-
    nb_current(musicomp_contour, contour(Curve,_,Total,Target,random)),
    Target =:= Total-Remaining+1, !,
    maplist(note_value, Measure, Values),
    ( Curve = up -> max_list(Values, Extreme), Extreme =:= Max
    ; min_list(Values, Extreme), Extreme =:= Min ),
    Result = bounds(Min,Max,extreme(Curve,Extreme)).
third_target_bounds(_, _, _, Bounds, Bounds).

% Choose an arc once per search. Bias is the probability of trying the arc's
% current direction first (0.6 means 60%). Backtracking tries both directions.
% Curve may be left unbound for an equal random choice or fixed to up/down.
species_with_contour(Species, Scale, Cantus, Position, Bias, Curve, Counterpoint) :-
    species_with_contour(Species, Scale, Cantus, Position, Bias, Curve, _, Counterpoint).

% Target is a one-based non-final measure, random unless specified by caller.
species_with_contour(Species, Scale, Cantus, Position, Bias, Curve, Target, Counterpoint) :-
    memberchk(Species, [first,second,third,fourth,fifth]),
    must_be(number, Bias),
    ( Bias >= 0, Bias =< 1 -> true ; domain_error(probability, Bias) ),
    is_list(Cantus), length(Cantus, Total), Total >= 3,
    ( var(Curve) -> random_member(Curve, [up,down]) ; memberchk(Curve, [up,down]) ),
    LastInterior is Total-1,
    ( var(Target) -> random_between(1, LastInterior, Target)
    ; must_be(integer, Target),
      ( between(1, LastInterior, Target) -> true ; domain_error(nonfinal_measure, Target) ) ),
    weighted_opening(Opening),
    ( nb_current(musicomp_contour, Old) -> Previous = some(Old) ; Previous = none ),
    ( nb_current(musicomp_opening, OldOpening) -> PriorOpening = some(OldOpening)
    ; PriorOpening = none ),
    setup_call_cleanup(
        (nb_setval(musicomp_contour, contour(Curve,Bias,Total,Target,random)),
         nb_setval(musicomp_opening, Opening)),
        contour_search(Species, Scale, Cantus, Position, Counterpoint),
        (restore_contour(Previous), restore_opening(PriorOpening))).

restore_contour(some(Old)) :- nb_setval(musicomp_contour, Old).
restore_contour(none) :- nb_delete(musicomp_contour).
restore_opening(some(Old)) :- nb_setval(musicomp_opening, Old).
restore_opening(none) :- nb_delete(musicomp_opening).

% Each independent sample draws a weighted opening. If it cannot yield a
% valid line, backtracking can explore the other starting intervals.
weighted_opening(Opening) :-
    Choice is random_float,
    ( Choice < 0.3 -> Opening = 0
    ; Choice < 0.6 -> Opening = 7
    ; Opening = 12 ).

opening_order([Preferred|Others]) :-
    nb_current(musicomp_opening, Preferred),
    nb_current(musicomp_contour, contour(_,_,_,_,random)), !,
    select(Preferred, [0,7,12], Others).
opening_order([0,7,12]).

% Fresh randomized attempts make new opening draws when a caller requests
% multiple solutions. The final ordered branch retains complete enumeration.
sampled_species(Species, Scale, Cantus, Position, Counterpoint) :-
    distinct(Counterpoint,
        ( between(1,40,_),
          sampled_contour(Species,Scale,Cantus,Position,Counterpoint)
        ; weighted_ordered_solutions(Species,Scale,Cantus,Position,Counterpoint)
        )).

% Multi-solution sampling skips a repeated ordered fallback on every attempt;
% the one complete ordered fallback follows all randomized attempts instead.
sampled_contour(Species, Scale, Cantus, Position, Counterpoint) :-
    ( nb_current(musicomp_sampling, Old) -> Previous = some(Old) ; Previous = none ),
    setup_call_cleanup(nb_setval(musicomp_sampling, true),
        once(species_with_contour(Species,Scale,Cantus,Position,0.6,_,Counterpoint)),
        restore_sampling(Previous)).

restore_sampling(some(Old)) :- nb_setval(musicomp_sampling, Old).
restore_sampling(none) :- nb_delete(musicomp_sampling).

contour_search(Species, Scale, Cantus, Position, Counterpoint) :-
    nb_current(musicomp_sampling, true), !,
    contour_trials(2, Species, Scale, Cantus, Position, Counterpoint).
contour_search(Species, Scale, Cantus, Position, Counterpoint) :-
    ( contour_trials(8, Species, Scale, Cantus, Position, RandomResult) ->
        ( Counterpoint = RandomResult
        ; contour_set_mode(ordered),
          weighted_ordered_solutions(Species,Scale,Cantus,Position,Counterpoint),
          Counterpoint \== RandomResult )
    ; contour_set_mode(ordered),
      weighted_ordered_solutions(Species,Scale,Cantus,Position,Counterpoint)
    ).

contour_trials(Attempts, Species, Scale, Cantus, Position, Result) :-
    Attempts > 0, contour_set_mode(random),
    contour_base(Species, random, Scale, Cantus, Position, Base),
    ( call_with_inference_limit(
          once((prefer_voice_extremes(Base, Cantus, Species, P),
                contour_target_extreme(Species, P))),
          200000, Status), Status \== inference_limit_exceeded -> Result = P
    ; Next is Attempts-1, contour_trials(Next, Species, Scale, Cantus, Position, Result) ).

contour_base(first, _, Scale, C, P, first_species_base(Scale,C,P)).
contour_base(second, _, Scale, C, P, second_species_base(Scale,C,P)).
contour_base(third, Mode, Scale, C, P, third_species_base(Mode,Scale,C,P)).
contour_base(fourth, _, Scale, C, P, fourth_species_base(Scale,C,P)).
contour_base(fifth, _, Scale, C, P, fifth_species_base(Scale,C,P)).

contour_set_mode(Mode) :-
    nb_getval(musicomp_contour, contour(Curve,Bias,Total,Target,_)),
    nb_setval(musicomp_contour, contour(Curve,Bias,Total,Target,Mode)).

contour_candidate(Remaining, Previous, Generator, Note) :-
    ( nb_current(musicomp_contour, contour(_,_,_,_,random)) ->
        direction_candidate(contour(Remaining), Previous, Generator, Note)
    ; call(Generator, Note) ).

contour_mode(random, Remaining, contour(Remaining)) :- !.
contour_mode(Mode, _, Mode).

contour_directions(Remaining, Directions) :-
    ( nb_current(musicomp_contour, contour(Curve,Bias,Total,Target,random)) ->
        Measure is Total-Remaining+1,
        ( Target =:= 1 -> Preferred = down
        ; Measure =< Target -> Preferred = Curve
        ; opposite_direction(Curve, Preferred) ),
        opposite_direction(Preferred, Other),
        Choice is random_float,
        ( Choice < Bias -> Directions = [Preferred,Other] ; Directions = [Other,Preferred] )
    ; random_permutation([up,down], Directions) ).

opposite_direction(up, down).
opposite_direction(down, up).

% A randomized trial is preferred when the chosen extreme sounds in the
% target measure. Tied continuations count in each measure where they sound.
contour_target_extreme(Species, Counterpoint) :-
    nb_getval(musicomp_contour, contour(Curve,_,_,Target,_)),
    counterpoint_measures(Species, Counterpoint, Measures),
    maplist(measure_values, Measures, ValuesByMeasure),
    append(ValuesByMeasure, Values), Values = [_|_],
    ( Curve = up -> max_list(Values, Extreme) ; min_list(Values, Extreme) ),
    nth1(Target, ValuesByMeasure, TargetValues), memberchk(Extreme, TargetValues).

% The complete fallback merges the three finite opening-specific streams.
% It makes another weighted draw for every returned candidate and removes an
% exhausted opening from subsequent draws. Preferred voice-extrema solutions
% are exhausted before fallback solutions, as in prefer_voice_extremes/4.
weighted_ordered_solutions(Species, Scale, Cantus, Position, Counterpoint) :-
    ( weighted_opening_phase(separate,Species,Scale,Cantus,Position,Counterpoint)
    ; weighted_opening_phase(shared_measure,Species,Scale,Cantus,Position,Counterpoint) ).

weighted_opening_phase(Relation, Species, Scale, Cantus, Position, Counterpoint) :-
    setup_call_cleanup(
        create_opening_engines(Relation,Species,Scale,Cantus,Position,Engines),
        draw_opening_engine(Engines,Counterpoint),
        destroy_opening_engines(Engines)).

create_opening_engines(Relation, Species, Scale, Cantus, Position, Engines) :-
    findall(opening(Distance,Engine),
        (member(Distance,[0,7,12]),
         engine_create(P, ordered_opening_candidate(Distance,Relation,Species,Scale,Cantus,Position,P), Engine)),
        Engines).

ordered_opening_candidate(Distance, Relation, Species, Scale, Cantus, Position, Counterpoint) :-
    setup_call_cleanup(
        (nb_setval(musicomp_forced_opening,Distance), nb_setval(musicomp_contour,ordered_search),
         nb_setval(musicomp_cooperative, ticks(0))),
        (contour_base(Species,ordered,Scale,Cantus,Position,Base),
         call(Base,Counterpoint),
         counterpoint_extreme_relation(Cantus,Species,Counterpoint,Relation)),
        (nb_delete(musicomp_forced_opening), nb_delete(musicomp_contour),
         nb_delete(musicomp_cooperative))).

destroy_opening_engines([]).
destroy_opening_engines([opening(_,Engine)|Rest]) :-
    catch(engine_destroy(Engine),_,true), destroy_opening_engines(Rest).

draw_opening_engine(Engines, Counterpoint) :-
    Engines = [_|_],
    select_weighted_opening(Engines, Chosen),
    Chosen = opening(_,Engine),
    ( engine_next(Engine, Result) ->
        ( Result == search_pause -> draw_opening_engine(Engines, Counterpoint)
        ; ( Counterpoint = Result ; draw_opening_engine(Engines, Counterpoint) ) )
    ; select(Chosen,Engines,Remaining),
      draw_opening_engine(Remaining,Counterpoint)
    ).

select_weighted_opening(Engines, Chosen) :-
    findall(W,(member(opening(D,_),Engines), opening_weight(D,W)),Weights),
    sum_list(Weights,Total), random_between(1,Total,Ticket),
    opening_for_ticket(Engines,Ticket,Chosen).

opening_weight(0,30).
opening_weight(7,30).
opening_weight(12,40).

opening_for_ticket([opening(D,E)|_], Ticket, opening(D,E)) :-
    opening_weight(D,Weight), Ticket =< Weight, !.
opening_for_ticket([opening(D,_)|Rest], Ticket, Chosen) :-
    opening_weight(D,Weight), Next is Ticket-Weight,
    opening_for_ticket(Rest,Next,Chosen).


% Long-running fallback streams yield periodically, so one difficult opening
% cannot block the other weighted streams. The engine resumes where it left off.
cooperative_search_tick :-
    ( nb_current(musicomp_cooperative, State) ->
        arg(1,State,Count), Next is Count+1,
        ( Next >= 10000 -> nb_setarg(1,State,0), engine_yield(search_pause)
        ; nb_setarg(1,State,Next) )
    ; true ).
