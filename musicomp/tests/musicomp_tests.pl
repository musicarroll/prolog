:- begin_tests(musicomp).
:- ensure_loaded('../musicomp.pl').
:- use_module(library(sgml)).

test(piano_low_boundary, [fail]) :- is_note([a,-1,0]).
test(piano_low_enharmonic) :- is_note([b,-1,0]).
test(piano_high_boundary, [fail]) :- is_note([c,1,8]).
test(piano_high_enharmonic) :- is_note([b,1,7]).
test(canonical_flat) :- canonical_note([d,-1,4], [c,1,4]).
test(canonical_octave) :- canonical_note([c,-1,4], [b,0,3]).
test(middle_c_midi) :- note_to_midi([c,0,4], 60).
test(enharmonic_interval) :- interval([c,1,4], [d,-1,4], 0).
test(enharmonic_repeat, [fail]) :- no_consecutive_repeats([[c,1,4],[d,-1,4]]).
test(candidate_deduplication) :-
    findall(N, phrase_notes([N], [c,0,4], major), Notes),
    sort(Notes, Unique), length(Notes, Count), length(Unique, Count).
test(phrase_lengths) :-
    findall(L, (phrase_length(P), length(P, L)), [8,9,10,11,12]).
test(short_patterns, [nondet]) :-
    maplist(check_repeated_two_note_patterns, [[],[[c,0,4]],[[c,0,4],[d,0,4]]]).
test(enharmonic_pattern, [fail]) :-
    check_repeated_two_note_patterns([[c,1,4],[d,0,4],[d,-1,4],[d,0,4]]).
test(leading_tone_exception_transposed, [nondet]) :-
    check_leading_tone_rule([[d,0,4],[f,1,4],[g,0,4]], [g,0,4], major).
test(leading_tone_invalid_leap, [fail]) :-
    check_leading_tone_rule([[c,0,4],[f,1,4],[g,0,4]], [g,0,4], major).
test(d_dorian_pitch_classes, [nondet]) :-
    findall(D, (between(0,11,D), scale_degrees(dorian,Degrees),
                memberchk(D,Degrees)), DegreesFound),
    DegreesFound == [0,2,3,5,7,9,10],
    maplist(in_d_dorian, [[d,0,4],[e,0,4],[f,0,4],[g,0,4],
                          [a,0,4],[b,0,4],[c,0,5]]),
    \+ in_scale([f,1,4],[d,0,4],dorian),
    \+ in_scale([b,-1,4],[d,0,4],dorian),
    \+ in_scale([c,1,5],[d,0,4],dorian).
in_d_dorian(Note) :- in_scale(Note,[d,0,4],dorian).
test(dorian_transposes, [nondet]) :-
    in_scale([e,-1,4],[c,0,4],dorian),
    in_scale([a,0,4],[c,0,4],dorian),
    \+ in_scale([a,-1,4],[c,0,4],dorian).
test(dorian_subtonic_no_major_leading_rule, [nondet]) :-
    check_leading_tone_rule([[g,0,4],[c,0,5],[d,0,5]], [d,0,4], dorian).
test(dorian_isometric_generation, [nondet]) :-
    Line = [[d,0,4],[c,0,4],[d,0,4],[e,0,4],
            [d,0,4],[f,0,4],[e,0,4],[d,0,4]],
    isometric_phrase(dorian,[d,0,4],Line),
    isometric_phrase_constrained(dorian,[d,0,4],[c,0,4],[e,0,4],Line).
test(minor_subtonic, [nondet]) :-
    check_leading_tone_rule([[c,0,4],[g,0,4],[a,0,4]], [a,0,4], minor).
test(isometric_broken_triad_fifth_then_third, [fail]) :-
    check_isometric_broken_triads([[c,0,4],[g,0,4],[e,0,4]]).
test(isometric_broken_minor_triad_inversion, [fail]) :-
    check_isometric_broken_triads([[g,0,4],[c,0,4],[e,-1,4]]).
test(isometric_broken_triad_enharmonic, [fail]) :-
    check_isometric_broken_triads([[c,0,4],[g,0,4],[f,-1,4]]).
test(isometric_fourth_then_third, [nondet]) :-
    Phrase = [[c,0,4],[f,0,4],[d,0,4],[e,0,4],
              [d,0,4],[c,0,4],[b,0,3],[c,0,4]],
    isometric_phrase(major,[c,0,4],Phrase),
    isometric_phrase_constrained(major,[c,0,4],[f,0,4],[b,0,3],Phrase).
test(isometric_opposite_thirds_are_not_a_triad, [nondet]) :-
    check_isometric_broken_triads([[c,0,4],[e,0,4],[c,0,4]]).
test(isometric_random_selection_without_replacement, [nondet]) :-
    getrand(State),
    setup_call_cleanup(set_random(seed(42)),
        (choose_isometric_candidates(3,[a,b,c,d,e],Selected),
         length(Selected,3), sort(Selected,Unique), length(Unique,3),
         forall(member(Item,Selected),memberchk(Item,[a,b,c,d,e]))),
        setrand(State)).
test(isometric_random_selection_too_few, [fail]) :-
    choose_isometric_candidates(3,[a,b],_).
test(choosenisosols_rejects_excess_before_writing, [nondet]) :-
    tmp_file_stream(text,File,Stream), write(Stream,untouched), close(Stream),
    setup_call_cleanup(true,
        (\+ choosenisosols(3,2,dorian,[d,0,4],File),
         read_file_to_string(File,Text,[]), Text == "untouched"),
        delete_file(File)).
test(choosenisosols_writes_requested_lines, [nondet]) :-
    tmp_file_stream(text,File,Stream), close(Stream), getrand(State),
    setup_call_cleanup(set_random(seed(5)),
        (choosenisosols(2,3,dorian,[d,0,4],File),
         load_structure(File,Doc,[dialect(xml),space(remove)]),
         findall(M,sub_term(element(measure,_,M),Doc),Measures),
         length(Measures,2)),
        (setrand(State),delete_file(File))).
test(isometric_range_octave, [nondet]) :-
    isometric_range_within([[c,0,4],[c,0,5]]).
test(isometric_range_larger_than_octave, [fail]) :-
    isometric_range_within([[c,0,4],[d,0,5]]).
test(isometric_generator_rejects_range_larger_than_octave, [fail]) :-
    isometric_phrase(major,[c,0,4],
        [[c,0,4],[d,0,5],[e,0,5],[d,0,5],[c,0,5],[b,0,4],[d,0,4],[c,0,4]]).
test(invalid_root, [fail]) :- isometric_phrase(major, [c,1,8], _).
test(constrained_generation, [nondet]) :-
    once(isometric_phrase_constrained(major, [c,0,4], [d,0,4], [b,0,3], P)),
    length(P, L), between(8, 12, L),
    P = [[c,0,4],[d,0,4]|_], append(_, [[b,0,3],[c,0,4]], P),
    maplist(is_note, P), isometric_range_within(P), single_highest(P),
    no_consecutive_repeats(P), check_isometric_leaps(P),
    check_isometric_broken_triads(P).
test(midi_header_and_end) :-
    midi_file(quarter, [[c,0,4]], Bytes),
    append([77,84,104,100|_], [0,255,47,0], Bytes), !.
test(musicxml_escaping_and_attributes, [nondet]) :-
    tmp_file_stream(text, File, Stream), close(Stream),
    setup_call_cleanup(
        true,
        (write_musicxml([[[c,0,4],[d,-1,4]]], File, 'A & B', '<score>'),
         load_structure(File, DOM, [dialect(xml), space(remove)]),
         DOM = [element('score-partwise', _, Children)],
         member(element(work, _, [element('work-title', _, ['A & B'])]), Children),
         member(element('movement-title', _, ['<score>']), Children),
         sub_term(element(divisions, _, ['1']), DOM),
         sub_term(element('senza-misura', _, []), DOM),
         sub_term(element(alter, _, ['-1']), DOM)),
        delete_file(File)).

example_cantus([[c,0,4],[b,0,3],[c,0,4],[d,0,4],
                [c,0,4],[e,0,4],[d,0,4],[c,0,4]]).

test(species_above, [nondet]) :-
    example_cantus(Cantus),
    once((first_species(major, Cantus, above, CP), append(_, [[b,0,4],[c,0,5]], CP))),
    same_length(Cantus, CP), unique_extremes(CP),
    maplist(upper_consonance, Cantus, CP).

test(species_below, [nondet]) :-
    example_cantus(Cantus), once(first_species(major, Cantus, below, CP)),
    unique_extremes(CP), maplist(lower_consonance, Cantus, CP).

upper_consonance(C, N) :- note_value(C, A), note_value(N, B),
    D is B-A, memberchk(D, [0,3,4,7,8,9,12,15,16,19]).
lower_consonance(C, N) :- upper_consonance(N, C).

test(minor_raised_leading_tone, [nondet]) :-
    Cantus = [[a,0,3],[b,0,3],[c,0,4],[d,0,4],[c,0,4],[b,0,3],[a,0,3]],
    once((first_species(minor, Cantus, above, CP),
          append(_, [[g,1,4],[a,0,4]], CP))).

test(parallel_fifths_rejected, [fail]) :-
    species_transition([c,0,4],[d,0,4],[g,0,4],[a,0,4],none,_).
test(direct_octave_rejected, [fail]) :-
    species_transition([c,0,4],[d,0,4],[a,0,4],[d,0,5],none,_).
test(crossing_rejected, [fail]) :-
    species_transition([c,0,4],[g,0,4],[e,0,4],[d,0,4],none,_).
test(leap_recovery_rejected, [fail]) :-
    species_transition([c,0,4],[d,0,4],[g,0,4],[a,0,4],leap(7),_).
test(invalid_cantus_cadence, [fail]) :-
    first_species(major, [[c,0,4],[e,0,4],[c,0,4]], _).

test(two_voice_midi_synchronization) :-
    counterpoint_midi_bytes([[e,0,4]], [[c,0,4]], quarter, Bytes),
    Bytes = [77,84,104,100,0,0,0,6,0,1,0,2,1,224|Tracks],
    Tracks = [77,84,114,107,0,0,0,13,0,144,64,64,131,96,128,64,64,0,255,47,0,
              77,84,114,107,0,0,0,13,0,145,60,64,131,96,129,60,64,0,255,47,0].
test(two_voice_midi_mismatched_lengths, [fail]) :-
    counterpoint_midi_bytes([[c,0,4]], [], quarter, _).

test(two_voice_xml, [nondet]) :-
    tmp_file_stream(text, File, Stream), close(Stream),
    setup_call_cleanup(true,
        (write_counterpoint_xml([[e,0,4],[d,0,4]], [[c,0,4],[b,0,3]], File, piano, 'A & B'),
         load_structure(File, DOM, [dialect(xml), space(remove)]),
         sub_term(element(staves, _, ['2']), DOM),
         sub_term(element(backup, _, [element(duration, _, ['8'])]), DOM),
         findall(V, sub_term(element(voice, _, [V]), DOM), ['1','1','2','2']),
         findall(S, sub_term(element(staff, _, [S]), DOM), ['1','1','2','2'])),
        delete_file(File)).
test(two_voice_single_staff_xml, [nondet]) :-
    tmp_file_stream(text, File, Stream), close(Stream),
    setup_call_cleanup(true,
        (write_counterpoint_xml([[e,0,4]], [[c,0,4]], File, single),
         load_structure(File, DOM, [dialect(xml), space(remove)]),
         findall(S, sub_term(element(staff, _, [S]), DOM), ['1','1']),
         \+ sub_term(element(staves, _, _), DOM)),
        delete_file(File)).
test(two_voice_xml_mismatched_lengths, [fail]) :-
    write_counterpoint_xml([[c,0,4]], [], '/tmp/unused-musicomp.xml', piano).

test(second_species_above, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    once(second_species(major, C, above, P)),
    length(C, N), length(P, M), M =:= 2*N-1, unique_extremes(P),
    check_second_pairs(C, P).
test(second_species_below, [nondet]) :-
    example_cantus(C), once(second_species(major, C, below, P)),
    length(P, 15).

check_second_pairs([C], [N]) :- upper_consonance(C, N).
check_second_pairs([C,Next|Cs], [Down,Weak,NextDown|Rest]) :-
    upper_consonance(C, Down),
    second_passing_rule(C, Down, Weak, NextDown),
    second_downbeat_rule(C, Next, Down, NextDown),
    check_second_pairs([Next|Cs], [NextDown|Rest]).

test(second_species_minor_cadence, [nondet]) :-
    C = [[a,0,3],[b,0,3],[c,0,4],[d,0,4],[c,0,4],[b,0,3],[a,0,3]],
    once(second_species(minor, C, above, P)),
    append(_, [[g,1,4],[a,0,4]], P).

test(second_passing_dissonance) :-
    second_passing_rule([c,0,4],[e,0,4],[f,0,4],[g,0,4]).
test(second_neighbor_dissonance_rejected, [fail]) :-
    second_passing_rule([c,0,4],[e,0,4],[f,0,4],[e,0,4]).
test(second_leap_to_dissonance_rejected, [fail]) :-
    second_passing_rule([c,0,4],[c,0,5],[f,0,4],[e,0,4]).
test(second_parallel_downbeat_fifths_rejected, [fail]) :-
    second_downbeat_rule([c,0,4],[d,0,4],[g,0,4],[a,0,4]).

test(second_species_midi_timing, [nondet]) :-
    second_species_midi_bytes([[c,0,4],[d,0,4]],
                              [[e,0,4],[f,0,4],[d,0,4]], Bytes),
    Bytes = [77,84,104,100,0,0,0,6,0,1,0,2,1,224|Tracks],
    Tracks = [77,84,114,107,0,0,0,22,
              0,144,60,64,143,0,128,60,64,0,144,62,64,143,0,128,62,64,0,255,47,0,
              77,84,114,107,0,0,0,31,
              0,145,64,64,135,64,129,64,64,0,145,65,64,135,64,129,65,64,
              0,145,62,64,143,0,129,62,64,0,255,47,0].
test(second_species_midi_invalid_length, [fail]) :-
    second_species_midi_bytes([[c,0,4],[d,0,4]], [[e,0,4]], _).

test(second_species_xml_rhythm, [nondet]) :-
    tmp_file_stream(text, File, Stream), close(Stream),
    setup_call_cleanup(true,
        (write_second_species_xml([[c,0,4],[d,0,4]],
             [[e,0,4],[f,0,4],[d,0,4]], above, File, piano),
         load_structure(File, DOM, [dialect(xml), space(remove)]),
         findall(Notes, (sub_term(element(measure, _, Elements), DOM),
                         findall(Type, (member(element(note, _, Fields), Elements),
                                        member(element(type, _, [Type]), Fields)), Notes)),
                 [['half','half','whole'],['whole','whole']]),
         findall(D, sub_term(element(backup, _, [element(duration, _, [D])]), DOM), ['4','4'])),
        delete_file(File)).
test(second_species_xml_below, [nondet]) :-
    tmp_file_stream(text, File, Stream), close(Stream),
    setup_call_cleanup(true,
        (write_second_species_xml([[c,0,4],[d,0,4]],
             [[e,0,3],[f,0,3],[d,0,3]], below, File, single),
         load_structure(File, DOM, [dialect(xml), space(remove)]),
         sub_term(element(measure, _, Elements), DOM),
         findall(T, (member(element(note, _, Fields), Elements),
                     member(element(type, _, [T]), Fields)), ['whole','half','half']), !),
        delete_file(File)).

test(third_species_above, [nondet]) :-
    example_cantus(C), once(third_species(major, C, above, P)),
    length(C, N), length(P, M), M =:= 4*N-3, unique_extremes(P),
    check_third_measures(C, P), check_measure_patterns(P).
test(third_species_below, [nondet]) :-
    example_cantus(C), once(third_species(major, C, below, P)),
    length(P, 29), check_measure_patterns(P).
test(third_species_minor_cadence, [nondet]) :-
    C = [[a,0,3],[b,0,3],[a,0,3]],
    once(third_species(minor, C, P)),
    append(_, [[g,1,4],[a,0,4]], P).

check_third_measures([C], [N]) :- upper_consonance(C, N).
check_third_measures([C,Next|Cs], [A,B,D,E,F|Rest]) :-
    upper_consonance(C, A),
    no_repeated_quarter_motif(A, B, D, E),
    third_accent_rule(C, B, D, E),
    third_dissonance_rule(C, A, B, D),
    third_dissonance_rule(C, B, D, E),
    third_dissonance_rule(C, D, E, F),
    second_downbeat_rule(C, Next, A, F),
    check_third_measures([Next|Cs], [F|Rest]).

test(third_dissonant_passing, [nondet]) :-
    third_dissonance_rule([c,0,4],[c,0,5],[b,0,4],[a,0,4]).
test(third_dissonant_neighbor) :-
    third_dissonance_rule([c,0,4],[e,0,4],[f,0,4],[e,0,4]).
test(third_dissonant_leap_rejected, [fail]) :-
    third_dissonance_rule([c,0,4],[e,0,4],[f,0,4],[a,0,4]).
test(third_consecutive_dissonances_rejected, [fail]) :-
    third_dissonance_rule([c,0,4],[d,0,4],[f,0,4],[g,0,4]).
test(third_invalid_length, [fail]) :-
    third_species_midi_bytes([[c,0,4],[d,0,4]], [[e,0,4]], _).
test(third_midi_rhythm, [nondet]) :-
    third_species_midi_bytes([[c,0,4],[d,0,4]],
        [[e,0,4],[f,0,4],[g,0,4],[a,0,4],[d,0,4]], Bytes),
    Bytes = [77,84,104,100,0,0,0,6,0,1,0,2,1,224|Tracks],
    Tracks = [77,84,114,107,0,0,0,22,
        0,144,60,64,143,0,128,60,64,0,144,62,64,143,0,128,62,64,0,255,47,0,
        77,84,114,107,0,0,0,49,
        0,145,64,64,131,96,129,64,64,0,145,65,64,131,96,129,65,64,
        0,145,67,64,131,96,129,67,64,0,145,69,64,131,96,129,69,64,
        0,145,62,64,143,0,129,62,64,0,255,47,0].
test(third_xml_rhythm, [nondet]) :-
    tmp_file_stream(text, File, Stream), close(Stream),
    setup_call_cleanup(true,
        (write_third_species_xml([[c,0,4],[d,0,4]],
            [[e,0,4],[f,0,4],[g,0,4],[a,0,4],[d,0,4]], above, File, piano),
         load_structure(File, DOM, [dialect(xml), space(remove)]),
         findall(Notes, (sub_term(element(measure, _, Elements), DOM),
                         findall(Type, (member(element(note, _, Fields), Elements),
                                        member(element(type, _, [Type]), Fields)), Notes)),
                 [['quarter','quarter','quarter','quarter','whole'],['whole','whole']]),
         findall(D, sub_term(element(backup, _, [element(duration, _, [D])]), DOM), ['4','4'])),
        delete_file(File)).
test(third_xml_below_single, [nondet]) :-
    tmp_file_stream(text, File, Stream), close(Stream),
    setup_call_cleanup(true,
        (write_third_species_xml([[c,0,4],[d,0,4]],
            [[e,0,3],[f,0,3],[g,0,3],[a,0,3],[d,0,3]], below, File, single),
         load_structure(File, DOM, [dialect(xml), space(remove)]),
         sub_term(element(measure, _, Elements), DOM),
         findall(T, (member(element(note, _, Fields), Elements),
                     member(element(type, _, [T]), Fields)),
                 ['whole','quarter','quarter','quarter','quarter']), !),
        delete_file(File)).

test(extremes_unique) :-
    unique_extremes([[c,0,4],[e,0,4],[g,0,4],[d,0,4]]).
test(extremes_repeated_endpoints_allowed) :-
    unique_extremes([[c,0,4],[e,0,4],[g,0,4],[c,0,4]]).
test(extremes_repeated_high_endpoints_allowed) :-
    unique_extremes([[g,0,4],[e,0,4],[c,0,4],[g,0,4]]).
test(extremes_repeated_interior_high_rejected, [fail]) :-
    unique_extremes([[c,0,4],[g,0,4],[e,0,4],[g,0,4],[c,0,4]]).
test(extremes_repeated_interior_low_rejected, [fail]) :-
    unique_extremes([[g,0,4],[c,0,4],[e,0,4],[c,0,4],[g,0,4]]).
test(extremes_endpoint_plus_interior_rejected, [fail]) :-
    unique_extremes([[c,0,4],[g,0,4],[c,0,4],[e,0,4]]).
test(extremes_enharmonic_duplicate_rejected, [fail]) :-
    unique_extremes([[c,0,4],[c,1,4],[d,-1,4],[c,0,4]]).
test(extremes_empty_rejected, [fail]) :- unique_extremes([]).

test(repeated_quarter_motif_rejected, [fail]) :-
    no_repeated_quarter_motif([e,0,4],[f,0,4],[e,0,4],[f,0,4]).
test(repeated_quarter_motif_enharmonic_rejected, [fail]) :-
    no_repeated_quarter_motif([c,1,4],[f,0,4],[d,-1,4],[e,1,4]).
test(distinct_quarter_motif_accepted) :-
    no_repeated_quarter_motif([e,0,4],[f,0,4],[e,0,4],[g,0,4]).

test(downbeat_hidden_fifth_rejected, [fail]) :-
    second_downbeat_rule([c,0,4],[d,0,4],[e,0,4],[a,0,4]).
test(downbeat_hidden_octave_rejected, [fail]) :-
    second_downbeat_rule([c,0,4],[d,0,4],[e,0,4],[d,0,5]).
test(downbeat_hidden_fifth_lower_voice_rejected, [fail]) :-
    second_downbeat_rule([e,0,4],[a,0,4],[c,0,4],[d,0,4]).
test(downbeat_parallel_octaves_rejected, [fail]) :-
    second_downbeat_rule([c,0,4],[d,0,4],[c,0,5],[d,0,5]).
test(downbeat_contrary_fifth_allowed) :-
    second_downbeat_rule([c,0,4],[d,0,4],[a,0,4],[g,0,4]).
test(downbeat_oblique_fifth_allowed) :-
    second_downbeat_rule([c,0,4],[d,0,4],[a,0,4],[a,0,4]).

test(fourth_species_above_cadence, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    once((fourth_species(major,C,above,P),
          P = [[rest,First],[tie(First),Weak],[Final]], note_value(Final, 60))),
    fourth_cadential_suspension([d,0,4],First,Weak,above),
    fourth_score_attacks(C,P,Attacks), unique_extremes(Attacks).
test(fourth_species_below_cadence, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    once((fourth_species(major,C,below,P),
          P = [[rest,First],[tie(First),Weak],[Final]], note_value(Final, 48))),
    fourth_cadential_suspension([d,0,4],First,Weak,below).
test(fourth_species_minor_leading_tone, [nondet]) :-
    C = [[a,0,3],[b,0,3],[a,0,3]],
    once((fourth_species(minor,C,P),
          P = [[rest,_],[tie(_),[g,1,4]],[[a,0,4]]])).
test(fourth_species_long_cantus, [nondet]) :-
    example_cantus(C), once(fourth_species(major,C,above,P)),
    same_length(C,P), fourth_score_attacks(C,P,Attacks), unique_extremes(Attacks).
test(fourth_unprepared_tie_rejected, [fail]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    fourth_score_attacks(C, [[rest,[c,0,5]],[tie([d,0,5]),[b,0,4]],[[c,0,5]]], _).
test(fourth_four_three_suspension, [nondet]) :-
    fourth_resolution([c,0,4],[f,0,4],[e,0,4],above).
test(fourth_seven_six_suspension, [nondet]) :-
    fourth_resolution([d,0,4],[c,0,5],[b,0,4],above).
test(fourth_two_three_suspension, [nondet]) :-
    fourth_resolution([d,0,4],[c,0,4],[b,0,3],below).
test(fourth_dissonance_upward_rejected, [fail]) :-
    fourth_resolution([c,0,4],[f,0,4],[g,0,4],above).
test(fourth_midi_tied_duration, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    P = [[rest,[c,0,5]],[tie([c,0,5]),[b,0,4]],[[c,0,5]]],
    fourth_species_midi_bytes(C,P,Bytes),
    Bytes = [77,84,104,100,0,0,0,6,0,1,0,2,1,224|Tracks],
    Tracks = [77,84,114,107,0,0,0,31,
        0,144,60,64,143,0,128,60,64,0,144,62,64,143,0,128,62,64,
        0,144,60,64,143,0,128,60,64,0,255,47,0,
        77,84,114,107,0,0,0,32,
        135,64,145,72,64,143,0,129,72,64,
        0,145,71,64,135,64,129,71,64,
        0,145,72,64,143,0,129,72,64,0,255,47,0].
test(fourth_xml_ties_and_rest, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    P = [[rest,[c,0,5]],[tie([c,0,5]),[b,0,4]],[[c,0,5]]],
    tmp_file_stream(text, File, Stream), close(Stream),
    setup_call_cleanup(true,
        (write_fourth_species_xml(C,P,above,File,piano,'A & B'),
         load_structure(File, DOM, [dialect(xml), space(remove)]),
         findall(T, sub_term(element(tie,[type=T],[]), DOM), [start,stop]),
         findall(T, sub_term(element(tied,[type=T],[]), DOM), [start,stop]),
         sub_term(element(rest,_,[]), DOM),
         findall(Notes, (sub_term(element(measure, _, Elements), DOM),
                          findall(Type, (member(element(note, _, Fields), Elements),
                                         member(element(type, _, [Type]), Fields)), Notes)),
                 [[half,half,whole],[half,half,whole],[whole,whole]])),
        delete_file(File)).
test(fourth_xml_rejects_broken_tie, [fail]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    P = [[rest,[c,0,5]],[tie([d,0,5]),[b,0,4]],[[c,0,5]]],
    write_fourth_species_xml(C,P,above,'/tmp/unused-fourth.xml',piano).

% Compare only full four-quarter measures; the final whole note is excluded.
check_measure_patterns([_]).
check_measure_patterns([_,_,_,_,_]).
check_measure_patterns([A,B,C,D,E,F,G,H|Rest]) :-
    quarter_measure_pattern([A,B,C,D], Pattern1),
    quarter_measure_pattern([E,F,G,H], Pattern2),
    Pattern1 \== Pattern2,
    check_measure_patterns([E,F,G,H|Rest]).

test(transposed_ascending_patterns_match) :-
    quarter_measure_pattern([[e,0,4],[f,0,4],[g,0,4],[a,0,4]], P),
    quarter_measure_pattern([[f,0,4],[g,0,4],[a,0,4],[b,0,4]], P).
test(transposed_descending_patterns_match) :-
    quarter_measure_pattern([[a,0,4],[g,0,4],[f,0,4],[e,0,4]], P),
    quarter_measure_pattern([[g,0,4],[f,0,4],[e,0,4],[d,0,4]], P).
test(transposed_thirds_patterns_match) :-
    quarter_measure_pattern([[c,0,4],[e,0,4],[d,0,4],[f,0,4]], P),
    quarter_measure_pattern([[d,0,4],[f,0,4],[e,0,4],[g,0,4]], P).
test(different_pattern_allowed) :-
    quarter_measure_pattern([[e,0,4],[f,0,4],[g,0,4],[a,0,4]], P),
    quarter_measure_pattern([[f,0,4],[g,0,4],[f,0,4],[g,0,4]], Q),
    P \== Q.
test(different_leap_size_allowed) :-
    quarter_measure_pattern([[c,0,4],[f,0,4],[e,0,4],[d,0,4]], P),
    quarter_measure_pattern([[c,0,4],[g,0,4],[f,0,4],[e,0,4]], Q),
    P \== Q.

test(fifth_species_above_mixed_rhythm, [nondet]) :-
    example_cantus(C), once(fifth_species(major,C,above,P)),
    fifth_score_attacks(C,P,Attacks), unique_extremes(Attacks),
    member(Measure,P), member(note(_,quarter,none),Measure),
    member(Other,P), member(note(_,half,start),Other), !.
test(fifth_species_below, [nondet]) :-
    C = [[c,0,4],[e,0,4],[f,0,4],[e,0,4],[d,0,4],[c,0,4]],
    once(fifth_species(major,C,below,P)), fifth_score_attacks(C,P,Attacks),
    unique_extremes(Attacks), member(Measure,P), member(note(_,quarter,none),Measure), !.
test(fifth_species_minor_cadence, [nondet]) :-
    C = [[a,0,3],[c,0,4],[d,0,4],[c,0,4],[b,0,3],[a,0,3]],
    once(fifth_species(minor,C,above,P)),
    append(_, [Penultimate,[note(Final,whole,none)]], P),
    last(Penultimate,note(Leading,half,none)),
    note_value(Leading,LeadValue), note_value(Final,FinalValue),
    LeadValue mod 12 =:= 8, FinalValue mod 12 =:= 9.
test(fifth_broken_tie_rejected, [fail]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    P = [[rest(half),note([c,0,5],half,start)],
         [note([d,0,5],half,stop),note([b,0,4],half,none)],
         [note([c,0,5],whole,none)]],
    fifth_score_attacks(C,P,_).
test(fifth_midi_quarter_tie_timing, [nondet]) :-
    Events = [note([c,0,4],half,start),note([c,0,4],quarter,stop),
              note([d,0,4],quarter,none)],
    fifth_midi_events(Events,[135,64],Bytes),
    Bytes = [135,64,145,60,64,139,32,129,60,64,
             0,145,62,64,131,96,129,62,64].
test(fifth_xml_ties_and_rhythms, [nondet]) :-
    example_cantus(C), once(fifth_species(major,C,above,P)),
    tmp_file_stream(text, File, Stream), close(Stream),
    setup_call_cleanup(true,
        (write_fifth_species_xml(C,P,above,File,piano,'A & B'),
         load_structure(File, DOM, [dialect(xml), space(remove)]),
         findall(T,sub_term(element(tie,[type=T],[]),DOM),Ties),
         include(=(start),Ties,Starts), include(=(stop),Ties,Stops),
         length(Starts,Count), length(Stops,Count), Count > 0,
         sub_term(element(type,_,[quarter]),DOM),
         sub_term(element(type,_,[half]),DOM),
         sub_term(element(rest,_,[]),DOM)),
        delete_file(File)).
test(fifth_xml_rejects_broken_tie, [fail]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    P = [[rest(half),note([c,0,5],half,start)],
         [note([d,0,5],half,stop),note([b,0,4],half,none)],
         [note([c,0,5],whole,none)]],
    write_fifth_species_xml(C,P,above,'/tmp/unused-fifth.xml',piano).

combined_sample(Species, C, P) :-
    memberchk(Species, [first,second,third,fourth]),
    C = [[c,0,4],[d,0,4],[c,0,4]],
    atom_concat(Species, '_species', Name),
    Goal =.. [Name,major,C,above,P], once(call(Goal)).
combined_sample(fifth, C, P) :-
    example_cantus(C), once(fifth_species(major,C,above,P)).

test(combined_all_species,
     [forall(member(Species,[first,second,third,fourth,fifth])), nondet]) :-
    combined_sample(Species,C,P),
    tmp_file_stream(text, File, Stream), close(Stream),
    setup_call_cleanup(true,
        (write_solutions_musicxml(C,Species,above,[P,P],File,piano,'A & B'),
         load_structure(File, DOM, [dialect(xml), space(remove)]),
         sub_term(element('work-title',_,['A & B']),DOM),
         findall(Label, sub_term(element(words,_,[Label]),DOM), ['Solution 1','Solution 2']),
         findall(Number,sub_term(element(measure,[number=Number],_),DOM),Numbers),
         ( Species = first -> Expected = 2 ; length(C,N), Expected is 2*N ),
         length(Numbers,Expected),
         numlist(1,Expected,Ints), maplist(atom_number,Numbers,Ints),
         findall(Style,sub_term(element('bar-style',_,[Style]),DOM),['light-light','light-light']),
         sub_term(element(staves,_,['2']),DOM),
         findall(c,sub_term(element(clef,_,_),DOM),[c,c]),
         findall(t,sub_term(element(time,_,_),DOM),[t]),
         findall(d,sub_term(element(divisions,_,_),DOM),[d])),
        delete_file(File)).

test(combined_distinct_third_solutions, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    findnsols(2,P,third_species(major,C,above,P),[A,B]), A \== B,
    tmp_file_stream(text,File,Stream), close(Stream),
    setup_call_cleanup(true,
        (write_solutions_musicxml(C,third,above,[A,B],File,single),
         load_structure(File,DOM,[dialect(xml),space(remove)]),
         findall(L,sub_term(element(words,_,[L]),DOM),['Solution 1','Solution 2']),
         findall(m,sub_term(element(measure,_,_),DOM),Measures), length(Measures,6),
         \+ sub_term(element(staves,_,_),DOM)),
        delete_file(File)).
test(combined_fourth_ties_preserved, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    P = [[rest,[c,0,5]],[tie([c,0,5]),[b,0,4]],[[c,0,5]]],
    tmp_file_stream(text,File,Stream), close(Stream),
    setup_call_cleanup(true,
        (write_solutions_musicxml(C,fourth,above,[P,P],File,piano),
         load_structure(File,DOM,[dialect(xml),space(remove)]),
         findall(T,sub_term(element(tie,[type=T],[]),DOM),[start,stop,start,stop])),
        delete_file(File)).
test(combined_empty_solutions_rejected, [fail]) :-
    write_solutions_musicxml([[c,0,4]],first,above,[],'/tmp/no-solutions.xml',piano).
test(combined_bad_solution_rejected, [fail]) :-
    write_solutions_musicxml([[c,0,4],[d,0,4]],second,above,
                             [[[e,0,4]]],'/tmp/bad-solutions.xml',piano).

% Deliberately place the fallback before the preferred solution to verify
% ordering independently of the underlying generator's enumeration.
extreme_fixture(CP) :-
    member(CP, [ [[e,0,4],[f,0,4],[d,0,4]],
                 [[c,0,4],[e,0,4],[c,0,4]],
                 [[e,0,4],[c,1,4],[e,0,4]] ]).

test(voice_extrema_priority_and_fallback) :-
    Cantus = [[c,0,4],[d,0,4],[c,0,4]],
    findall(P, prefer_voice_extremes(extreme_fixture,Cantus,first,P), Solutions),
    Solutions = [ [[e,0,4],[c,1,4],[e,0,4]],
                  [[e,0,4],[f,0,4],[d,0,4]] ].
test(voice_same_low_pitch_rejected, [fail]) :-
    counterpoint_extreme_relation([[c,0,4],[d,0,4],[c,0,4]],first,
                                 [[c,0,4],[e,0,4],[c,0,4]],_).
test(voice_same_high_enharmonic_rejected, [fail]) :-
    counterpoint_extreme_relation([[c,0,4],[c,1,4],[c,0,4]],first,
                                 [[b,0,3],[d,-1,4],[b,0,3]],_).
test(voice_extrema_preferred) :-
    counterpoint_extreme_relation([[c,0,4],[d,0,4],[c,0,4]],first,
                                 [[e,0,4],[c,1,4],[e,0,4]],separate).
test(voice_extrema_fallback) :-
    counterpoint_extreme_relation([[c,0,4],[d,0,4],[c,0,4]],first,
                                 [[e,0,4],[f,0,4],[d,0,4]],shared_measure).
test(voice_extrema_fourth_ties_in_both_measures) :-
    counterpoint_measures(fourth,
        [[rest,[a,0,4]],[tie([a,0,4]),[b,0,4]],[[c,0,5]]],
        [[[a,0,4]],[[a,0,4],[b,0,4]],[[c,0,5]]]).
test(voice_extrema_fifth_ties_in_both_measures) :-
    counterpoint_measures(fifth,
        [[rest(half),note([a,0,4],half,start)],
         [note([a,0,4],quarter,stop),note([b,0,4],quarter,none),
          note([c,0,5],half,none)]],
        [[[a,0,4]],[[a,0,4],[b,0,4],[c,0,5]]]).
test(all_species_respect_voice_extrema,
     [forall(member(Species,[first,second,third,fourth,fifth])), nondet]) :-
    combined_sample(Species,C,P),
    counterpoint_extreme_relation(C,Species,P,Relation),
    memberchk(Relation,[separate,shared_measure]).

random_direction_fixture([d,0,4]).
random_direction_fixture([b,0,3]).

test(random_direction_explores_both) :-
    getrand(State),
    setup_call_cleanup(true,
        (set_random(seed(42)),
         findall(N, direction_candidate(random,[c,0,4],random_direction_fixture,N), Notes),
         sort(Notes, [[b,0,3],[d,0,4]])),
        setrand(State)).
test(random_direction_changes_with_seed) :-
    getrand(State),
    setup_call_cleanup(true,
        (findall(N, (between(1,20,Seed), set_random(seed(Seed)),
                     once(direction_candidate(random,[c,0,4],random_direction_fixture,N))), Notes),
         sort(Notes, [[b,0,3],[d,0,4]])),
        setrand(State)).
test(third_random_seed_reproducible, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]], getrand(State),
    setup_call_cleanup(true,
        (set_random(seed(7)), once(third_species(major,C,above,A)),
         set_random(seed(7)), once(third_species(major,C,above,B)), A == B),
        setrand(State)).
test(third_randomized_solutions_have_no_duplicates, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]], getrand(State),
    setup_call_cleanup(true,
        (set_random(seed(9)), findnsols(2,P,third_species(major,C,above,P),[A,B]), A \== B),
        setrand(State)).

test(fourth_relaxed_cadence_available, [nondet]) :-
    example_cantus(C),
    once((fourth_species(major,C,above,P),
          append(_,[[Strong,Weak],[Final]],P), is_note(Strong))),
    last(C,[c,0,4]), fourth_score_attacks(C,P,Attacks),
    append(_, [Strong,Weak,Final], Attacks),
    second_cadence([d,0,4],[c,0,4],Weak,Final,[c,0,4]).
test(fourth_original_example_has_five_solutions) :-
    example_cantus(C), findall(P,fourth_species(major,C,above,P),Ps),
    length(Ps,5).
test(fourth_untied_cadence_midi_timing, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    P = [[rest,[c,0,5]],[[g,0,4],[b,0,4]],[[c,0,5]]],
    fourth_species_midi_bytes(C,P,Bytes),
    Bytes = [77,84,104,100,0,0,0,6,0,1,0,2,1,224|Tracks],
    Tracks = [77,84,114,107,0,0,0,31,
        0,144,60,64,143,0,128,60,64,0,144,62,64,143,0,128,62,64,
        0,144,60,64,143,0,128,60,64,0,255,47,0,
        77,84,114,107,0,0,0,41,
        135,64,145,72,64,135,64,129,72,64,
        0,145,67,64,135,64,129,67,64,
        0,145,71,64,135,64,129,71,64,
        0,145,72,64,143,0,129,72,64,0,255,47,0].
test(fourth_untied_cadence_xml_has_no_ties, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    P = [[rest,[c,0,5]],[[g,0,4],[b,0,4]],[[c,0,5]]],
    tmp_file_stream(text,File,Stream), close(Stream),
    setup_call_cleanup(true,
        (write_fourth_species_xml(C,P,above,File,piano),
         load_structure(File,DOM,[dialect(xml),space(remove)]),
         \+ sub_term(element(tie,_,_),DOM),
         findall(T,sub_term(element(type,_,[T]),DOM),
                 [half,half,whole,half,half,whole,whole,whole])),
        delete_file(File)).

test(upper_final_fifth_is_allowed, [nondet]) :- species_distance(end, above, 7).
test(lower_final_fifth_is_not_added, [fail]) :- species_distance(end, below, 7).
test(fifth_cadence_contrary_steps) :-
    second_cadence([d,0,4],[c,0,4],[f,0,4],[g,0,4],[c,0,4]).
test(fifth_cadence_similar_motion_rejected, [fail]) :-
    second_cadence([d,0,4],[c,0,4],[a,0,4],[g,0,4],[c,0,4]).
test(first_species_ending_fifth, [nondet]) :-
    example_cantus(C),
    once((first_species(major,C,above,P),last(P,[g,0,4]))),
    append(_,[[f,0,4],[g,0,4]],P).
test(third_species_ending_fifth, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]], getrand(State),
    setup_call_cleanup(true,
        (set_random(seed(2)),
         once((third_species(major,C,above,P),last(P,[g,0,4]))),
         append(_,[[f,0,4],[g,0,4]],P)),
        setrand(State)).

% The biased direction changes at the target, but both directions remain
% available on backtracking, including when the preferred one has no notes.
test(contour_direction_before_and_after_target) :-
    setup_call_cleanup(nb_setval(musicomp_contour, contour(up,1.0,8,4,random)),
        (contour_directions(7,[up,down]), contour_directions(3,[down,up])),
        nb_delete(musicomp_contour)).
test(contour_down_arc_and_override) :-
    setup_call_cleanup(nb_setval(musicomp_contour, contour(down,1.0,8,4,random)),
        (contour_directions(7,[down,up]), contour_directions(3,[up,down])),
        nb_delete(musicomp_contour)).
test(contour_zero_bias_reverses_first_choice) :-
    setup_call_cleanup(nb_setval(musicomp_contour, contour(up,0.0,8,4,random)),
        contour_directions(7,[down,up]), nb_delete(musicomp_contour)).
test(contour_backtracks_to_other_direction) :-
    setup_call_cleanup(nb_setval(musicomp_contour, contour(up,1.0,8,4,random)),
        (findall(N,contour_candidate(7,[c,0,4],random_direction_fixture,N),Ns),
         Ns = [[d,0,4],[b,0,3]]), nb_delete(musicomp_contour)).
test(contour_bias_out_of_range, [error(domain_error(probability,1.2))]) :-
    species_with_contour(first,major,[[c,0,4],[d,0,4],[c,0,4]],above,1.2,_,_).

test(contour_profile_all_species,
     [forall(member(Species,[first,second,third,fourth,fifth])),nondet]) :-
    combined_sample(Species,C,_), getrand(State),
    setup_call_cleanup(true,
        (set_random(seed(73)), once(species_with_contour(Species,major,C,above,0.75,up,P)),
         counterpoint_extreme_relation(C,Species,P,_)), setrand(State)).
test(contour_seed_reproducible, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]], getrand(State),
    setup_call_cleanup(true,
        (set_random(seed(12)), once(species_with_contour(first,major,C,above,0.6,Curve1,A)),
         set_random(seed(12)), once(species_with_contour(first,major,C,above,0.6,Curve2,B)),
         Curve1 == Curve2, A == B), setrand(State)).
test(contour_context_restored, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    setup_call_cleanup(nb_setval(musicomp_contour, existing),
        (once(species_with_contour(first,major,C,above,0.6,_,_)),
         nb_getval(musicomp_contour,existing)), nb_delete(musicomp_contour)).

test(contour_default_bias_statistical) :-
    getrand(State),
    setup_call_cleanup(
        (set_random(seed(615)), nb_setval(musicomp_contour, contour(up,0.6,8,4,random))),
        (findall(D, (between(1,2000,_), contour_directions(7,[D|_])), Directions),
         include(=(up), Directions, Ups), length(Ups, Count),
         Count >= 1120, Count =< 1280),
        (nb_delete(musicomp_contour), setrand(State))).

test(contour_switches_at_chosen_measure) :-
    setup_call_cleanup(nb_setval(musicomp_contour, contour(up,1.0,8,2,random)),
        (contour_directions(7,[up,down]), contour_directions(6,[down,up])),
        nb_delete(musicomp_contour)).
test(contour_target_maximum_measure) :-
    setup_call_cleanup(nb_setval(musicomp_contour, contour(up,0.6,3,2,random)),
        contour_target_extreme(first, [[c,0,4],[e,0,4],[d,0,4]]),
        nb_delete(musicomp_contour)).
test(contour_target_minimum_measure) :-
    setup_call_cleanup(nb_setval(musicomp_contour, contour(down,0.6,3,2,random)),
        contour_target_extreme(first, [[e,0,4],[c,0,4],[d,0,4]]),
        nb_delete(musicomp_contour)).
test(contour_target_tied_note_counts) :-
    setup_call_cleanup(nb_setval(musicomp_contour, contour(up,0.6,3,2,random)),
        contour_target_extreme(fourth,
            [[rest,[a,0,4]],[tie([a,0,4]),[g,0,4]],[[f,0,4]]]),
        nb_delete(musicomp_contour)).
test(contour_target_outside_nonfinal, [error(domain_error(nonfinal_measure,0))]) :-
    species_with_contour(first,major,[[c,0,4],[d,0,4],[c,0,4]],above,0.6,up,0,_).
test(contour_explicit_target_solution, [nondet]) :-
    example_cantus(C), getrand(State),
    setup_call_cleanup(true,
        (set_random(seed(4)), once(species_with_contour(first,major,C,above,0.6,up,4,P)),
         maplist(note_value,P,Vs), max_list(Vs,Max), nth1(4,Vs,Max)),
        setrand(State)).
test(contour_target_fallback_when_unavailable, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]], getrand(State),
    setup_call_cleanup(true,
        (set_random(seed(10)), once(species_with_contour(first,major,C,above,0.6,up,2,P)),
         maplist(note_value,P,Vs), max_list(Vs,Max), nth1(2,Vs,V), V =\= Max),
        setrand(State)).

test(first_measure_maximum_bias_descends) :-
    setup_call_cleanup(nb_setval(musicomp_contour, contour(up,1.0,8,1,random)),
        (contour_directions(8,[down,up]), contour_directions(5,[down,up])),
        nb_delete(musicomp_contour)).
test(first_measure_minimum_bias_descends) :-
    setup_call_cleanup(nb_setval(musicomp_contour, contour(down,1.0,8,1,random)),
        (contour_directions(8,[down,up]), contour_directions(5,[down,up])),
        nb_delete(musicomp_contour)).
test(first_measure_target_is_allowed, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]], getrand(State),
    setup_call_cleanup(true,
        (set_random(seed(12)), once(species_with_contour(first,major,C,above,0.6,up,1,P)),
         maplist(note_value,P,Values), max_list(Values,Max), Values = [Max|_]),
        setrand(State)).
test(final_measure_target_rejected, [error(domain_error(nonfinal_measure,3))]) :-
    species_with_contour(first,major,[[c,0,4],[d,0,4],[c,0,4]],above,0.6,up,3,_).

test(combined_single_staff_attributes_once,
     [forall(member(Species,[first,second,third,fourth,fifth])),nondet]) :-
    combined_sample(Species,C,P), tmp_file_stream(text,File,Stream), close(Stream),
    setup_call_cleanup(true,
        (write_solutions_musicxml(C,Species,above,[P,P],File,single),
         load_structure(File,DOM,[dialect(xml),space(remove)]),
         findall(c,sub_term(element(clef,_,_),DOM),[c]),
         findall(t,sub_term(element(time,_,_),DOM),[t]),
         findall(d,sub_term(element(divisions,_,_),DOM),[d]),
         findall(w,sub_term(element(words,_,_),DOM),[w,w])),
        delete_file(File)).

test(combined_retains_attribute_changes) :-
    A = element(attributes,[],[element(divisions,[],['1'])]),
    B = element(attributes,[],[element(divisions,[],['2'])]),
    Measures = [element(measure,[],[A]),element(measure,[],[A]),
                element(measure,[],[B]),element(measure,[],[B]),
                element(measure,[],[A])],
    musicxml:remove_redundant_score_attributes(Measures,none,Clean,_),
    Clean = [element(measure,[],[A]),element(measure,[],[]),
             element(measure,[],[B]),element(measure,[],[]),
             element(measure,[],[A])].

% A multi-solution third-species query should sample more than the first
% randomly selected contour, while never returning duplicate melodies.
contour_extrema_order(Notes, Order) :-
    maplist(note_value,Notes,Values), min_list(Values,Min), max_list(Values,Max),
    nth1(LowIndex,Values,Min), nth1(HighIndex,Values,Max),
    ( LowIndex < HighIndex -> Order = low_first ; Order = high_first ).

test(third_multi_solution_contour_diversity, [nondet]) :-
    example_cantus(C), getrand(State),
    setup_call_cleanup(true,
        (set_random(seed(7)),
         findnsols(10, P, third_species(major,C,above,P), Solutions),
         length(Solutions,10), sort(Solutions, Unique), length(Unique,10),
         maplist(check_third_measures(C),Solutions),
         member(Down,Solutions), contour_extrema_order(Down,low_first),
         member(Up,Solutions), contour_extrema_order(Up,high_first)),
        setrand(State)).

test(third_incremental_bounds_matches_range) :-
    extend_counterpoint_bounds(bounds(48,53,none), [g,0,4], bounds(48,55,none)),
    \+ extend_counterpoint_bounds(bounds(48,53,none), [f,0,5], _).

test(third_target_prunes_ineligible_prefix, [fail]) :-
    setup_call_cleanup(nb_setval(musicomp_contour,contour(up,0.6,8,2,random)),
        third_target_bounds(random,7,[[c,0,4],[d,0,4],[e,0,4],[f,0,4]],
                            bounds(48,55,none),_), nb_delete(musicomp_contour)).

test(third_target_bounds_prune_later_extreme) :-
    setup_call_cleanup(nb_setval(musicomp_contour,contour(up,0.6,8,2,random)),
        (third_target_bounds(random,7,[[c,0,4],[d,0,4],[e,0,4],[f,0,4]],
                             bounds(48,53,none),Bound),
         extend_counterpoint_bounds(Bound,[e,0,4],_),
         \+ extend_counterpoint_bounds(Bound,[g,0,4],_)),
        nb_delete(musicomp_contour)).

test(third_target_pruning_disabled_for_ordered_fallback) :-
    setup_call_cleanup(nb_setval(musicomp_contour,contour(up,0.6,8,2,random)),
        third_target_bounds(ordered,7,[[c,0,4],[d,0,4],[e,0,4],[f,0,4]],
                            bounds(48,55,none),bounds(48,55,none)),
        nb_delete(musicomp_contour)).

test(third_direction_optimization_preserves_candidates) :-
    C = [c,0,4], Root = [c,0,4], Prev = [e,0,4],
    findall(N,direction_candidate(ordered,Prev,
            third_note_candidate(C,Root,major,above,middle,Prev),N),Before),
    findall(N,third_direction_candidate(ordered,C,Root,major,above,middle,Prev,N),After),
    Before == After.

% System breaks use absolute measure numbers across solution boundaries.
test(combined_system_breaks_after_eight,
     [forall(member(Layout,[piano,single])),nondet]) :-
    example_cantus(C), length(P,29), maplist(=([g,0,4]),P),
    tmp_file_stream(text,File,Stream), close(Stream),
    setup_call_cleanup(true,
        (write_solutions_musicxml(C,third,above,[P,P,P],File,Layout),
         load_structure(File,DOM,[dialect(xml),space(remove)]),
         findall(Number,
             (sub_term(element(measure,[number=Number],Children),DOM),
              member(element(print,['new-system'=yes],[]),Children)),
             ['9','17']),
         sub_term(element(measure,[number='9'],[element(print,['new-system'=yes],[])|_]),DOM),
         findall(t,sub_term(element(time,_,_),DOM),[t]),
         findall(L,sub_term(element(words,_,[L]),DOM),['Solution 1','Solution 2','Solution 3'])),
        delete_file(File)).

test(combined_breaks_inside_solution_keep_ties, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    P = [[rest,[c,0,5]],[tie([c,0,5]),[b,0,4]],[[c,0,5]]],
    tmp_file_stream(text,File,Stream), close(Stream),
    setup_call_cleanup(true,
        (write_solutions_musicxml(C,fourth,above,[P,P,P,P,P,P],File,piano),
         load_structure(File,DOM,[dialect(xml),space(remove)]),
         findall(N,(sub_term(element(measure,[number=N],Children),DOM),
                    member(element(print,['new-system'=yes],[]),Children)),['9','17']),
         findall(T,sub_term(element(tie,[type=T],[]),DOM),Ties),
         include(=(start),Ties,Starts),include(=(stop),Ties,Stops),
         length(Starts,6),length(Stops,6)),
        delete_file(File)).

write_single_guitar(first,C,P,File) :- write_counterpoint_xml(P,C,File,guitar).
write_single_guitar(second,C,P,File) :- write_second_species_xml(C,P,above,File,guitar).
write_single_guitar(third,C,P,File) :- write_third_species_xml(C,P,above,File,guitar).
write_single_guitar(fourth,C,P,File) :- write_fourth_species_xml(C,P,above,File,guitar).
write_single_guitar(fifth,C,P,File) :- write_fifth_species_xml(C,P,above,File,guitar).

guitar_score_structure(DOM) :-
    sub_term(element('part-name',_,['Guitar']),DOM),
    findall(X,sub_term(element('clef-octave-change',_,[X]),DOM),['-1']),
    \+ sub_term(element(transpose,_,_),DOM),
    \+ sub_term(element(staves,_,_),DOM),
    sub_term(element(voice,_,['1']),DOM), sub_term(element(voice,_,['2']),DOM),
    % Input C4 sounds as C3; MuseScore renders it at written C4.
    sub_term(element(note,_,Fields),DOM),
    member(element(voice,_,['2']),Fields),
    member(element(pitch,_,Pitch),Fields),
    member(element(step,_,['C']),Pitch), member(element(octave,_,['3']),Pitch), !.

test(guitar_standalone_all_species,
     [forall(member(Species,[first,second,third,fourth,fifth])),nondet]) :-
    combined_sample(Species,C,P), tmp_file_stream(text,File,Stream), close(Stream),
    setup_call_cleanup(true,
        (once(write_single_guitar(Species,C,P,File)),
         load_structure(File,DOM,[dialect(xml),space(remove)]), guitar_score_structure(DOM)),
        delete_file(File)).

test(guitar_combined_all_species,
     [forall(member(Species,[first,second,third,fourth,fifth])),nondet]) :-
    combined_sample(Species,C,P), tmp_file_stream(text,File,Stream), close(Stream),
    setup_call_cleanup(true,
        (write_solutions_musicxml(C,Species,above,[P,P],File,guitar),
         load_structure(File,DOM,[dialect(xml),space(remove)]), guitar_score_structure(DOM),
         findall(L,sub_term(element(words,_,[L]),DOM),['Solution 1','Solution 2'])),
        delete_file(File)).

test(guitar_ties_retain_pitch, [nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]],
    P = [[rest,[c,0,5]],[tie([c,0,5]),[b,0,4]],[[c,0,5]]],
    tmp_file_stream(text,File,Stream),close(Stream),
    setup_call_cleanup(true,
        (write_fourth_species_xml(C,P,above,File,guitar),
         load_structure(File,DOM,[dialect(xml),space(remove)]),
         findall(O,(sub_term(element(note,_,Fields),DOM),
                    member(element(tie,[type=_],[]),Fields),
                    member(element(pitch,_,Pitch),Fields),
                    member(element(octave,_,[O]),Pitch)),['4','4'])),
        delete_file(File)).

test(guitar_valid_high_octave_export, [nondet]) :-
    tmp_file_stream(text,File,Stream),close(Stream),
    setup_call_cleanup(true,
        (write_counterpoint_xml([[c,0,9]],[[c,0,4]],File,guitar),
         load_structure(File,DOM,[dialect(xml),space(remove)]),
         sub_term(element(octave,_,['8']),DOM)), delete_file(File)).

test(guitar_octave_zero_rejected, [fail]) :-
    write_counterpoint_xml([[a,0,0]],[[c,0,4]],'/tmp/invalid-guitar-low.xml',guitar).

test(guitar_octave_written_once, [nondet]) :-
    tmp_file_stream(text,File,Stream),close(Stream),
    setup_call_cleanup(true,
        (write_counterpoint_xml([[g,0,4]],[[c,0,4]],File,guitar),
         load_structure(File,DOM,[dialect(xml),space(remove)]),
         \+ sub_term(element(transpose,_,_),DOM),
         sub_term(element('clef-octave-change',_,['-1']),DOM),
         findall(S-O,(sub_term(element(pitch,_,Pitch),DOM),
                      member(element(step,_,[S]),Pitch),
                      member(element(octave,_,[O]),Pitch)),['G'-'3','C'-'3'])),
        delete_file(File)).

test(third_beat_three_consonant) :-
    third_accent_rule([c,0,4],[d,0,4],[e,0,4],[f,0,4]).
test(third_beat_three_dissonant_stepwise_allowed, [nondet]) :-
    third_accent_rule([c,0,4],[e,0,4],[f,0,4],[g,0,4]),
    third_dissonance_rule([c,0,4],[e,0,4],[f,0,4],[g,0,4]).
test(third_beat_three_dissonant_neighbor_allowed) :-
    third_accent_rule([c,0,4],[e,0,4],[f,0,4],[e,0,4]),
    third_dissonance_rule([c,0,4],[e,0,4],[f,0,4],[e,0,4]).
test(third_beat_three_dissonant_beat_two_rejected, [fail]) :-
    third_accent_rule([c,0,4],[d,0,4],[f,0,4],[g,0,4]).
test(third_beat_three_dissonant_beat_four_rejected, [fail]) :-
    third_accent_rule([c,0,4],[e,0,4],[f,0,4],[f,1,4]).
test(third_beat_three_dissonant_leap_rejected, [fail]) :-
    third_accent_rule([c,0,4],[g,0,4],[d,0,4],[e,0,4]).
test(third_beat_three_dissonant_below_cantus, [nondet]) :-
    third_accent_rule([c,0,5],[g,1,4],[g,0,4],[f,0,4]),
    third_dissonance_rule([c,0,5],[g,1,4],[g,0,4],[f,0,4]).

% Opening draws use 30/30/40 weights; actual returned frequencies may differ
% because a preferred opening can be unavailable or already returned.
test(opening_draw_weights) :-
    getrand(State),
    setup_call_cleanup(set_random(seed(315)),
        (findall(D,(between(1,10000,_),weighted_opening(D)),Draws),
         include(=(0),Draws,Unisons), length(Unisons,U), U >= 2850, U =< 3150,
         include(=(7),Draws,Fifths), length(Fifths,F), F >= 2850, F =< 3150,
         include(=(12),Draws,Octaves), length(Octaves,O), O >= 3850, O =< 4150),
        setrand(State)).

test(weighted_opening_is_tried_first) :-
    setup_call_cleanup(
        (nb_setval(musicomp_opening,7),nb_setval(musicomp_contour,contour(up,0.6,8,4,random))),
        (findall(D,species_distance(start,above,D),[7,0,12]),
         findall(D,species_distance(start,below,D),[7,0,12])),
        (nb_delete(musicomp_opening),nb_delete(musicomp_contour))).

test(weighted_opening_stream_renormalizes_available_choices) :-
    getrand(State),
    setup_call_cleanup(set_random(seed(17)),
        (findall(D,(between(1,100,_),select_weighted_opening(
                     [opening(0,a),opening(12,b)],opening(D,_))),Draws),
         memberchk(0,Draws),memberchk(12,Draws),\+ memberchk(7,Draws)),
        setrand(State)).

test(weighted_opening_fallback_preserves_all_solutions,
     [forall(member(Species,[first,second,fourth])),nondet]) :-
    C = [[c,0,4],[d,0,4],[c,0,4]], getrand(State),
    setup_call_cleanup(true,
        (contour_base(Species,ordered,major,C,above,Base),
         findall(P,prefer_voice_extremes(Base,C,Species,P),Old),sort(Old,Expected),
         set_random(seed(55)),
         findall(P,weighted_ordered_solutions(Species,major,C,above,P),New),
         sort(New,Actual), Expected == Actual),
        setrand(State)).

test(multiple_solutions_have_fresh_opening_choices, [nondet]) :-
    example_cantus(C), getrand(State),
    setup_call_cleanup(true,
        (set_random(seed(7)),
         findnsols(10,P,third_species(major,C,above,P),Solutions),
         length(Solutions,10),sort(Solutions,Unique),length(Unique,10),
         findall(D,(member([First|_],Solutions),interval([c,0,4],First,D)),Intervals),
         sort(Intervals,[0,7,12])),
        setrand(State)).
:- end_tests(musicomp).
