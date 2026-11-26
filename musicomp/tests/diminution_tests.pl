:- begin_tests(diminution).
:- ensure_loaded('../musicomp.pl').
:- use_module(library(sgml)).

sample_line([[c,0,4],[b,0,3],[c,0,4],[d,0,4],
             [c,0,4],[e,0,4],[d,0,4],[c,0,4]]).

check_anchor(Pitch, [note(Pitch,_,_)|_]).

test(variations_preserve_anchors_and_duration,
     [forall(member(Mode,[halves,quarters,syncopated,mixed])),nondet]) :-
    sample_line(Line), once(diminution(major,[c,0,4],Line,Mode,Measures)),
    same_length(Line,Measures), maplist(check_anchor,Line,Measures),
    valid_diminution_measures(Measures),
    diminution_attacks(Measures, Attacks),
    forall(member(N,Attacks), (note_value(N,V), V >= 47, V =< 52)).

test(all_modes_reach_final_pitch_by_step,
     [forall(member(Mode,[halves,quarters,syncopated,mixed])),nondet]) :-
    sample_line(Line), once(diminution(major,[c,0,4],Line,Mode,Measures)),
    diminution_attacks(Measures, Attacks),
    append(_, [Previous,Final], Attacks),
    interval(Previous,Final,Distance), memberchk(Distance,[1,2]).

test(final_tie_is_not_a_new_attack, [nondet]) :-
    Measures = [[note([d,0,4],half,none),note([c,0,4],half,start)],
                [note([c,0,4],whole,stop)]],
    diminution_attacks(Measures,Attacks),
    Attacks = [[d,0,4],[c,0,4]], last_preceded_by_second(Attacks).

test(repeated_pitch_does_not_count_as_step, [fail]) :-
    Measures = [[note([d,0,4],half,none),note([c,0,4],half,none)],
                [note([c,0,4],whole,none)]],
    diminution_attacks(Measures,Attacks), last_preceded_by_second(Attacks).

test(dorian_diminution, [nondet]) :-
    Line = [[d,0,4],[c,0,4],[d,0,4],[e,0,4],
            [d,0,4],[f,0,4],[e,0,4],[d,0,4]],
    once(diminution(dorian,[d,0,4],Line,quarters,Measures)),
    valid_diminution_measures(Measures),
    diminution_attacks(Measures,Notes),
    forall(member(N,Notes), in_scale(N,[d,0,4],dorian)).

test(syncopation_includes_ties_and_ordinary_half_notes, [nondet]) :-
    sample_line(Line), once(diminution(major,[c,0,4],Line,syncopated,Measures)),
    member(M,Measures), member(note(_,half,start),M),
    member([_,note(_,half,none)],Measures).

test(mixed_can_combine_ties_and_quarters, [nondet]) :-
    sample_line(Line),
    once(diminution(major,[c,0,4],Line,mixed,Measures)),
    member(Bar,Measures), member(note(_,_,start),Bar),
    member(Other,Measures), member(note(_,quarter,none),Other),
    valid_diminution_measures(Measures).

test(mixed_patterns_and_eighths) :-
    diminution_durations(half_quarters,[half,quarter,quarter]),
    diminution_durations(half_eighths,[half,quarter,eighth,eighth]).

test(no_upward_leap_from_quarter_downbeat, [nondet]) :-
    sample_line(Line), once(diminution(major,[c,0,4],Line,quarters,Measures)),
    forall((member([note(A,quarter,_),note(B,quarter,_)|_],Measures)),
           (note_value(A,X),note_value(B,Y),Y-X =< 2)).

test(two_note_motif_repeated_within_measure, [fail]) :-
    check_repeated_two_note_patterns([[c,0,4],[b,0,3],[c,0,4],[b,0,3]]).
test(two_note_motif_repeated_over_barline, [fail]) :-
    diminution_recent_notes([[c,0,4],[d,0,4],[e,0,4]], History),
    append(History,[[d,0,4],[e,0,4]], Window),
    check_repeated_two_note_patterns(Window).
test(successive_transposed_quarter_patterns_rejected, [fail]) :-
    diminution_different_quarter_patterns(
        [[note([c,0,4],quarter,none),note([d,0,4],quarter,none),
          note([e,0,4],quarter,none),note([f,0,4],quarter,none)],
         [note([d,0,4],quarter,none),note([e,0,4],quarter,none),
          note([f,0,4],quarter,none),note([g,0,4],quarter,none)]]).
test(same_quarter_measure_repeated_later_rejected, [fail]) :-
    M = [note([c,0,4],quarter,none),note([d,0,4],quarter,none),
         note([e,0,4],quarter,none),note([f,0,4],quarter,none)],
    diminution_unique_quarter_measures([M,[note([c,0,4],half,none)],M]).
test(sample_quarter_variation_has_no_repeated_patterns, [nondet]) :-
    sample_line(Line), once(diminution(major,[c,0,4],Line,quarters,Measures)),
    diminution_attacks(Measures,Attacks),
    check_repeated_two_note_patterns(Attacks),
    diminution_different_quarter_patterns(Measures),
    diminution_unique_quarter_measures(Measures).

test(triad_arpeggiation_rejected, [fail]) :-
    diminution_no_triad([[c,0,4],[e,0,4],[g,0,4]]).
test(mismatched_tie_rejected, [fail]) :-
    valid_diminution_measures([[note([c,0,4],half,none),note([d,0,4],half,start)],
                               [note([e,0,4],whole,stop)]]).
test(incomplete_measure_rejected, [fail]) :-
    valid_diminution_measures([[note([c,0,4],quarter,none)]]).

test(tied_midi_is_one_attack, [nondet]) :-
    Measures = [[note([c,0,4],half,none),note([d,0,4],half,start)],
                [note([d,0,4],whole,stop)]],
    append(Measures,Events), diminution_midi_events(Events,Bytes),
    Bytes = [0,144,60,64,135,64,128,60,64,
             0,144,62,64,150,64,128,62,64].

test(xml_eighths_and_ties, [nondet]) :-
    Measures = [[note([c,0,4],half,none),note([d,0,4],quarter,none),
                 note([e,0,4],eighth,none),note([d,0,4],eighth,start)],
                [note([d,0,4],whole,stop)]],
    tmp_file_stream(text,File,Stream), close(Stream),
    setup_call_cleanup(true,
        (write_diminution_xml(Measures,File,guitar),
         load_structure(File,Doc,[dialect(xml),space(remove)]),
         sub_term(element(divisions,_,['2']),Doc),
         sub_term(element(type,_,[eighth]),Doc),
         sub_term(element('clef-octave-change',_,['-1']),Doc),
         sub_term(element(tie,[type=start],_),Doc),
         sub_term(element(tie,[type=stop],_),Doc)),
        delete_file(File)).
test(combined_solutions_xml_preserves_notation, [nondet]) :-
    A = [[note([c,0,4],half,none),note([d,0,4],quarter,none),
          note([e,0,4],eighth,none),note([d,0,4],eighth,start)],
         [note([d,0,4],whole,stop)]],
    B = [[note([e,0,4],whole,none)]],
    tmp_file_stream(text,File,Stream), close(Stream),
    setup_call_cleanup(true,
        (write_diminution_solutions_xml([A,B],File,guitar,'A & B'),
         load_structure(File,Doc,[dialect(xml),space(remove)]),
         findall(N,sub_term(element(measure,[number=N],_),Doc),Numbers),
         Numbers == ['1','2','3'],
         findall(T,sub_term(element(words,_,[T]),Doc),Labels),
         Labels == ['Solution 1','Solution 2'],
         findall(Attrs,sub_term(element(attributes,_,Attrs),Doc),Attributes),
         length(Attributes,1),
         sub_term(element('work-title',_,['A & B']),Doc),
         sub_term(element('clef-octave-change',_,['-1']),Doc),
         sub_term(element(type,_,[eighth]),Doc),
         sub_term(element(tie,[type=start],_),Doc),
         sub_term(element(tie,[type=stop],_),Doc),
         findall(Style,sub_term(element('bar-style',_,[Style]),Doc),Styles),
         Styles == ['light-light','light-light']),
        delete_file(File)).

test(combined_breaks_follow_global_measure_numbers, [nondet]) :-
    length(Seven,7), maplist(=([note([c,0,4],whole,none)]),Seven),
    length(Four,4), maplist(=([note([d,0,4],whole,none)]),Four),
    tmp_file_stream(text,File,Stream), close(Stream),
    setup_call_cleanup(true,
        (write_diminution_solutions_xml([Seven,Four],File,single),
         load_structure(File,Doc,[dialect(xml),space(remove)]),
         findall(N,(sub_term(element(measure,[number=N],Children),Doc),
                    member(element(print,['new-system'=yes],_),Children)),Breaks),
         Breaks == ['9']), delete_file(File)).

test(combined_invalid_later_solution_does_not_overwrite, [nondet]) :-
    tmp_file_stream(text,File,Stream), write(Stream,untouched), close(Stream),
    setup_call_cleanup(true,
        (Valid = [[note([c,0,4],whole,none)]],
         Invalid = [[note([d,0,4],quarter,none)]],
         \+ write_diminution_solutions_xml([Valid,Invalid],File,single),
         read_file_to_string(File,Text,[]), Text == "untouched"),
        delete_file(File)).

test(combined_empty_solutions_fail, [fail]) :-
    write_diminution_solutions_xml([], '/tmp/unused-diminution.xml', single).

:- end_tests(diminution).
