:-use_module(library(plunit)).
:-use_module('binops.pro').
:-use_module('functions.pro').

:- begin_tests(powerset).

test(empty_set, [true(PowerSet == [[]])]) :-
    functions:powerset([], PowerSet).

test(single_element, [true(PowerSet == [[], [a]])]) :-
    functions:powerset([a], PowerSet).

test(multi_elements, [true(PowerSet == [[], [a], [b], [a, b], [c], [a, c], [b, c], [a, b, c]])]) :-
    functions:powerset([a, b, c], PowerSet).

test(includes_empty_set, [true(Member == [])]) :-
    functions:powerset([a, b, c], PowerSet),
    member(Member, PowerSet),
    !.

test(includes_full_set) :-
        Set = [a, b, c],
        powerset(Set, PowerSet),
        member(Set, PowerSet),  % This checks for the presence of the full set within the power set.
        !.  % Adding cut to address potential choicepoint warning
       
test(correct_number_of_subsets, [true(Length == 8)]) :-
    functions:powerset([a, b, c], PowerSet),
    length(PowerSet, Length).

:- end_tests(powerset).
