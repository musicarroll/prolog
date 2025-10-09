:- begin_tests(multisets).

:- use_module('../lib/universe').
:- use_module('../lib/multiset_operations').
:- use_module('../lib/element_operations').

% Updated tests for disjoint polarity universe (a+, b+, c+, a-, b-, c-)

% Test cases for mset_normalize/2
test(normalize_partial) :-
    mset_normalize([[3, a+]], [[3, a+], [0, b+], [0, c+], [0, a-], [0, b-], [0, c-]]).
test(normalize_empty) :-
    mset_normalize([], [[0, a+], [0, b+], [0, c+], [0, a-], [0, b-], [0, c-]]).

% Test cases for melintersect/3
test(melintersect_positive) :-
    melintersect([3, a+], [2, a+], [2, a+]).
test(melintersect_negative) :-
    melintersect([3, a-], [2, a-], [3, a-]).

% Test cases for melunion/3
test(melunion_positive) :-
    melunion([3, a+], [2, a+], [3, a+]).
test(melunion_negative) :-
    melunion([3, a-], [2, a-], [2, a-]).

% Test cases for melcomplement/2
test(melcomplement_positive) :-
    melcomplement([3, a+], [3, a-]).
test(melcomplement_negative) :-
    melcomplement([3, a-], [3, a+]).

% Test cases for melcomplement_list/2
test(melcomplement_list_partial) :-
    melcomplement_list([[2, a+], [1, b-]], [[2, a-], [1, b+]]).

test(melcomplement_list_full) :-
    melcomplement_list([[2, a+], [1, b-], [0, c+]], [[2, a-], [1, b+], [0, c-]]).

% Test cases for full_bit_vector/2
test(full_bit_vector_partial) :-
    full_bit_vector([[2, a+]], [[2, a+], [0, b+], [0, c+], [0, a-], [0, b-], [0, c-]]).
test(full_bit_vector_empty) :-
    full_bit_vector([], [[0, a+], [0, b+], [0, c+], [0, a-], [0, b-], [0, c-]]).

% Test cases for mset_intersect/3
test(mset_intersect_partial) :-
    mset_intersect([[2, a+], [1, b-]], [[1, a+], [3, b-]], [[1, a+], [3, b-], [0, c+], [0, a-], [0, b+], [0, c-]]).

test(mset_intersect_full) :-
    mset_intersect([[2, a+], [1, b+], [1, a-], [0, b-], [0, c+], [0, c-]],
                   [[3, a+], [0, b+], [2, a-], [1, b-], [0, c+], [0, c-]],
                   [[2, a+], [0, b+], [1, a-], [1, b-], [0, c+], [0, c-]]).

% Test cases for mset_union/3
test(mset_union_partial) :-
    mset_union([[2, a+], [1, b-]], [[1, a+], [3, b-]], [[2, a+], [1, b-], [0, b+], [0, c+], [0, a-], [0, c-]]).

test(mset_union_full) :-
    mset_union([[2, a+], [1, b+], [1, a-], [0, b-], [0, c+], [0, c-]],
               [[3, a+], [0, b+], [2, a-], [1, b-], [0, c+], [0, c-]],
               [[3, a+], [1, b+], [1, a-], [0, b-], [0, c+], [0, c-]]).

:- end_tests(multisets).
