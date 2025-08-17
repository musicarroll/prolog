:- begin_tests(multisets).

:- use_module('../lib/universe').
:- use_module('../lib/multiset_operations').
:- use_module('../lib/element_operations').

% Test cases for mset_normalize/2
test(normalize_partial) :-
    mset_normalize([[3, a]], [[3, a], [0, b], [0, c]]), !.
test(normalize_empty) :-
    mset_normalize([], [[0, a], [0, b], [0, c]]).

% Test cases for melintersect/3
test(melintersect_positive) :-
    melintersect([3, a], [2, a], [2, a]), !.

test(melintersect_negative) :-
    melintersect([-3, a], [-2, a], [-2, a]).

test(melintersect_mixed) :-
    melintersect([3, a], [-2, a], [-2, a]).

% Test cases for melunion/3
test(melunion_positive) :-
    melunion([3, a], [2, a], [3, a]).

test(melunion_negative) :-
    melunion([-3, a], [-2, a], [-3, a]).

test(melunion_mixed) :-
    melunion([3, a], [-2, a], [3, a]).

% Test cases for melcomplement/2
test(melcomplement_positive) :-
    melcomplement([3, a], [-3, a]).

test(melcomplement_negative) :-
    melcomplement([-3, a], [3, a]).

test(melcomplement_zero) :-
    melcomplement([0, a], [0, a]).

% Test cases for mset_intersect/3
test(mset_intersect_partial) :-
    mset_intersect([[3, a], [1, b]], [[2, a], [-1, c]], [[2, a], [0, b], [0, c]]).

test(mset_intersect_full) :-
    mset_intersect([[3, a], [1, b], [0, c]], [[2, a], [-1, b], [0, c]], [[2, a], [-1, b], [0, c]]).

% Test cases for mset_union/3
% test(mset_union_partial) :-
%     mset_union([[3, a], [1, b]], [[2, a], [-1, c]], [[3, a], [1, b], [0, c]]).
test(mset_union_partial) :-
    mset_union([[3, a], [1, b]], [[2, a], [-1, c]], [[3, a], [1, b], [-1, c]]), !.
test(mset_union_full) :-
    mset_union([[3, a], [1, b], [0, c]], [[2, a], [-1, b], [0, c]], [[3, a], [1, b], [0, c]]).

% Test cases for melcomplement_list/2
test(melcomplement_list_partial) :-
    melcomplement_list([[3, a], [-1, b]], [[-3, a], [1, b]]).

test(melcomplement_list_full) :-
    melcomplement_list([[3, a], [-1, b], [0, c]], [[-3, a], [1, b], [0, c]]).

% Test cases for full_bit_vector/2
test(full_bit_vector_partial) :-
    full_bit_vector([[3, a]], [[3, a], [0, b], [0, c]]).

test(full_bit_vector_empty) :-
    full_bit_vector([], [[0, a], [0, b], [0, c]]).


test(melrelative_complement_positive) :-
    melrelative_complement([3, a], [2, a], [-2, a]).
    
test(melrelative_complement_negative) :-
    melrelative_complement([-3, a], [2, a], [-2, a]).

test(melrelative_complement_mixed) :-
    melrelative_complement([3, a], [-2, a], [2, a]).
    
test(melsymmetric_difference_positive) :-
    melsymmetric_difference([3, a], [2, a], [-3, a]).

test(melsymmetric_difference_negative) :-
    melsymmetric_difference([-3, a], [2, a], [-2, a]).
% Mset-Level Tests

% Test for mset_union/3
test(mset_union_partial_superset) :-
    mset_union([[3, a], [1, b]], [[2, a], [-1, b]], [[3, a], [1, b], [0, c]]).

% Test for mset_relative_complement/3
test(mset_relative_complement_partial) :-
    mset_relative_complement([[3, a], [1, b]], [[2, a], [-1, b]], [[-2, a], [1, b], [0, c]]).

test(mset_relative_complement_full) :-
    mset_relative_complement([[3, a], [1, b], [0, c]], [[2, a], [-1, b], [4, c]], [[-2, a], [1, b], [0, c]]).

% Test for mset_symmetric_difference/3
test(mset_symmetric_difference_partial) :-
    mset_symmetric_difference([[3, a], [1, b]], [[2, a], [-1, b]], [[-3, a], [1, b], [0, c]]).

test(mset_symmetric_difference_full) :-
    mset_symmetric_difference([[3, a], [1, b], [0, c]], [[2, a], [-1, b], [4, c]], [[-3, a], [1, b], [0, c]]).

% Subset relation for full sets
test(mset_subset_full) :-
    mset_subset([[1, a], [0, b], [0, c]], [[2, a], [1, b], [0, c]]).

% Subset relation for partial sets
test(mset_subset_partial) :-
    mset_subset([[1, a]], [[2, a], [1, b]]).

% Not a subset
test(mset_not_subset) :-
    \+ mset_subset([[1, a], [1, c]], [[2, a], [1, b]]).

% Equal sets
test(mset_subset_equal) :-
    mset_subset([[1, a], [0, b], [0, c]], [[1, a], [0, b], [0, c]]).

% Mixed set with positive and negative multiplicities
test(all_msubsets_mixed) :-
    all_msubsets([[2, a], [-1, b], [0, c]], [[[0,a],[-1,b],[0,c]],[[0,a],[0,b],[0,c]],[[1,a],[-1,b],[0,c]],[[1,a],[0,b],[0,c]],[[2,a],[-1,b],[0,c]],[[2,a],[0,b],[0,c]]]).

% Edge case: Multiset containing only positive elements
test(all_msubsets_positive) :-
    all_msubsets([[1, a], [2, b]], [[[0,a],[0,b]],[[0,a],[1,b]],[[0,a],[2,b]],[[1,a],[0,b]],[[1,a],[1,b]],[[1,a],[2,b]]]).

% Edge case: Multiset containing only negative elements
test(all_msubsets_negative) :-
    all_msubsets([[-1, a], [-2, b]], [[[-1,a],[-2,b]],[[-1,a],[-1,b]],[[-1,a],[0,b]],[[0,a],[-2,b]],[[0,a],[-1,b]],[[0,a],[0,b]]]).

% Test for melintersect_alt/3
test(melintersect_alt_positive_negative) :-
    melintersect_alt([2, a], [-1, a], [1, a]).
test(melintersect_alt_negative_positive) :-
    melintersect_alt([-3, b], [2, b], [-1, b]).
test(melintersect_alt_positive_positive) :-
    melintersect_alt([2, a], [3, a], [2, a]).
test(melintersect_alt_negative_negative) :-
    melintersect_alt([-2, b], [-3, b], [-2, b]).
test(melintersect_alt_zero_case) :-
    melintersect_alt([0, a], [3, a], [0, a]).

% Test for melunion_alt/3
test(melunion_alt_positive_negative) :-
    melunion_alt([2, a], [-1, a], [1, a]).
test(melunion_alt_negative_positive) :-
    melunion_alt([-3, b], [2, b], [-1, b]).
test(melunion_alt_positive_positive) :-
    melunion_alt([2, a], [3, a], [3, a]).
test(melunion_alt_negative_negative) :-
    melunion_alt([-2, b], [-3, b], [-3, b]).
test(melunion_alt_zero_case) :-
    melunion_alt([0, a], [-3, a], [-3, a]).

% Test for mset_intersect_alt/3
test(mset_intersect_alt_mixed) :-
    mset_intersect_alt([[2, a], [-3, b], [0, c]], [[-1, a], [2, b], [0, c]], [[1, a], [-1, b], [0, c]]).
test(mset_intersect_alt_positive) :-
    mset_intersect_alt([[3, a], [1, b]], [[2, a], [1, b]], [[2, a], [1, b], [0, c]]).
test(mset_intersect_alt_negative) :-
    mset_intersect_alt([[-2, a], [-1, b]], [[-3, a], [-1, b]], [[-2, a], [-1, b], [0, c]]).

% Test for mset_union_alt/3
test(mset_union_alt_mixed) :-
    mset_union_alt([[2, a], [-3, b], [0, c]], [[-1, a], [2, b], [0, c]], [[1, a], [-1, b], [0, c]]).
test(mset_union_alt_positive) :-
    mset_union_alt([[3, a], [1, b]], [[2, a], [1, b]], [[3, a], [1, b], [0, c]]).
test(mset_union_alt_negative) :-
    mset_union_alt([[-2, a], [-1, b]], [[-3, a], [-1, b]], [[-3, a], [-1, b], [0, c]]).

% Test for mset_relative_complement_alt/3
test(mset_relative_complement_alt) :-
    mset_relative_complement_alt([[3, a], [-3, b], [0, c]], [[2, a], [-1, b], [0, c]], [[1, a], [-2, b], [0, c]]).

% Test for mset_symmetric_difference_alt/3
test(mset_symmetric_difference_alt) :-
    mset_symmetric_difference_alt([[3, a], [-3, b], [0, c]], [[2, a], [-1, b], [0, c]], [[0, a], [0, b], [0, c]]).

 test(mset_alt_union_preserves_superset) :-
mset_subset_alt([[2, a], [-1, b], [0, c]], [[3, a], [-1, b], [0, c]]),
mset_union_alt([[2, a], [-1, b], [0, c]], [[3, a], [-1, b], [0, c]], [[3, a], [-1, b], [0, c]]).

test(melunion_alt_mixed) :-
    melunion_alt([2, a], [-1, a], [1, a]),
    melunion_alt([-3, a], [-2, a], [-3, a]).
test(melunion_alt_positive) :-
    melunion_alt([2, a], [3, a], [3, a]).
test(melunion_alt_negative) :-
    melunion_alt([-2, a], [-3, a], [-3, a]).
    

:- end_tests(multisets).


:- begin_tests(melcomplement_with_max).

:- use_module('../lib/element_operations',
    [melintersect/3, melunion/3, 
    melintersect_alt/3, melunion_alt/3,
    melcomplement/2, 
    melcomplement_with_max/3,
    melrelative_complement/3,
    melrelative_complement_alt/3,
    melrelative_complement_with_max/4,
    melsymmetric_difference/3, 
    melsymmetric_difference_alt/3
    ]).

test(melcomplement_with_max_positive) :-
    melcomplement_with_max([2, a], R, 3),
    R == [-1, a].

test(melcomplement_with_max_negative) :-
    melcomplement_with_max([-1, b], R, 3),
    R == [2, b].

test(melcomplement_with_max_zero) :-
    melcomplement_with_max([0, c], R, 3),
    R == [0, c].

test(melrelative_complement_with_max_positive) :-
    melrelative_complement_with_max([2, a], [1, a], [0, a], 3).

test(melrelative_complement_with_max_mixed) :-
    melrelative_complement_with_max([2, a], [-1, a], [2, a], 3).

test(melrelative_complement_with_max_zero) :-
    melrelative_complement_with_max([0, a], [1, a], [0, a], 3).

:- end_tests(melcomplement_with_max).


:- begin_tests(mset_complement_with_max).

:- use_module('../lib/universe').
:- use_module('../lib/multiset_operations').

test(mset_complement_with_max_positive) :-
    mset_complement_with_max([[2, a], [1, b]], R, 3),
    R == [[-1, a], [-2, b]].

test(mset_complement_with_max_negative) :-
    mset_complement_with_max([[-1, a], [0, b]], R, 3),
    R == [[2, a], [0, b]].

test(mset_complement_with_max_mixed) :-
    mset_complement_with_max([[2, a], [-1, b], [0, c]], R, 3),
    R == [[-1, a], [2, b], [0, c]].

:- end_tests(mset_complement_with_max).

