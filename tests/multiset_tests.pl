:- use_module(library(plunit)).

% Workaround for older SWI builds missing '$messages':to_list/2 used by plunit
:- initialization(fix_messages).
fix_messages :-
    (   current_predicate('$messages':to_list/2)
    ->  true
    ;   catch(assertz(('$messages':to_list(X, L) :- (is_list(X) -> L = X ; L = [X]))), _, true)
    ).

:- begin_tests(multisets).

:- use_module('../lib/universe').
:- use_module('../lib/multiset_operations').
:- use_module('../lib/element_operations').

% Updated tests for disjoint polarity universe (a+, b+, c+, a-, b-, c-)

% Test cases for mset_normalize/2
test(normalize_partial) :-
    mset_normalize([[3, 'a+']], [[3, 'a+'], [0, 'b+'], [0, 'c+'], [0, 'a-'], [0, 'b-'], [0, 'c-']]).
test(normalize_empty) :-
    mset_normalize([], [[0, 'a+'], [0, 'b+'], [0, 'c+'], [0, 'a-'], [0, 'b-'], [0, 'c-']]).

% Test cases for melintersect/3
test(melintersect_positive) :-
    melintersect([3, 'a+'], [2, 'a+'], [2, 'a+']).
test(melintersect_negative) :-
    melintersect([3, 'a-'], [2, 'a-'], [3, 'a-']).

% Test cases for melunion/3
test(melunion_positive) :-
    melunion([3, 'a+'], [2, 'a+'], [3, 'a+']).
test(melunion_negative) :-
    melunion([3, 'a-'], [2, 'a-'], [2, 'a-']).

% Test cases for melcomplement/2
test(melcomplement_positive) :-
    melcomplement([3, 'a+'], [3, 'a-']).
test(melcomplement_negative) :-
    melcomplement([3, 'a-'], [3, 'a+']).

% Test cases for melcomplement_list/2
test(melcomplement_list_partial) :-
    melcomplement_list([[2, 'a+'], [1, 'b-']], [[2, 'a-'], [1, 'b+']]).

test(melcomplement_list_full) :-
    melcomplement_list([[2, 'a+'], [1, 'b-'], [0, 'c+']], [[2, 'a-'], [1, 'b+'], [0, 'c-']]).

% Test cases for full_bit_vector/2
test(full_bit_vector_partial) :-
    full_bit_vector([[2, 'a+']], [[2, 'a+'], [0, 'b+'], [0, 'c+'], [0, 'a-'], [0, 'b-'], [0, 'c-']]).
test(full_bit_vector_empty) :-
    full_bit_vector([], [[0, 'a+'], [0, 'b+'], [0, 'c+'], [0, 'a-'], [0, 'b-'], [0, 'c-']]).

% Test cases for mset_intersect/3
test(mset_intersect_partial) :-
    mset_intersect([[2, 'a+'], [1, 'b-']], [[1, 'a+'], [3, 'b-']], [[1, 'a+'], [0, 'b+'], [0, 'c+'], [0, 'a-'], [3, 'b-'], [0, 'c-']]).

test(mset_intersect_full) :-
    mset_intersect([[2, 'a+'], [1, 'b+'], [1, 'a-'], [0, 'b-'], [0, 'c+'], [0, 'c-']],
                   [[3, 'a+'], [0, 'b+'], [2, 'a-'], [1, 'b-'], [0, 'c+'], [0, 'c-']],
                   [[2, 'a+'], [0, 'b+'], [0, 'c+'], [2, 'a-'], [1, 'b-'], [0, 'c-']]).

% Test cases for mset_union/3
test(mset_union_partial) :-
    mset_union([[2, 'a+'], [1, 'b-']], [[1, 'a+'], [3, 'b-']], [[2, 'a+'], [0, 'b+'], [0, 'c+'], [0, 'a-'], [1, 'b-'], [0, 'c-']]).

test(mset_union_full) :-
    mset_union([[2, 'a+'], [1, 'b+'], [1, 'a-'], [0, 'b-'], [0, 'c+'], [0, 'c-']],
               [[3, 'a+'], [0, 'b+'], [2, 'a-'], [1, 'b-'], [0, 'c+'], [0, 'c-']],
               [[3, 'a+'], [1, 'b+'], [0, 'c+'], [1, 'a-'], [0, 'b-'], [0, 'c-']]).

% (keep tests for msubsets and on-support enumeration in same suite)

% Subset semantics: positives monotone (<=), negatives antitone (>=)
test(msubset_positive_true) :-
    msubset([[1, 'a+']], [[2, 'a+']]).

test(msubset_positive_false, [fail]) :-
    msubset([[3, 'a+']], [[2, 'a+']]).

test(msubset_negative_true) :-
    msubset([[3, 'b-']], [[1, 'b-']]).

test(msubset_negative_false, [fail]) :-
    msubset([[0, 'b-']], [[1, 'b-']]).

% Enumeration with explicit max bound
test(all_msubsets_with_max_small_enumeration) :-
    Parent = [[2, 'a+'], [1, 'b-']],
    M = 1,
    all_msubsets_with_max(Parent, M, Subs),
    % With M=1, combinations: a+ in {0,1}, b- in {1}, a- in {0,1}, c- in {0,1}
    % and b+, c+ fixed to 0: 2*1*2*2 = 8
    length(Subs, 8),
    % Spot-check one member deterministically
    once(member([[1,'a+'], [0,'b+'], [0,'c+'], [0,'a-'], [1,'b-'], [0,'c-']], Subs)).


test(on_support_counts_only_positive_varies) :-
    Parent = [[2,'a+'], [1,'b-']],
    all_msubsets_on_support(Parent, Subs),
    length(Subs, 3), % a+ ∈ {0,1,2}; b- fixed to 1; others fixed to 0
% Check endpoints present
    once(member([[0,'a+'], [0,'b+'], [0,'c+'], [0,'a-'], [1,'b-'], [0,'c-']], Subs)),
    once(member([[2,'a+'], [0,'b+'], [0,'c+'], [0,'a-'], [1,'b-'], [0,'c-']], Subs)),
    !.

test(on_support_two_positives) :-
    Parent = [[2,'a+'], [1,'b+']],
    all_msubsets_on_support(Parent, Subs),
    length(Subs, L), L is 3*2, % (a+ in 0..2) × (b+ in 0..1)
    once(member([[1,'a+'], [0,'b+'], [0,'c+'], [0,'a-'], [0,'b-'], [0,'c-']], Subs)),
    once(member([[2,'a+'], [1,'b+'], [0,'c+'], [0,'a-'], [0,'b-'], [0,'c-']], Subs)),
    !.

test(power_msubsets_alias_matches_on_support) :-
    Parent = [[2,'a+'], [1,'b-']],
    all_msubsets_on_support(Parent, Subs1),
    power_msubsets(Parent, Subs2),
    length(Subs1, L1), length(Subs2, L2), L1 == L2,
    % Spot-check a canonical member exists in both
    Member = [[1,'a+'], [0,'b+'], [0,'c+'], [0,'a-'], [1,'b-'], [0,'c-']],
    once(member(Member, Subs1)),
    once(member(Member, Subs2)),
    !.

:- end_tests(multisets).
