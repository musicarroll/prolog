:-use_module(library(plunit)).
:-use_module('functions.pro').

% generate_large_list/2 - defines the generate large list predicate.
generate_large_list(N, List) :-
    findall(X, between(1, N, X), List).


:- begin_tests(order_set_and_cartesian).

% test/1 - defines the test predicate.
test(cartesian_product) :-
    NumElems = 500,
    generate_large_list(NumElems, LargeList), % Generate larger lists
    cartesian_product(LargeList, LargeList,Answer),
    statistics(walltime, [StartCart|_]),
    cartesian_product(LargeList, LargeList, Cart),
    statistics(walltime, [EndCart|_]),
    TimeSort is EndCart - StartCart,
    assertion(Cart == Answer),
    nl,format('Time taken for cartesian_product/3 with ~w elements: ~w ms~n', [NumElems,TimeSort]),
    !.

% test/1 - defines the test predicate.
test(cartesian) :-
    NumElems = 500,
    generate_large_list(NumElems, LargeList), % Generate larger lists
    cartesian_product(LargeList, LargeList,Answer),
    statistics(walltime, [StartCart|_]),
    cartesian(LargeList,LargeList,Cart),
    statistics(walltime, [EndCart|_]),
    TimeSort is EndCart - StartCart,
    assertion(Cart == Answer),
    nl,format('Time taken for cartesian/3 with ~w elements: ~w ms~n', [NumElems,TimeSort]),
    !.
    
% test/1 - defines the test predicate.
test(empty_set) :-
    order_set_and_cartesian([], OrderedSet, OrderedCartesian),
    assertion(OrderedSet == []),
    assertion(OrderedCartesian == []),
    !.

% test/1 - defines the test predicate.
test(single_element) :-
    order_set_and_cartesian([a], OrderedSet, OrderedCartesian),
    assertion(OrderedSet == [a]),
    assertion(OrderedCartesian == [[a, a]]),
    !.

% test/1 - defines the test predicate.
test(multiple_elements) :-
    order_set_and_cartesian([e2, e1, a, b], OrderedSet, OrderedCartesian),
    assertion(OrderedSet == [a, b, e1, e2]),
    assertion(OrderedCartesian == [[a, a], [a, b], [a, e1], [a, e2], [b, a], [b, b], [b, e1], [b, e2], [e1, a], [e1, b], [e1, e1], [e1, e2], [e2, a], [e2, b], [e2, e1], [e2, e2]]),
    !.

% test/1 - defines the test predicate.
test(repeated_elements) :-
    order_set_and_cartesian([a, a, b, b], OrderedSet, OrderedCartesian),
    assertion(OrderedSet == [a, b]),
    assertion(OrderedCartesian == [[a, a], [a, b], [b, a], [b, b]]),
    !.

% Test Cartesian product of three different sets
% test/1 - defines the test predicate.
test(different_sets) :-
    cartesian3([1,2], [a,b], [x,y], Result),
    Expected = [[1,a,x],[1,a,y],[1,b,x],[1,b,y],[2,a,x],[2,a,y],[2,b,x],[2,b,y]],
    assertion(Result == Expected),
    !.

% Test Cartesian product of three identical sets
% test/1 - defines the test predicate.
test(identical_sets) :-
    cartesian3([1,2], Result),
    Expected = [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]],
    assertion(Result == Expected),
    !.

% Test with one empty set should yield an empty result
% test/1 - defines the test predicate.
test(one_empty_set) :-
    cartesian3([1,2], [a,b], [], Result),
    assertion(Result == []),
    !.

% Test with all empty sets
% test/1 - defines the test predicate.
test(all_empty_sets) :-
    cartesian3([], [], [], Result),
    assertion(Result == []),
    !.

% Test with non-integer elements
% test/1 - defines the test predicate.
test(non_integer_elements) :-
    cartesian3(['apple', 'banana'], ['red', 'green'], ['small', 'large'], Result),
    Expected = [
        ['apple', 'red', 'small'], ['apple', 'red', 'large'],
        ['apple', 'green', 'small'], ['apple', 'green', 'large'],
        ['banana', 'red', 'small'], ['banana', 'red', 'large'],
        ['banana', 'green', 'small'], ['banana', 'green', 'large']
    ],
    assertion(Result == Expected),
    !.


:- end_tests(order_set_and_cartesian).
