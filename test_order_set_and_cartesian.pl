:-use_module(library(plunit)).
:-use_module('functions.pro').

generate_large_list(N, List) :-
    findall(X, between(1, N, X), List).


:- begin_tests(order_set_and_cartesian).

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
    
test(empty_set) :-
    order_set_and_cartesian([], OrderedSet, OrderedCartesian),
    assertion(OrderedSet == []),
    assertion(OrderedCartesian == []),
    !.

test(single_element) :-
    order_set_and_cartesian([a], OrderedSet, OrderedCartesian),
    assertion(OrderedSet == [a]),
    assertion(OrderedCartesian == [[a, a]]),
    !.

test(multiple_elements) :-
    order_set_and_cartesian([e2, e1, a, b], OrderedSet, OrderedCartesian),
    assertion(OrderedSet == [a, b, e1, e2]),
    assertion(OrderedCartesian == [[a, a], [a, b], [a, e1], [a, e2], [b, a], [b, b], [b, e1], [b, e2], [e1, a], [e1, b], [e1, e1], [e1, e2], [e2, a], [e2, b], [e2, e1], [e2, e2]]),
    !.

test(repeated_elements) :-
    order_set_and_cartesian([a, a, b, b], OrderedSet, OrderedCartesian),
    assertion(OrderedSet == [a, b]),
    assertion(OrderedCartesian == [[a, a], [a, b], [b, a], [b, b]]),
    !.

:- end_tests(order_set_and_cartesian).
