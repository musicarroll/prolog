:-use_module(library(plunit)).
:-use_module('functions.pro').

:- begin_tests(order_set_and_cartesian).

test(cartesian_product) :-
    cartesian_product([a,b],[c,d],Cart),
    assertion(Cart == [[a,c],[a,d],[b,c],[b,d]]),
    !.

test(cartesian) :-
        cartesian([a,b],[c,d],Cart),
        assertion(Cart == [[a,c],[a,d],[b,c],[b,d]]),
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
