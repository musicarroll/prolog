% Define u/1 predicate
u(X) :- member(X, [a, b, c]).

% Define melem/1 predicate
melem([N, X]) :- u(X), integer(N).

% Define mmember/2 predicate
mmember(X, M) :-
    list_to_set(M, S),
    length(M, Lm),
    length(S, Ls),
    Lm =:= Ls,  % Ensure strict equality
    member(X, M),
    melem(X).


meloperation(OpMax, OpMin, [M, X], [N, X], [R, X]) :-
    u(X),  % Ensure valid element
    sign(M, SM),
    sign(N, SN),
    select_operation(OpMax, OpMin, SM, SN, M, N, R).

% Selector for operations based on the given max/min strategy
select_operation(OpMax, _, -1, -1, M, N, R) :- call(OpMax, M, N, R).
select_operation(OpMax, _, -1,  0, M, N, R) :- call(OpMax, M, N, R).
select_operation(OpMax, _, -1,  1, M, N, R) :- call(OpMax, M, N, R).
select_operation(OpMax, _,  0, -1, M, N, R) :- call(OpMax, M, N, R).
select_operation(_, _,  0,  0, _, _,  0). % Neutral zero case
select_operation(_, OpMin,  0,  1, M, N, R) :- call(OpMin, M, N, R).
select_operation(_, OpMin,  1, -1, M, N, R) :- call(OpMin, M, N, R).
select_operation(_, OpMin,  1,  0, M, N, R) :- call(OpMin, M, N, R).
select_operation(_, OpMin,  1,  1, M, N, R) :- call(OpMin, M, N, R).

% Define helper predicates for max and min
max_operation(M, N, R) :- R is max(M, N).
min_operation(M, N, R) :- R is min(M, N).

% Define melintersect/3 using max for negatives and min for positives
melintersect(A, B, C) :-
    meloperation(max_operation, min_operation, A, B, C).

% Define melunion/3 using min for negatives and max for positives
melunion(A, B, C) :-
    meloperation(min_operation, max_operation, A, B, C).

% melcomplement/2: Computes the complement of a multiset element.
melcomplement([M, X], [R, X]) :-
    u(X), % Ensure valid element
    R is -M. % Negate the multiplicity

melcomplement_list(MSet, Complement) :-
    maplist(melcomplement, MSet, Complement).

% full_bit_vector/2: Extends a partial mset to a full bit vector form.
full_bit_vector(Partial, Full) :-
    findall([0, X], u(X), DefaultVector), % Create the default vector with 0s for all universe elements
    update_vector(Partial, DefaultVector, Full). % Update the default vector with the partial mset

% update_vector/3: Updates the default vector with the multiplicities from the partial list.
update_vector([], Full, Full). % Base case: If no updates remain, return the result
update_vector([[M, X] | Rest], Default, Updated) :-
    select([0, X], Default, [M, X], Intermediate), % Replace [0, X] with [M, X]
    update_vector(Rest, Intermediate, Updated). % Recur with the rest of the updates

% mset_normalize/2: Normalize a partial mset to a full bit vector.
mset_normalize(Partial, Full) :-
    findall([0, X], u(X), DefaultVector), % Create the default vector with 0s for all universe elements
    update_vector(Partial, DefaultVector, Full). % Update the default vector with the partial mset

% mset_intersect/3: Compute the intersection of two multisets.
mset_intersect(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1), % Normalize the first mset
    mset_normalize(MSet2, Norm2), % Normalize the second mset
    maplist(melintersect, Norm1, Norm2, Result). % Apply melintersect element-wise

% mset_union/3: Compute the union of two multisets.
mset_union(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1), % Normalize the first mset
    mset_normalize(MSet2, Norm2), % Normalize the second mset
    maplist(melunion, Norm1, Norm2, Result). % Apply melunion element-wise

    