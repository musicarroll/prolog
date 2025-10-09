% Updated multisets.pl for bipolar disjoint polarity-based multiset universe

:- module(multisets, [
    u/1,
    melem/1,
    mmember/2,
    melintersect/3,
    melunion/3,
    melcomplement/2,
    melcomplement_list/2,
    full_bit_vector/2,
    update_vector/3,
    mset_normalize/2,
    mset_intersect/3,
    mset_union/3
]).

% Universe U is disjoint union of U+ and U-
u(X) :- member(X, [a+, b+, c+, a-, b-, c-]).

% Valid multiset element: pair (multiplicity, element)
melem([N, X]) :- integer(N), N >= 0, u(X).

% Ensure no duplicates and all members are valid
mmember(X, M) :-
    list_to_set(M, S),
    length(M, Lm),
    length(S, Ls),
    Lm =:= Ls,
    member(X, M),
    melem(X).

% Define polarity-aware intersection
melintersect([M, X], [N, X], [R, X]) :-
    ( is_positive(X) -> R is min(M, N)
    ; is_negative(X) -> R is max(M, N)
    ).

% Define polarity-aware union
melunion([M, X], [N, X], [R, X]) :-
    ( is_positive(X) -> R is max(M, N)
    ; is_negative(X) -> R is min(M, N)
    ).

% Complement an individual element
melcomplement([M, X], [M, Y]) :-
    opposite(X, Y).

% Complement entire multiset
melcomplement_list(MSet, Complement) :-
    maplist(melcomplement, MSet, Complement).

% Normalize mset to full vector with 0s for missing elements
full_bit_vector(Partial, Full) :-
    findall([0, X], u(X), DefaultVector),
    update_vector(Partial, DefaultVector, Full).

update_vector([], Full, Full).
update_vector([[M, X] | Rest], Default, Updated) :-
    select([_, X], Default, Temp, Intermediate),
    UpdatedX = [M, X],
    update_vector(Rest, [UpdatedX | Temp], Updated).

% Normalize: same as full_bit_vector
mset_normalize(Partial, Full) :-
    full_bit_vector(Partial, Full).

% Multiset intersection (element-wise)
mset_intersect(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1),
    mset_normalize(MSet2, Norm2),
    maplist(melintersect, Norm1, Norm2, Result).

% Multiset union (element-wise)
mset_union(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1),
    mset_normalize(MSet2, Norm2),
    maplist(melunion, Norm1, Norm2, Result).

% --- Polarity and opposite helpers ---
is_positive(X) :- atom_concat(_, '+', X).
is_negative(X) :- atom_concat(_, '-', X).

opposite(X, Y) :- atom_concat(Base, '+', X), atom_concat(Base, '-', Y), !.
opposite(X, Y) :- atom_concat(Base, '-', X), atom_concat(Base, '+', Y), !.
