% Updated element_operations.pl for disjoint polarity-based multisets

:- module(element_operations, [
    melintersect/3,
    melunion/3,
    melcomplement/2,
    melrelative_complement/3,
    melsymmetric_difference/3
]).

% melintersect/3: Intersection operation for bipolar multisets
melintersect([M, X], [N, X], [R, X]) :-
    ( is_positive(X) -> R is min(M, N)
    ; is_negative(X) -> R is max(M, N)
    ).

% melunion/3: Union operation for bipolar multisets
melunion([M, X], [N, X], [R, X]) :-
    ( is_positive(X) -> R is max(M, N)
    ; is_negative(X) -> R is min(M, N)
    ).

% melcomplement/2: Complement of an element via polarity swap
melcomplement([M, X], [M, Y]) :-
    opposite(X, Y).

% melrelative_complement/3: Relative complement A \ B = A ∩ (¬B)
melrelative_complement([M, X], [N, X], [R, X]) :-
    melcomplement([N, X], [NComp, Y]),
    opposite(X, Y),  % ensure correct pairing
    melintersect([M, X], [NComp, X], [R, X]).

% melsymmetric_difference/3: A Δ B = (A \ B) ∪ (B \ A)
melsymmetric_difference([M, X], [N, X], [R, X]) :-
    melrelative_complement([M, X], [N, X], [D1, X]),
    melrelative_complement([N, X], [M, X], [D2, X]),
    melunion([D1, X], [D2, X], [R, X]).

% --- Polarity and opposite relation helpers ---

% Define is_positive/1 and is_negative/1 based on naming convention
is_positive(X) :- atom(X), atom_concat(_, '+', X).
is_negative(X) :- atom(X), atom_concat(_, '-', X).

% Define opposites (must be consistent with universe.pl)
opposite(X, Y) :-
    atom(X), atom(Y),
    atom_concat(Base, '+', X), atom_concat(Base, '-', Y), !.
opposite(X, Y) :-
    atom(X), atom(Y),
    atom_concat(Base, '-', X), atom_concat(Base, '+', Y), !.
