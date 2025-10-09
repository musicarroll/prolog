% Updated universe.pl for disjoint-polarity model

:- module(universe, [u/1, u_plus/1, u_minus/1, opposite/2, melem/1, mmember/2]).

% Define universe sets (disjoint U⁺ and U⁻)
u_plus(X)  :- member(X, ['a+', 'b+', 'c+']).
u_minus(X) :- member(X, ['a-', 'b-', 'c-']).

u(X) :- u_plus(X); u_minus(X).

% Opposite mapping between U⁺ and U⁻
opposite(X, Y) :- atom(X), atom_concat(Base, '+', X), atom_concat(Base, '-', Y), !.
opposite(X, Y) :- atom(X), atom_concat(Base, '-', X), atom_concat(Base, '+', Y), !.

% Valid element-multiplicity pair
melem([N, X]) :- u(X), integer(N), N >= 0.

% Membership check for msets (no duplicates, all valid elements)
mmember(X, M) :-
    list_to_set(M, S),
    length(M, Lm),
    length(S, Ls),
    Lm =:= Ls,
    member(X, M),
    melem(X).
