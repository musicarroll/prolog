:- module(universe, [u/1, melem/1, mmember/2]).

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
