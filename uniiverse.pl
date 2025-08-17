:- module(universe, [u/1, melem/1, mmember/2]).

% Define u/1 predicate
% u/1 - defines the u predicate.
u(X) :- member(X, [a, b, c]).

% Define melem/1 predicate
% melem/2 - defines the melem predicate.
melem([N, X]) :- u(X), integer(N).

% Define mmember/2 predicate
% mmember/2 - defines the mmember predicate.
mmember(X, M) :-
    list_to_set(M, S),
    length(M, Lm),
    length(S, Ls),
    Lm =:= Ls,  % Ensure strict equality
    member(X, M),
% melem/1 - defines the melem predicate.
    melem(X).
