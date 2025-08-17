% generate subsets of a list
% subsets/2 - defines the subsets predicate.
subsets([], []).
% subsets/2 - defines the subsets predicate.
subsets([X|Xs], [X|Ys]) :- subsets(Xs, Ys).
% subsets/2 - defines the subsets predicate.
subsets([_|Xs], Ys) :- subsets(Xs, Ys).

% generate all subsets of a set
% all_subsets/2 - defines the all subsets predicate.
all_subsets(Set, Subsets) :-
    list_to_set(Set, SetUnique),
    findall(Subset, (subsets(SetUnique, Subset), length(Subset, N), N > 0), Subsets).

