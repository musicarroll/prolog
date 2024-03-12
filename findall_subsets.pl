% generate subsets of a list
subsets([], []).
subsets([X|Xs], [X|Ys]) :- subsets(Xs, Ys).
subsets([_|Xs], Ys) :- subsets(Xs, Ys).

% generate all subsets of a set
all_subsets(Set, Subsets) :-
    list_to_set(Set, SetUnique),
    findall(Subset, (subsets(SetUnique, Subset), length(Subset, N), N > 0), Subsets).

