% Define your ternary predicate. Replace this with your actual ternary predicate.
ternary_predicate(a, b, c).
ternary_predicate(d, e, f).
% Add more facts for your ternary predicate as needed.

% Define a binary relation predicate with three arguments.
binary_relation(X, Y, Z) :- ternary_predicate(X, Y, Z).

% Convert the ternary predicate to a list of binary relations.
convert_to_binary_relations(Relations) :-
    findall((X, Y, Z), binary_relation(X, Y, Z), Relations).

% Example usage:
% To convert the ternary predicate "ternary_predicate" to a list of binary relations:
% ?- convert_to_binary_relations(Relations).
