% Define the DCG rules
program --> [].
program --> assignment_statement, "\n", program.

assignment_statement --> variable, equal, value.

variable --> [X], { member(X, ["X", "Y"]) }.
equal --> ["="].
value --> integer.
value --> [Value], { member(Value, [0, 1, 2]) }.

integer --> [X], { integer(X) }.


parse(Tokens) :-
    phrase(program, Tokens).
% Sample query
% parse(["X", "=", "10", ";", "if", "X", "==", "Y", ":", "Y", "=", "20", ";", "elif", "X", "==", "Z", ":", "Z", "=", "30", ";", "else", ":", "Y", "=", "40", ";", "endif"], []).
% Output: true
