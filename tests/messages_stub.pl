:- module('$messages', [to_list/2]).

% Minimal stub to satisfy older SWI builds where '$messages':to_list/2
% may be missing. Ensures message printing does not crash plunit.
to_list(X, L) :-
    (   is_list(X)
    ->  L = X
    ;   L = [X]
    ).
