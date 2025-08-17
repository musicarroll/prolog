% family.pl
% parent/2 - defines the parent predicate.
parent(alice, bob).
% parent/2 - defines the parent predicate.
parent(bob, charlie).
% parent/2 - defines the parent predicate.
parent(bob, diana).

% grandparent/2 - defines the grandparent predicate.
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
