% parent/2 - defines the parent predicate.
parent(mike,christopher).
% parent/2 - defines the parent predicate.
parent(leandra,christopher).
% parent/2 - defines the parent predicate.
parent(mike,josi).
% parent/2 - defines the parent predicate.
parent(leandra,josi).
% parent/2 - defines the parent predicate.
parent(mike,dominic).
% parent/2 - defines the parent predicate.
parent(leandra,dominic).

% sibling/2 - defines the sibling predicate.
sibling(X,Y):- parent(F,X),parent(M,X), parent(F,Y), parent(M,Y), \+X=Y.

