parent(mike,christopher).
parent(leandra,christopher).
parent(mike,josi).
parent(leandra,josi).
parent(mike,dominic).
parent(leandra,dominic).

sibling(X,Y):- parent(F,X),parent(M,X), parent(F,Y), parent(M,Y), \+X=Y.

