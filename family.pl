% family.pl
parent(alice, bob).
parent(bob, charlie).
parent(bob, diana).

grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
