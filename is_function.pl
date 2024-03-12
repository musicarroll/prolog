is_function(Pairs) :-
    \+ (member((X,_), Pairs), \+ single_valued(X, Pairs)).

single_valued(X, Pairs) :-
    findall(Y, member([X,Y], Pairs), Ys),
    list_to_set(Ys, Set),
    length(Set, 1).

is_bijection(Pairs) :-
    is_function(Pairs),
    is_injection(Pairs),
    is_surjection(Pairs).
    
is_injection(Pairs) :-
    \+ (member([X1,Y], Pairs),
            member([X2,Y], Pairs),
            X1 \= X2).
    
is_surjection(Pairs) :-
ground(Pairs),
findall(Y, (member([_,Y], Pairs), nonvar(Y)), Ys),
sort(Ys, DistinctYs),
length(Pairs, NumPairs),
length(DistinctYs, NumDistinct),
(   NumDistinct =:= 0, NumPairs =:= 0
;   NumDistinct > 0,
    forall(member(Y, DistinctYs),
            (member([_, Y], Pairs))
    ),
    NumPairs =:= NumDistinct
).
                                             
all_equal(_, []).
all_equal(X, [Y|Ys]) :-
    X == Y,
    all_equal(X, Ys).
                