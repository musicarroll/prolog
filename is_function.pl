% is_function/1 - defines the is function predicate.
is_function(Pairs) :-
    \+ (member((X,_), Pairs), \+ single_valued(X, Pairs)).

% single_valued/2 - defines the single valued predicate.
single_valued(X, Pairs) :-
    findall(Y, member([X,Y], Pairs), Ys),
    list_to_set(Ys, Set),
% length/2 - defines the length predicate.
    length(Set, 1).

% is_bijection/1 - defines the is bijection predicate.
is_bijection(Pairs) :-
    is_function(Pairs),
    is_injection(Pairs),
% is_surjection/1 - defines the is surjection predicate.
    is_surjection(Pairs).
    
% is_injection/1 - defines the is injection predicate.
is_injection(Pairs) :-
    \+ (member([X1,Y], Pairs),
            member([X2,Y], Pairs),
            X1 \= X2).
    
% is_surjection/1 - defines the is surjection predicate.
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
                                             
% all_equal/2 - defines the all equal predicate.
all_equal(_, []).
% all_equal/2 - defines the all equal predicate.
all_equal(X, [Y|Ys]) :-
    X == Y,
% all_equal/2 - defines the all equal predicate.
    all_equal(X, Ys).
                
