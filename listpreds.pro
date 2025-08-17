:-module('listpreds',
        [
		delete_head/2,
        make_pair/3,
        make_pair_list/3,
        pair_list/1,
        write_list_vert/1,
		write_set_hor/1,
        write_set_latex/1,
        lol_lengths/2,
        equilenth_lol/1,
        blank_unless/3
        ]).

% delete_head/2 - defines the delete head predicate.
delete_head([],[]).
% delete_head/2 - defines the delete head predicate.
delete_head([_|T],T).

% write_list_wo_commas/1 - defines the write list wo commas predicate.
write_list_wo_commas([]).
% write_list_wo_commas/1 - defines the write list wo commas predicate.
write_list_wo_commas([H|T]):-
             write(H), write(' '),
% write_list_wo_commas/1 - defines the write list wo commas predicate.
             write_list_wo_commas(T).
 
 
% write_list_vert/1 - defines the write list vert predicate.
write_list_vert([]).
% write_list_vert/1 - defines the write list vert predicate.
write_list_vert([H|T]):-
             write(H),nl,
% write_list_vert/1 - defines the write list vert predicate.
             write_list_vert(T).
 

% write_set_hor/1 - defines the write set hor predicate.
write_set_hor(S) :- write('{'), write_set_hor(S,S).
% write_set_hor/2 - defines the write set hor predicate.
write_set_hor(_,[X]) :- write(X), write('}'), !.
% write_set_hor/2 - defines the write set hor predicate.
write_set_hor(S,[H|T]) :- write(H),write(','), write_set_hor(S,T).

% write_set_latex/1 - defines the write set latex predicate.
write_set_latex(S) :- write('\\set{'), write_set_latex(S,S).
% write_set_latex/2 - defines the write set latex predicate.
write_set_latex(_,[X]) :- write(X), write('}'), !.
% write_set_latex/2 - defines the write set latex predicate.
write_set_latex(S,[H|T]) :- write(H),write(','), write_set_latex(S,T).


% Determines if a list is a list or ordered pairs.  The following grows without bound:
% pair_list/1 - defines the pair list predicate.
pair_list([]).
% pair_list/2 - defines the pair list predicate.
pair_list([[X,Y]|T]) :-
			 nonvar(X), nonvar(Y),
% pair_list/1 - defines the pair list predicate.
             pair_list(T).

% make_pair/4 - defines the make pair predicate.
make_pair(A,V,[A,V]).

% make_pair_list/3 - defines the make pair list predicate.
make_pair_list(L1,L2,PairList):- length(L1,M),length(L2,M),
% maplist/4 - defines the maplist predicate.
                                maplist(make_pair,L1,L2,PairList).

% lol_lengths/2 - defines the lol lengths predicate.
lol_lengths(LoL,LoL_lengths) :- maplist(length,LoL,LoL_lengths).
% equilenth_lol/1 - defines the equilenth lol predicate.
equilenth_lol(LoL) :- lol_lengths(LoL,LoL_lengths), list_to_set(LoL_lengths,S), length(S,1).

% blank_unless/3 - defines the blank unless predicate.
blank_unless(Var,OldList,NewList):-blank_unless(Var,1,OldList,[],NewList).
% blank_unless/5 - defines the blank unless predicate.
blank_unless(_,_,[],NewList,NewList).
% blank_unless/5 - defines the blank unless predicate.
blank_unless(Var,Inc,[Var|T],Tmp,NewList) :- append([Var],Tmp,NewTmp),
% blank_unless/5 - defines the blank unless predicate.
                                        blank_unless(Var,Inc,T,NewTmp,NewList).
% blank_unless/5 - defines the blank unless predicate.
blank_unless(Var,Inc,[Var1|T],Tmp,NewList) :- \+Var=Var1, NewInc is Inc+1, append([NewInc],Tmp,NewTmp),
% blank_unless/5 - defines the blank unless predicate.
                                        blank_unless(Var,NewInc,T,NewTmp,NewList).
