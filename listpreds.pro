:-module('listpreds',
        [
		delete_head/2,
        make_pair/3,
        make_pair_list/3,
        pair_list/1,
        write_list_vert/1,
		write_set_hor/1,
        lol_lengths/2,
        equilenth_lol/1
        ]).

delete_head([],[]).
delete_head([_|T],T).

write_list_wo_commas([]).
write_list_wo_commas([H|T]):-
             write(H), write(' '),
             write_list_wo_commas(T).
 
 
write_list_vert([]).
write_list_vert([H|T]):-
             write(H),nl,
             write_list_vert(T).
 

write_set_hor(S) :- write('{'), write_set_hor(S,S).
write_set_hor(_,[X]) :- write(X), write('}'), !.
write_set_hor(S,[H|T]) :- write(H),write(','), write_set_hor(S,T).

% Determines if a list is a list or ordered pairs.  The following grows without bound:
pair_list([]).
pair_list([[X,Y]|T]) :-
			 nonvar(X), nonvar(Y),
             pair_list(T).

make_pair(A,V,[A,V]).

make_pair_list(L1,L2,PairList):- length(L1,M),length(L2,M),
                                maplist(make_pair,L1,L2,PairList).

lol_lengths(LoL,LoL_lengths) :- maplist(length,LoL,LoL_lengths).
equilenth_lol(LoL) :- lol_lengths(LoL,LoL_lengths), list_to_set(LoL_lengths,S), length(S,1).
