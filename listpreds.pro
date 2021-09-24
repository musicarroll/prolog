:-module('listpreds',
        [
        write_list_vert/1,
		write_set_hor/1
        ]).
 
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
