% 
:-module('functions',
        [
		has_subset/2,
		is_subset/2,
		delete_head/2,
		powerset/2,
		relation/3,
        pair_list/1,
        domain/2,
        range/2,
        image/3,
        func_image/2,
        function/1,
        gen_func/2,
        gen_func/3,
		gen_all_funcs/3,
		choose_val/2,
		gen_choice_func/3,
        eval/3,
        cartesian/2,
        gen_binop/2,
        cartesian/3,
        cartesian3/4,
		cartesian3/2,
		binop12/2,
        non_assoc_binop/1,
		assoc_binop_for/4,
		is_assoc_binop/2,
        has_identity/2,
		has_inverse_for/4,
		has_inverses/3,
        non_commute_binop/1,
        left_mults/3,
        write_list_vert/1,
		write_set_hor/1,
        subst_value/4,
        subst_value/3,
        subst_value_list/3,
        range_list/2,
		collection_of_nonempty_sets/2,
		write_op_table/1
        ]).
 
:-use_module(library(random)).
:-use_module(library(aggregate)).

% Bactrackable subset predicate Volodymyr Ostruk (stackoverflow, Jan 1, 2016)

has_subset(_, []).
has_subset([X|L], [A|NTail]):-
    member(A,[X|L]),    
    has_subset(L, NTail),
    not(member(A, NTail)).

is_subset([], []).
is_subset([E|Tail], [E|NTail]):-
  is_subset(Tail, NTail).
is_subset([_|Tail], NTail):-
  is_subset(Tail, NTail).

delete_head([],[]).
delete_head([_|T],T).

powerset(X,P) :- findall(SubSorted,(has_subset(X,Sub),sort(Sub,SubSorted)),L),list_to_set(L,P).

collection_of_nonempty_sets(C,X) :- is_set(X), \+X=[], 
	powerset(X,P), !, 
	subtract(P,[[]],NP), 
	has_subset(NP,S), \+S=[], 
	sort(S,C).


relation(Dom,Rng,R) :- cartesian(Dom,Rng,Cart), powerset(Cart,P), member(R,P).
 
% Determines if a list is a list or ordered pairs.  The following grows without bound:
pair_list([]).
pair_list([[X,Y]|T]) :-
			 nonvar(X), nonvar(Y),
             pair_list(T).
 
 
% As a list of pairs is a relation, this predicate produces the domain of
% that relation:  Note the definition of pair_list requires PL to be instantiated.
domain(PL,Dom) :-
			 pair_list(PL),
             findall(X,(member([X,Y],PL),nonvar(X),nonvar(Y)),L),
             list_to_set(L,Dom).
 
% Ditto for the range:
range(PL, Rng) :-
			 pair_list(PL),
             findall(Y,member([_,Y],PL),L),
             list_to_set(L,Rng).
 
range_list(PL, RngList):-
             pair_list(PL),
             findall(Y,member([_,Y],PL),RngList).
 
image(PL,X,ImgX) :-
             pair_list(PL),
             domain(PL,Dom),
             member(X,Dom),
             findall(Y,member([X,Y],PL),L),
             list_to_set(L,ImgX).
 
func_image(PL,ImgX) :-
             image(PL,_,ImgX),
             length(ImgX,1).
 
function(PL) :-
             pair_list(PL),
             forall(image(PL,_,ImgX),func_image(PL,ImgX)).
 
gen_func(DomRng,Func) :-
             gen_func(DomRng,DomRng,Func).
 
gen_func(Dom,Rng,Func) :-
             is_set(Dom),
             is_set(Rng),
             gen_func(Dom,Rng,[],Tmp),
             reverse(Tmp,Func).
 
gen_func([],_,Func,Func).
gen_func([H|T],Rng,Func1,Func) :-
             member(Y,Rng),
             Func2=[[H,Y]|Func1],
             gen_func(T,Rng,Func2,Func).
			 
gen_all_funcs(Dom,Rng,Funcs) :- findall(F,gen_func(Dom,Rng,F),L), list_to_set(L,Funcs).			 

choose_val(A,V):-member(V,A).
make_pair(A,V,[A,V]).

gen_choice_func(X,C,F) :- collection_of_nonempty_sets(C,X), 
		Dom=C,
		maplist(choose_val,Dom,Vals),
		maplist(make_pair,Dom,Vals,Ftmp),
		sort(Ftmp,F).
		
 
eval(F,X,Y) :- nonvar(F), nonvar(X),
             function(F),
             member([X,Y],F).
 
cartesian(A,B,Cart):-
             findall([X,Y],(member(X,A),member(Y,B)),L),
             list_to_set(L,Cart),!.
 
cartesian(A,Cart):- cartesian(A,A,Cart).
 
cartesian3(A,B,C,Cart):-
             findall([X,Y,Z],(member(X,A),member(Y,B),member(Z,C)),L),
             list_to_set(L,Cart),!.
cartesian3(A,Cart) :- cartesian3(A,A,A,Cart).

binop12(R3,R2) :- findall([[X,Y],Z],member([X,Y,Z],R3),L), list_to_set(L,R2),length(L,M),length(R2,N),M=N.
 
gen_binop(A,BinOp):-
             cartesian(A,Cart),
             gen_func(Cart,A,BinOp).
 
subst_value(Y,Z,[X,Y],[X,Z]).
subst_value(Z,[X,_],[X,Z]).
 
subst_value_list([],List,List).
subst_value_list([H|T],[[H1,_]|T1],[[H1,H]|T2]):-
             subst_value_list(T,T1,T2).
 
%permute_binop(Bop1,Bop2):-
 
assoc_binop_for(BinOp,X,Y,Z) :- eval(BinOp,[X,Y],Left),
			eval(BinOp,[Left,Z],Result),
			eval(BinOp,[Y,Z],Right),
			eval(BinOp,[X,Right],Result).
			
is_assoc_binop(BinOp,A) :- cartesian3(A,A,A, Cart3),
			forall(member([X,Y,Z],Cart3),assoc_binop_for(BinOp,X,Y,Z)).
			
non_assoc_binop(BinOp):-
             domain(BinOp,D),
             flatten(D,A),
             cartesian3(A,A,A,Cart),
             member([X,Y,Z],Cart),
             eval(BinOp,[X,Y],X1),
             eval(BinOp,[Y,Z],Y2),
             eval(BinOp,[X1,Z],U),
             eval(BinOp,[X,Y2],V),
             U\=V.
 
has_identity(BinOp,Id):-
             binop_domain(BinOp,Dom),
             member(E,Dom),
             findall(B,(member(B,Dom),eval(BinOp,[E,B],B),eval(BinOp,[B,E],B)),L),
             list_to_set(L,S),
             S==Dom,
             E=Id.

has_inverse_for(BinOp,X,Y,Id) :- var(Id), has_identity(BinOp,Id), eval(BinOp,[X,Y],Id). 
has_inverse_for(BinOp,X,Y,Id) :- nonvar(Id), eval(BinOp,[X,Y],Id). 

has_inverses(BinOp,A,Id) :- var(Id),
			has_identity(BinOp,Id),
			forall(member(X,A),(member(Y,A),has_inverse_for(BinOp,X,Y,Id))).
			
has_inverses(BinOp,A,Id) :- nonvar(Id),
			forall(member(X,A),(member(Y,A),has_inverse_for(BinOp,X,Y,Id))).
	
 
binop_domain(BinOp,Dom):-
             domain(BinOp,D),
             flatten(D,D1),
             list_to_set(D1,Dom).
 
 
non_commute_binop(BinOp):-
             binop_domain(BinOp,Dom),
             cartesian(Dom,Cart),
             member([X,Y],Cart),
             eval(BinOp,[X,Y],Z1),
             eval(BinOp,[Y,X],Z2),
             Z1\==Z2.
 
left_mults(X,Bop,Mults):-
             binop_domain(Bop,Dom),
             findall(Z,(member(Y,Dom),eval(Bop,[X,Y],Z)),Mults).
 
 
write_list_wo_commas([]).
write_list_wo_commas([H|T]):-
             write(H), write(' '),
             write_list_wo_commas(T).
 
 
write_list_vert([]).
write_list_vert([H|T]):-
             write(H),nl,
             write_list_vert(T).
 
write_op_table(Bop):- nonvar(Bop),
             binop_domain(Bop,Dom),
             write('   '), write_list_wo_commas(Dom),nl,
			 foreach(member(X,Dom),
				( 
					write(X), 
					write('  '),
					forall(member(Y,Dom),(eval(Bop,[X,Y],Z),!,write(Z),write(' '))),
					nl)
				).

write_set_hor(S) :- write('{'), write_set_hor(S,S).
write_set_hor(_,[X]) :- write(X), write('}'), !.
write_set_hor(S,[H|T]) :- write(H),write(','), write_set_hor(S,T).


