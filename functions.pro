:-module('functions',
        [
		has_subset/2,
		is_subset/2,
		powerset/2,
		relation/3,
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
        cartesian/3,
        cartesian3/4,
		cartesian3/2,
        subst_value/4,
        subst_value/3,
        subst_value_list/3,
        range_list/2,
		collection_of_nonempty_sets/2
        ]).
 
:-use_module(library(random)).
:-use_module(library(aggregate)).
:-use_module('binops.pro').
:-use_module('listpreds.pro').

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


powerset(X,P) :- findall(SubSorted,(has_subset(X,Sub),sort(Sub,SubSorted)),L),list_to_set(L,P).

collection_of_nonempty_sets(C,X) :- is_set(X), \+X=[], 
	powerset(X,P), !, 
	subtract(P,[[]],NP), 
	has_subset(NP,S), \+S=[], 
	sort(S,C).


relation(Dom,Rng,R) :- cartesian(Dom,Rng,Cart), powerset(Cart,P), member(R,P).
 

 
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

             
subst_value(Y,Z,[X,Y],[X,Z]).
subst_value(Z,[X,_],[X,Z]).
 
subst_value_list([],List,List).
subst_value_list([H|T],[[H1,_]|T1],[[H1,H]|T2]):-
             subst_value_list(T,T1,T2).
