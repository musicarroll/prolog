:-module('functions',
        [
		has_subset/2,
        has_subset_of_size/3,
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
        gen_bijection/2,
		gen_all_funcs/3,
		choose_val/2,
		gen_choice_func/3,
        eval/3,
        eval_on_set/3,
        make_pair_func/2,
        stabilizer_of/3,
        gen_automorph_rel/3,
        full_automorph_rel/3,
        full_automorph_from_sset_vset/4,
        is_automorph_of/2,
        all_automorphs_of/3,
        order_set_and_cartesian/3,
        cartesian_product/3,
        cartesian/2,
        cartesian/3,
        cartesian3/4,
		cartesian3/2,
        subst_value/4,
        subst_value/3,
        subst_value_list/3,
        range_list/2,
		collection_of_nonempty_sets/2,
        test1/4,
        test_bijections/0,
        test_bijections/3,
        test_all_bijections/0,
        write_rel_bij/2,
        is_function/1,
        is_bijection/1,
        is_bijection/3,
        subsets/2,
        all_subsets/2,
        all_bijections/3,
        set_equal/2
        ]).
 
:-use_module(library(random)).
:-use_module(library(aggregate)).
:-use_module('binops.pro').
:-use_module('listpreds.pro').

set_equal(A,B):- subset(A,B), subset(B,A).


% Subsets, from ChatGPT
% generate subsets of a list
subsets([], []).
subsets([X|Xs], [X|Ys]) :- subsets(Xs, Ys).
subsets([_|Xs], Ys) :- subsets(Xs, Ys).

% generate all subsets of a set
all_subsets(Set, Subsets) :-
    list_to_set(Set, SetUnique),
    findall(Subset, (subsets(SetUnique, Subset), length(Subset, N), N > 0), Subsets).

single_valued(X, Pairs) :-
    findall(Y, member([X,Y], Pairs), Ys),
    list_to_set(Ys, Set),
    length(Set, 1).

is_bijection(Pairs) :-
    is_function(Pairs),
    is_injection(Pairs),
    is_surjection(Pairs).

is_bijection(Pairs,Dom,Rng) :-
    is_function(Pairs),
    domain(Pairs,D), set_equal(D,Dom),
    range(Pairs,R), set_equal(R,Rng),
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
    
% Bactrackable subset predicate Volodymyr Ostruk (stackoverflow, Jan 1, 2016)
% Superceded by all_subsets, and its logic.

has_subset(_, []).
has_subset([X|L], [A|NTail]):-
    member(A,[X|L]),    
    has_subset(L, NTailtmp),
    sort(NTailtmp,NTail),
    not(member(A, NTail)).

is_subset([], A) :- is_set(A).
is_subset([E|Tail], [E|NTail]):-
  is_subset(Tail, NTail).
is_subset([_|Tail], NTail):-
  is_subset(Tail, NTail).

% Generate the power set of Set
powerset(Set, PowerSet) :-
    length(Set, Length),
    Limit is 2^Length,
    UpperBound is Limit - 1, % Ensure arithmetic evaluation
    findall(Subset, (between(0, UpperBound, Index), index_to_subset(Index, Set, Subset)), PowerSet).

% Map an index to a subset of Set based on the binary representation of the index
index_to_subset(Index, Set, Subset) :-
    binary_rep(Index, Set, [], RevSubset),
    reverse(RevSubset, Subset).

% Generate a subset from a binary representation
binary_rep(0, [], Subset, Subset) :- !.
binary_rep(Index, [Element|Set], Acc, Subset) :-
    Bit is Index mod 2,
    NextIndex is Index // 2,
    ( Bit == 1 -> NewAcc = [Element|Acc] ; NewAcc = Acc ),
    binary_rep(NextIndex, Set, NewAcc, Subset).


%powerset(X,P) :- findall(SubSorted,(has_subset(X,Sub),sort(Sub,SubSorted)),L),list_to_set(L,P).
% powerset(X,P) :- all_subsets(X,P).


% Seems to produce duplicate subsets:
%has_subset_of_size(A,S,N) :- ground(A), ground(N),has_subset(A,Stmp),sort(Stmp,S), length(S,N).


% Works but very inefficient.
has_subset_of_size(A,S,N) :- ground(A), ground(N), powerset(A,P), 
                            findall(Stmp,(member(Stmp,P),length(Stmp,N)),L), 
                            list_to_set(L,SoS), !, member(S,SoS).

collection_of_nonempty_sets(C,X) :- is_set(X), \+X=[], 
	powerset(X,P), !, 
	subtract(P,[[]],NP), 
	has_subset(NP,S), \+S=[], 
	sort(S,C).

% Utility predicate that orders a set and generates its ordered Cartesian product.
order_set_and_cartesian(Set, OrderedSet, OrderedCartesian) :-
    sort(Set, OrderedSet),
    cartesian_product(OrderedSet, OrderedSet, OrderedCartesian).
    % sort(Cartesian, OrderedCartesian).

% Generate the Cartesian product of two sets.
cartesian_product([], _, []).
cartesian_product([A|As], Bs, Product) :-
    pair_with_all(A, Bs, Paired),
    cartesian_product(As, Bs, Rest),
    append(Paired, Rest, Product).

% Pair an element with all elements of a set.
pair_with_all(_, [], []).
pair_with_all(A, [B|Bs], [[A,B]|Paired]) :-
    pair_with_all(A, Bs, Paired).

% Older, less efficient implementations:
cartesian(A,B,Cart):-
             findall([X,Y],(member(X,A),member(Y,B)),L),
             list_to_set(L,Cart),!.
 
cartesian(A,Cart):- cartesian_product(A,A,Cart).


% Recursive definition to compute cartesian product of three lists
cartesian_product3([], _, _, []).
cartesian_product3([A|As], B, C, Cart) :-
    pair_with_all_bc(A, B, C, PairedWithA),
    cartesian_product3(As, B, C, RestCart),
    append(PairedWithA, RestCart, Cart).

% Pair element A with each element in B and C
pair_with_all_bc(_, [], _, []).
pair_with_all_bc(A, [B|Bs], C, PairedWithA) :-
    pair_with_all_c(A, B, C, PairedWithAB),
    pair_with_all_bc(A, Bs, C, RestPairs),
    append(PairedWithAB, RestPairs, PairedWithA).

% Pair elements A and B with each element in C
pair_with_all_c(_, _, [], []).
pair_with_all_c(A, B, [C|Cs], [[A,B,C]|Rest]) :-
    pair_with_all_c(A, B, Cs, Rest).


% Older implementation:
% cartesian3(A,B,C,Cart):-
%              findall([X,Y,Z],(member(X,A),member(Y,B),member(Z,C)),L),
%              list_to_set(L,Cart),!.

% Cartesian product of three sets
cartesian3(A, B, C, Cart) :-
    cartesian_product3(A, B, C, Cart).

cartesian3(A,Cart) :- cartesian3(A,A,A,Cart).


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
eval_on_set(F,S1,S2) :-
    findall(Y,(member(X,S1),eval(F,X,Y)),L), list_to_set(L,S2),!.

eval_on_pair(F,[X,Y],[RX,RY]):- eval(F,X,RX), eval(F,Y,RY).

make_pair_func(F,Pair_Func) :- domain(F,D),cartesian(D,C), 
    findall([[X,Y],[FX,FY]],(member([X,Y],C),eval(F,X,FX),eval(F,Y,FY)),L),
    list_to_set(L,Pair_Func), !.

gen_bijection(DomRng,F) :-
    permutation(DomRng,Rng), 
    make_pair_list(DomRng,Rng,F).

stabilizer_of(BaseSet,Pair,Stabilizer) :- cartesian(BaseSet,BaseSet,Cart),
    member(Pair,Cart), RandSub=[Pair], 
    findall(Sigma,(gen_bijection(BaseSet,Sigma),is_automorph_of(Sigma,RandSub)),Stabilizer).


is_automorph_of(F,R):- forall(member([X,Y],R),
                        (eval_on_pair(F,[X,Y],[FX,FY]),member([FX,FY],R))).
all_automorphs_of(R,SuperDom,Autos):- 
                findall(G,(gen_bijection(SuperDom,G),is_automorph_of(G,R)),L),
                list_to_set(L,Autos).

gen_automorph_rel([X0,Y0],F,R) :- gen_automorph_rel([X0,Y0],[],F,[],R).

gen_automorph_rel([X0,Y0],[X0,Y0],_,R,R).
gen_automorph_rel([X0,Y0],Pair,F,Rinc,R) :- 
    (
     Pair=[]-> eval_on_pair(F,[X0,Y0],[FX,FY])
    ;
     Pair=[X,Y],
     eval_on_pair(F,[X,Y],[FX,FY])
     ),
    Rnew = [[FX,FY]|Rinc],
    list_to_set(Rnew,Snew),
    gen_automorph_rel([X0,Y0],[FX,FY],F,Snew,R).

full_automorph_rel(SeedSet,F,Rel):-
    make_pair_func(F,PF),
    full_automorph_rel(SeedSet,PF,SeedSet,[],Rel).

full_automorph_rel(_,_,Rel,Rel,Rel):-!.
full_automorph_rel(SeedSet,PF,LastSet,NewSet,Rel):-
    eval_on_set(PF,LastSet,Temp),
    list_to_set(Temp,NewSetTmp), 
    union(LastSet,NewSetTmp,Union),
    union(NewSet,Union,NextNewSet),
    full_automorph_rel(SeedSet,PF,NewSet,NextNewSet,Rel).


             
subst_value(Y,Z,[X,Y],[X,Z]).
subst_value(Z,[X,_],[X,Z]).
 
subst_value_list([],List,List).
subst_value_list([H|T],[[H1,_]|T1],[[H1,H]|T2]):-
             subst_value_list(T,T1,T2).

full_automorph_from_sset_vset(SeedSet,VertexSet,Bijection,FullRelation):- 
    gen_bijection(VertexSet,Bijection), 
    full_automorph_rel(SeedSet,Bijection,FullRelation).

is_function(Relation) :-
    % Create a list of all the X values
    get_domain(Relation, Domain),
    % Check if each X value maps to a unique Y value
    check_uniqueness(Relation, Domain).

% Given a relation, return a list of all the X values
get_domain(Relation, Domain) :-
    % Use findall to collect all the unique X values in the relation
    findall(X, member([X,_], Relation), DomainList),
    % Use list_to_set to remove duplicates
    list_to_set(DomainList, Domain).

% Given a relation and a list of X values, check if each X value maps to a unique Y value
check_uniqueness(_, []) :- !.
check_uniqueness(Relation, [X|Xs]) :-
    % Use findall to collect all the Y values that map to X
    findall(Y, member([X,Y], Relation), YList),
    % Use list_to_set to remove duplicates
    list_to_set(YList, UniqueY),
    % If there's only one Y value, move on to the next X value
    length(UniqueY, 1),
    check_uniqueness(Relation, Xs).

% Experiments:
test1(SeedSet,VertexSet,Bijection,FullRelation):-
    full_automorph_from_sset_vset(SeedSet,VertexSet,Bijection,FullRelation), 
    nl,nl,write('*****'), nl, 
    write('Seed Set: '),write(SeedSet),nl,
    write('Vertex Set: '),
    write(VertexSet),nl,
    write('Bijection: '),
    write(Bijection),nl,
    write('Full Relation: '),write(FullRelation),nl,nl.
test_bijections:- test_bijections([[a,b,c],R,Bs]),
    write_rel_bij(R,Bs).

test_bijections(A,R,Bs):- 
        all_bijections(A,A,Bijections),
        cartesian(A,Cart),
        all_subsets(Cart,Subsets),
        member(R,Subsets),
        findall(Bij,
            (member(Bij,Bijections),
            make_pair_func(Bij,PairFunc),
            eval_on_set(PairFunc,R,S),
            set_equal(R,S)),L),
        list_to_set(L,Bs).

write_rel_bij(R,Bs):-
        write('Relation R:'),nl,
        write_list_vert(R), 
        write('Bijections: '),nl,
        write_list_vert(Bs).

all_bijections(A,B,Bs):- cartesian(A,B,C),
    all_subsets(C,Subsets),
    findall(F,
        (member(F,Subsets),
        is_bijection(F,A,B)),
    L),
    list_to_set(L,Bs).

test_all_bijections:- all_bijections([a,b,c],[a,b,c],Bs),
    write_list_vert(Bs).
