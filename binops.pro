:-module('binops',
        [
        gen_binop/2,
		binop12/2,
        binop_star/3,
        binop_circle/3,
        assoc_for_elem/2,
        gen_assoc_binop/2,
        non_assoc_binop/1,
		assoc_binop_for/4,
        is_assoc_triple_for/2,
		is_assoc_binop/2,
        has_identity/2,
		has_inverse_for/4,
		has_inverses/3,
        non_commute_binop/1,
        left_mults/3,
        binop_equiv_class/3,
        binop_equiv_classes/2,
		write_op_table/1,
        binop_equiv/3,
        klein4/1,
        sym3/1,
        dih4/1,
        z3/1,
        z4/1
        ]).

:-use_module(library(random)).
:-use_module(library(aggregate)).
:-use_module('functions.pro').
:-use_module('listpreds.pro').

binop12(R3,R2) :- findall([[X,Y],Z],member([X,Y,Z],R3),L), list_to_set(L,R2),length(L,M),length(R2,N),M=N.
 
gen_binop(A,BinOp):-
             cartesian(A,Cart),
             gen_func(Cart,A,BinOp).

% Predicates for implementing Light's procedure for checking associativity.

binop_star(BinOp,A, BinOpStar):-
    findall([[X,Y],Z],(member([[A,Y],U],BinOp), member([[X,U],Z],BinOp) ),BinOpStar).

binop_circle(BinOp,A, BinOpCircle):-
    findall([[X,Y],Z],(member([[X,A],U],BinOp), member([[U,Y],Z],BinOp) ),BinOpCircle).
        
assoc_for_elem(BinOp,A):-
    binop_star(BinOp,A,Star),
    binop_circle(BinOp,A, Circle),
    ?=(Star,Circle).

%permute_binop(Bop1,Bop2):-
 
assoc_binop_for(BinOp,X,Y,Z) :- eval(BinOp,[X,Y],Left),
			eval(BinOp,[Left,Z],Result),
			eval(BinOp,[Y,Z],Right),
			eval(BinOp,[X,Right],Result).

% Assumes BinOp is given:
is_assoc_triple_for([X,X,X],BinOp) :- domain(BinOp,D), member([X,X],D).
is_assoc_triple_for([X,Y,Z],BinOp) :- assoc_binop_for(BinOp,X,Y,Z).

% Generate minimal binop elements to ensure that [X,Y,Z] is an associative
% triple:
% is_assoc_triple_on([X,Y,Z],A,BinOp1,BinOp2) :- cartesian3(A,A,A,Cart3),
%            member([[X,Y],L],BinOp1)
is_assoc_binop(BinOp,A) :- cartesian3(A,A,A, Cart3),
			forall(member([X,Y,Z],Cart3),assoc_binop_for(BinOp,X,Y,Z)).

gen_assoc_binop(A,BinOp) :- gen_binop(A,BinOp), is_assoc_binop(BinOp,A).

non_assoc_binop(BinOp):-
             domain(BinOp,D),
             flatten(D,L), list_to_set(L,A),
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

%left_mult_onto(BinOp):- domain(BinOp,Dom), range(BinOp,Rng),
%                        forall

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
binop_equiv(BinOp,[X1,Y1],[X2,Y2]) :- eval(BinOp,[X1,Y1],Result), eval(BinOp,[X2,Y2],Result).

binop_equiv_class(BinOp,[X,Y],Class) :- domain(BinOp,Dom),member([X,Y],Dom),
                                        findall(Result,binop_equiv(BinOp,[X,Y],Result),L), list_to_set(L,Class).

binop_equiv_classes(BinOp,EquivClasses) :- findall(Class, 
                                            (domain(BinOp,Dom),member([X,Y],Dom),
                                            findall(Result,binop_equiv(BinOp,[X,Y],Result),L), 
                                                list_to_set(L,Class)),Tmp), list_to_set(Tmp, EquivClasses).

klein4(V) :- V=[[[e,e],e],[[e,a],a],[[e,b],b],[[e,c],c],
                [[a,e],a],[[a,a],e],[[a,b],c],[[a,c],b],
                [[b,e],b],[[b,a],c],[[b,b],e],[[b,c],a],
                [[c,e],c],[[c,a],b],[[c,b],a],[[c,c],e]].

sym3(S3) :- S3=[ [ [(1),(1)],   (1)], [[(1),  (12)], (12)],[[(1),(13)],(13)],[[(1),(23)],(23)],[[(1),(123)],(123)],[[(1),(132)],(132)],
                [[(12),(1)],  (12)], [[(12), (12)],  (1)],[[(12),(13)],(132)],[[(12),(23)],(123)],[[(12),(123)],(23)],[[(12),(132)],(13)],
                [[(13),(1)],  (13)], [[(13), (12)],(123)],[[(13),(13)],(1)],[[(13),(23)],(132)],[[(13),(123)],(12)],[[(13),(132)],(23)],
                [[(23),(1)],  (23)], [[(23), (12)],(132)],[[(23),(13)],(123)],[[(23),(23)],(1)],[[(23),(123)],(13)],[[(23),(132)],(12)],
                [[(123),(1)],(123)], [[(123),(12)], (13)],[[(123),(13)],(23)],[[(123),(23)],(12)],[[(123),(123)],(132)],[[(123),(132)],(1)],
                [[(132),(1)],(132)], [[(132),(12)], (23)],[[(132),(13)],(12)],[[(132),(23)],(13)],[[(132),(123)],(1)],[[(132),(132)],(123)]].                

dih4(D4) :- D4=[[[e,e],e],        [[e,b],b],     [[e,a],a],     [[e,'a^2'],'a^2'], [[e,'a^3'],'a^3'],[[e,ab],ab],   [[e,'a^2b'],'a^2b'],[[e,'a^3b'],'a^3b'],
                [[b,e],b],        [[b,b],e],     [[b,a],'a^3b'],[[b,'a^2'],'a^2b'],[[b,'a^3'],ab],   [[b,ab],'a^3'],[[b,'a^2b'],'a^2'],[[b,'a^3b'],a],
                [[a,e],a],        [[a,b],ab],    [[a,a],'a^2'], [[a,'a^2'],'a^3'], [[a,'a^3'],e],    [[a,ab],'a^2b'],  [[a,'a^2b'],'a^3b'],[[a,'a^3b'],b],
                [['a^2',e],'a^2'],[['a^2',x],x], [['a^2',x],x],     [['a^2',x],x],[['a^2',x],x],[['a^2',x],x],[['a^2',x],x],[['a^2',x],x],
                [['a^3',e],'a^3'],[['a^3',x],x],[['a^3',x],x],[['a^3',x],x],[['a^3',x],x],[['a^3',x],x],[['a^3',x],x],[['a^3',x],x],
                [[ab,e],ab],[[ab,x],x],[[ab,x],x],[[ab,x],x],[[ab,x],x],[[ab,x],x],[[ab,x],x],[[ab,x],x],
                [['a^2b',e],'a^2b'],[['a^2b',x],x],[['a^2b',x],x],[['a^2b',x],x],[['a^2b',x],x],[['a^2b',x],x],[['a^2b',x],x],[['a^2b',x],x],
                [['a^3b',e],'a^b'],[['a^3b',x],x],[['a^3b',x],x],[['a^3b',x],x],[['a^3b',x],x],[['a^3b',x],x],[['a^3b',x],x],[['a^3b',x],x]
                ].
z3(Z3):- Z3=[ 
                [[0,0],0], [[0,1],1], [[0,2],2],
                [[1,0],1], [[1,1],2], [[1,2],0],
                [[2,0],2], [[2,1],0], [[2,2],1]
            ].

z4(Z4):- Z4=[ 
                [[0,0],0], [[0,1],1], [[0,2],2], [[0,3],3],
                [[1,0],1], [[1,1],2], [[1,2],3], [[1,3],0],
                [[2,0],2], [[2,1],3], [[2,2],0], [[2,3],1],
                [[3,0],3], [[3,1],0], [[3,2],1], [[3,3],2]
            ].
