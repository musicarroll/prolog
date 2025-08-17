:-module('binops',
        [
        gen_binop/2,
        gen_nth_binop/3,
		binop12/2,
        index_to_word/4,
        gen_nth_binop_lex/4,
        domain_values_to_binop/3,
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
        show_eclasses/1,
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
:-use_module('octonions.pro').

% Converts a ternary relation to a binary one by bundling the first two
% elements into a single pair.
% binop12/2 - defines the binop12 predicate.
binop12(R3,R2) :- 
    findall([[X,Y],Z],member([X,Y,Z],R3),L), 
    list_to_set(L,R2),
    length(L,M),
    length(R2,N),
    M=N.
 
% gen_binop/2 - defines the gen binop predicate.
gen_binop(A,BinOp):-
             cartesian(A,Cart),
% gen_func/3 - defines the gen func predicate.
             gen_func(Cart,A,BinOp).

% gen_all_binops/2 - defines the gen all binops predicate.
gen_all_binops(A,BinOps):-
    cartesian(A,Cart),!,
% gen_all_funcs/3 - defines the gen all funcs predicate.
    gen_all_funcs(Cart,A,BinOps).

% gen_nth_binop/3 - defines the gen nth binop predicate.
gen_nth_binop(A, N, BinOp) :-
                gen_all_binops(A,BinOps),
% nth1/3 - defines the nth1 predicate.
                nth1(N, BinOps, BinOp).


% Helper predicate to convert an index to a base-N number represented as a list of digits.
% index_to_base_n_digits/3 - defines the index to base n digits predicate.
index_to_base_n_digits(0, _, []) :- !.
% index_to_base_n_digits/3 - defines the index to base n digits predicate.
index_to_base_n_digits(Index, BaseLen, [Digit|Rest]) :-
    Index > 0,
    Digit is Index mod BaseLen,
    NewIndex is Index div BaseLen,
% index_to_base_n_digits/3 - defines the index to base n digits predicate.
    index_to_base_n_digits(NewIndex, BaseLen, Rest).

% Converts the list of digits to the corresponding elements in set A.
% digits_to_elements/3 - defines the digits to elements predicate.
digits_to_elements([], _, []).
% digits_to_elements/3 - defines the digits to elements predicate.
digits_to_elements([Digit|Digits], A, [Element|Elements]) :-
    nth0(Digit, A, Element),
% digits_to_elements/3 - defines the digits to elements predicate.
    digits_to_elements(Digits, A, Elements).

% Ensures the word has the correct length by prepending the necessary number of 'first elements' of A.
% pad_word/4 - defines the pad word predicate.
pad_word(Word, Base, A, PaddedWord) :-
    length(Word, Len),
    Diff is Base - Len,
    first_element_padding(Diff, A, Padding),
% append/3 - defines the append predicate.
    append(Padding, Word, PaddedWord).

% first_element_padding/3 - defines the first element padding predicate.
first_element_padding(0, _, []) :- !.
% first_element_padding/3 - defines the first element padding predicate.
first_element_padding(Count, A, [First|Rest]) :-
    Count > 0,
    nth0(0, A, First),  % Assumes A is not empty.
    NewCount is Count - 1,
% first_element_padding/3 - defines the first element padding predicate.
    first_element_padding(NewCount, A, Rest).

% Main predicate to convert an index to its corresponding word.
% index_to_word/4 - defines the index to word predicate.
index_to_word(Index, Base, A, Word) :-
    length(A, BaseLen),
    index_to_base_n_digits(Index, BaseLen, DigitsReversed),
    reverse(DigitsReversed, Digits),
    digits_to_elements(Digits, A, TempWord),
% pad_word/4 - defines the pad word predicate.
    pad_word(TempWord, Base, A, Word).

% Generates the Nth binary operation in lexicographic order
% gen_nth_binop_lex(A, N, Base, BinOp) :-
%     length(A, Len),
%     TotalElements is Len^Base,
%     Index is (N - 1) mod TotalElements,
%     index_to_word(Index, Base, A, BinOp).

% gen_nth_binop_lex/4 - defines the gen nth binop lex predicate.
gen_nth_binop_lex(A, N, Base, BinOp) :-
    Index is N - 1,
% index_to_word/4 - defines the index to word predicate.
    index_to_word(Index, Base, A, BinOp).

% word_to_index/4 - defines the word to index predicate.
word_to_index(BinOp, Base, A, N) :-
        length(BinOp, Length),
        % Ensure BinOp has the correct length as per Base
        Length == Base,
        word_to_index(BinOp, A, 0, Index),
        N is Index + 1.
           
% word_to_index/4 - defines the word to index predicate.
word_to_index([], _, Accum, Accum).
% word_to_index/4 - defines the word to index predicate.
word_to_index([H|T], A, Accum, Index) :-
        nth0(Pos, A, H),
        length(A, Len),
        length(T, Remaining),
        Accum1 is Accum + Pos * Len^Remaining,
% word_to_index/4 - defines the word to index predicate.
        word_to_index(T, A, Accum1, Index).
    
% domain_values_to_binop/3 - defines the domain values to binop predicate.
domain_values_to_binop(Set,Vals,BinOp):-
    cartesian(Set,Set,Cart),
% make_pair_list/3 - defines the make pair list predicate.
    make_pair_list(Cart,Vals,BinOp).

% Predicates for implementing Light's procedure for checking associativity.

% binop_star/3 - defines the binop star predicate.
binop_star(BinOp,A, BinOpStar):-
    findall([[X,Y],Z],(member([[A,Y],U],BinOp), member([[X,U],Z],BinOp) ),BinOpStar).

% binop_circle/3 - defines the binop circle predicate.
binop_circle(BinOp,A, BinOpCircle):-
    findall([[X,Y],Z],(member([[X,A],U],BinOp), member([[U,Y],Z],BinOp) ),BinOpCircle).
        
% assoc_for_elem/2 - defines the assoc for elem predicate.
assoc_for_elem(BinOp,A):-
    binop_star(BinOp,A,Star),
    binop_circle(BinOp,A, Circle),
    ?=(Star,Circle).

%permute_binop(Bop1,Bop2):-
 
% assoc_binop_for/4 - defines the assoc binop for predicate.
assoc_binop_for(BinOp,X,Y,Z) :- eval(BinOp,[X,Y],Left),
			eval(BinOp,[Left,Z],Result),
			eval(BinOp,[Y,Z],Right),
% eval/4 - defines the eval predicate.
			eval(BinOp,[X,Right],Result).

% Assumes BinOp is given:
% is_assoc_triple_for/4 - defines the is assoc triple for predicate.
is_assoc_triple_for([X,X,X],BinOp) :- domain(BinOp,D), member([X,X],D).
% is_assoc_triple_for/4 - defines the is assoc triple for predicate.
is_assoc_triple_for([X,Y,Z],BinOp) :- assoc_binop_for(BinOp,X,Y,Z).

% Generate minimal binop elements to ensure that [X,Y,Z] is an associative
% triple:
% is_assoc_triple_on([X,Y,Z],A,BinOp1,BinOp2) :- cartesian3(A,A,A,Cart3),
%            member([[X,Y],L],BinOp1)
% is_assoc_binop/2 - defines the is assoc binop predicate.
is_assoc_binop(BinOp,A) :- cartesian3(A,A,A, Cart3),
			forall(member([X,Y,Z],Cart3),assoc_binop_for(BinOp,X,Y,Z)).

% gen_assoc_binop/2 - defines the gen assoc binop predicate.
gen_assoc_binop(A,BinOp) :- gen_binop(A,BinOp), is_assoc_binop(BinOp,A).

% non_assoc_binop/1 - defines the non assoc binop predicate.
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

% has_identity/2 - defines the has identity predicate.
has_identity(BinOp,Id):-
             binop_domain(BinOp,Dom),
             member(E,Dom),
             findall(B,(member(B,Dom),eval(BinOp,[E,B],B),eval(BinOp,[B,E],B)),L),
             list_to_set(L,S),
             S==Dom,
             E=Id.

% has_inverse_for/4 - defines the has inverse for predicate.
has_inverse_for(BinOp,X,Y,Id) :- var(Id), has_identity(BinOp,Id), eval(BinOp,[X,Y],Id). 
% has_inverse_for/4 - defines the has inverse for predicate.
has_inverse_for(BinOp,X,Y,Id) :- nonvar(Id), eval(BinOp,[X,Y],Id). 

% has_inverses/3 - defines the has inverses predicate.
has_inverses(BinOp,A,Id) :- var(Id),
			has_identity(BinOp,Id),
			forall(member(X,A),(member(Y,A),has_inverse_for(BinOp,X,Y,Id))).
			
% has_inverses/3 - defines the has inverses predicate.
has_inverses(BinOp,A,Id) :- nonvar(Id),
			forall(member(X,A),(member(Y,A),has_inverse_for(BinOp,X,Y,Id))).
	
 
% binop_domain/2 - defines the binop domain predicate.
binop_domain(BinOp,Dom):-
             domain(BinOp,D),
             flatten(D,D1),
% list_to_set/2 - defines the list to set predicate.
             list_to_set(D1,Dom).
 
% non_commute_binop/1 - defines the non commute binop predicate.
non_commute_binop(BinOp):-
             binop_domain(BinOp,Dom),
             cartesian(Dom,Cart),
             member([X,Y],Cart),
             eval(BinOp,[X,Y],Z1),
             eval(BinOp,[Y,X],Z2),
             Z1\==Z2.
 
% left_mults/3 - defines the left mults predicate.
left_mults(X,Bop,Mults):-
             binop_domain(Bop,Dom),
             findall(Z,(member(Y,Dom),eval(Bop,[X,Y],Z)),Mults).

%left_mult_onto(BinOp):- domain(BinOp,Dom), range(BinOp,Rng),
%                        forall

% write_op_table/1 - defines the write op table predicate.
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
% binop_equiv/5 - defines the binop equiv predicate.
binop_equiv(BinOp,[X1,Y1],[X2,Y2]) :- eval(BinOp,[X1,Y1],Result), eval(BinOp,[X2,Y2],Result).

% binop_equiv_class/4 - defines the binop equiv class predicate.
binop_equiv_class(BinOp,[X,Y],Class) :- domain(BinOp,Dom),member([X,Y],Dom),
                                        findall(Result,binop_equiv(BinOp,[X,Y],Result),L), list_to_set(L,Class).

% binop_equiv_classes/2 - defines the binop equiv classes predicate.
binop_equiv_classes(BinOp,EquivClasses) :- findall(Class, 
                                            (domain(BinOp,Dom),member([X,Y],Dom),
                                            findall(Result,binop_equiv(BinOp,[X,Y],Result),L), 
                                                list_to_set(L,Class)),Tmp), list_to_set(Tmp, EquivClasses).

% Test to generate equivalence classes for binops:
% show_eclasses/1 - defines the show eclasses predicate.
show_eclasses(A) :- 
    gen_binop(A,B), 
    binop_equiv_classes(B,EquivClasses),
    length(EquivClasses,M),
    length(A,N),
    M=N,
    write('binop:'),nl,
    write_list_vert(B),
    nl,
    write('equiv classes:'),nl, 
    write_list_vert(EquivClasses), 
    write('num of eclasses:'),nl, write(M).


% klein4/1 - defines the klein4 predicate.
klein4(V) :- V=[[[e,e],e],[[e,a],a],[[e,b],b],[[e,c],c],
                [[a,e],a],[[a,a],e],[[a,b],c],[[a,c],b],
                [[b,e],b],[[b,a],c],[[b,b],e],[[b,c],a],
                [[c,e],c],[[c,a],b],[[c,b],a],[[c,c],e]].

% sym3/1 - defines the sym3 predicate.
sym3(S3) :- S3=[ [ [(1),(1)],   (1)], [[(1),  (12)], (12)],[[(1),(13)],(13)],[[(1),(23)],(23)],[[(1),(123)],(123)],[[(1),(132)],(132)],
                [[(12),(1)],  (12)], [[(12), (12)],  (1)],[[(12),(13)],(132)],[[(12),(23)],(123)],[[(12),(123)],(23)],[[(12),(132)],(13)],
                [[(13),(1)],  (13)], [[(13), (12)],(123)],[[(13),(13)],(1)],[[(13),(23)],(132)],[[(13),(123)],(12)],[[(13),(132)],(23)],
                [[(23),(1)],  (23)], [[(23), (12)],(132)],[[(23),(13)],(123)],[[(23),(23)],(1)],[[(23),(123)],(13)],[[(23),(132)],(12)],
                [[(123),(1)],(123)], [[(123),(12)], (13)],[[(123),(13)],(23)],[[(123),(23)],(12)],[[(123),(123)],(132)],[[(123),(132)],(1)],
                [[(132),(1)],(132)], [[(132),(12)], (23)],[[(132),(13)],(12)],[[(132),(23)],(13)],[[(132),(123)],(1)],[[(132),(132)],(123)]].                

% dih4/1 - defines the dih4 predicate.
dih4(D4) :- D4=[[[e,e],e],        [[e,b],b],     [[e,a],a],     [[e,'a^2'],'a^2'], [[e,'a^3'],'a^3'],[[e,ab],ab],   [[e,'a^2b'],'a^2b'],[[e,'a^3b'],'a^3b'],
                [[b,e],b],        [[b,b],e],     [[b,a],'a^3b'],[[b,'a^2'],'a^2b'],[[b,'a^3'],ab],   [[b,ab],'a^3'],[[b,'a^2b'],'a^2'],[[b,'a^3b'],a],
                [[a,e],a],        [[a,b],ab],    [[a,a],'a^2'], [[a,'a^2'],'a^3'], [[a,'a^3'],e],    [[a,ab],'a^2b'],  [[a,'a^2b'],'a^3b'],[[a,'a^3b'],b],
                [['a^2',e],'a^2'],[['a^2',x],x], [['a^2',x],x],     [['a^2',x],x],[['a^2',x],x],[['a^2',x],x],[['a^2',x],x],[['a^2',x],x],
                [['a^3',e],'a^3'],[['a^3',x],x],[['a^3',x],x],[['a^3',x],x],[['a^3',x],x],[['a^3',x],x],[['a^3',x],x],[['a^3',x],x],
                [[ab,e],ab],[[ab,x],x],[[ab,x],x],[[ab,x],x],[[ab,x],x],[[ab,x],x],[[ab,x],x],[[ab,x],x],
                [['a^2b',e],'a^2b'],[['a^2b',x],x],[['a^2b',x],x],[['a^2b',x],x],[['a^2b',x],x],[['a^2b',x],x],[['a^2b',x],x],[['a^2b',x],x],
                [['a^3b',e],'a^b'],[['a^3b',x],x],[['a^3b',x],x],[['a^3b',x],x],[['a^3b',x],x],[['a^3b',x],x],[['a^3b',x],x],[['a^3b',x],x]
                ].
% z3/1 - defines the z3 predicate.
z3(Z3):- Z3=[ 
                [[0,0],0], [[0,1],1], [[0,2],2],
                [[1,0],1], [[1,1],2], [[1,2],0],
                [[2,0],2], [[2,1],0], [[2,2],1]
            ].

% z4/1 - defines the z4 predicate.
z4(Z4):- Z4=[ 
                [[0,0],0], [[0,1],1], [[0,2],2], [[0,3],3],
                [[1,0],1], [[1,1],2], [[1,2],3], [[1,3],0],
                [[2,0],2], [[2,1],3], [[2,2],0], [[2,3],1],
                [[3,0],3], [[3,1],0], [[3,2],1], [[3,3],2]
            ].
