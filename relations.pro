:-use_module('functions.pro').

r(X,Y) :- member([X,Y],[[a,b],[a,c],[b,c],[c,d],[d,d]]).
s(X,Y) :- member([X,Y],[[a,a],[b,c],[b,d]]).
f(X,Y) :- member([X,Y], [[a,a],[b,a],[c,a],[d,a]]).
t(X,Y) :- member([X,Y],[[a,a],[e,f],[e,g],[f,g]]).

compose_pair(R,S,[X,Y]) :- call(R,X,Z), call(S,Z,Y).
compose(R,S,C) :- findall(Pair,compose_pair(R,S,Pair),L), list_to_set(L,C).

or_pair(R,S,[X,Y]) :- call(R,X,Y); call(S,X,Y).
pair_union(R,S,U) :- union(R,S,U).

and_pair(R,S,[X,Y]) :- call(R,X,Y), call(S,X,Y).
pair_intersect(R,S,I) :- intersection(R,S,I).

rel_compl_pair(R,S,[X,Y]) :- call(R,X,Y), \+call(S,Y,Y).
rel_compl(R,S,C) :- findall(P,rel_compl_pair(R,S,P),L), list_to_set(L,C).

% Upper Image (Suppes and Zanotti) or X-Section (Halmos)
xR(X,R,S) :- findall(Y,call(R,X,Y),L), list_to_set(L,S).

% Suppes Example of Two Coins, one new and the other old and worn and easy to misread:
% f2 = "flip two coins"
q_pair(X,Y) :- member([X,Y],[[f2,hh],[f2,ht],[f2,th],[f2,tt]]).
% r_suppes counts the number of heads, but imperfectly, whearas s_suppes is perfect:
r_suppes(X,Y) :- member([X,Y],[[hh,1],[hh,2],[ht,1],[ht,2],[th,0],[th,1],[tt,0],[tt,1]]).
r_suppes_set(Set) :- findall([X,Y],r_suppes(X,Y),L), list_to_set(L,Set).

s_suppes(X,Y) :- member([X,Y],[[hh,2],[ht,1],[th,1],[tt,0]]).  % Deterministic RV functions
s_suppes_set(Set) :- findall([X,Y],s_suppes(X,Y),L), list_to_set(L,Set).

relation_set(R,Set) :- findall([X,Y],call(R,X,Y),L), list_to_set(L,Set).
sections(R,Sections) :- relation_set(R,Set), domain(Set,D), findall(Section,(member(X,D),xR(X,R,Section)),L),list_to_set(L,Sections).

% Predicates for writing graphs (graphviz)

% open / close / headers / footers
open_dot_file(Filename, Fd) :- open(Filename, write, Fd), 
	write(Fd, 'digraph G {\n'),
%	write(Fd, 'size =\"10,10\";\n'),
	write(Fd, 'layout=circo;\n').

close_dot_file(Fd) :- write(Fd,'}\n'), close(Fd).

% Write binary relation pairs to dot file:
write_dot_pair([X,Y],Fd) :- write(Fd,X), write(Fd,'->'), write(Fd,Y), write(Fd,';\n').
write_dot_pairs(Pairs,Fd) :- forall(member([X,Y],Pairs),write_dot_pair([X,Y],Fd)).

% Write ternary relations to dot file:
% Build ordered lists for domain and range:
write_ternary_dot_pair([X,Y],Dom,Rng,Fd) :- 
	nth0(Xindex,Dom,X), atom_concat('a',Xindex,Xlabel), 
	nth0(Yindex,Rng,Y), atom_concat('b',Yindex,Ylabel),
	write(Fd,Xlabel), write(Fd,'->'), 
	write(Fd,Ylabel), write(Fd,';\n'),
	write(Fd,Xlabel), write(Fd, ' '), write(Fd, '[label=\"'), write(Fd, X), write(Fd, '\"]\n'),
	write(Fd,Ylabel), write(Fd, ' '), write(Fd, '[label=\"'), write(Fd, Y), write(Fd, '\"]\n').

write_ternary_dot_pairs(Pairs, Fd) :- domain(Pairs,Dom), range(Pairs,Rng),
		forall(member([X,Y],Pairs),write_ternary_dot_pair([X,Y],Dom,Rng, Fd)).

gen_tern_dot(Rel,Filename) :- open_dot_file(Filename,Fd), 
							write_ternary_dot_pairs(Rel,Fd), 
							close_dot_file(Fd).

rand_sub(Set,M, RandSub) :- length(Set,N), randset(M,N,S), 
	findall(SN,(member(X,S),nth1(X,Set,SN)),RandSub).
bin2tern([[X,Y],Z],[X,Y,Z]).

write_latex_triple([X,Y,Z]) :- write('('),write(X),write(','),write(Y),write(','), write(Z),write(')').

write_latex_triple_set([H|T],0) :- write('\\set{'), write_latex_triple(H), 
								(T=[]->write('}');write(','),write_latex_triple_set(T,1)).
write_latex_triple_set([H|T],1) :-	write_latex_triple(H), 
								(T=[]->write('}');write(','),write_latex_triple_set(T,1)).	
								
gen_dot_filename(Filename) :- get_time(TS), stamp_date_time(TS,Date,local), 
							date_time_value(year,Date,Y),	write(Y),nl,						
							date_time_value(month,Date,M),	write(M),nl,
							date_time_value(day,Date,D),	write(D),nl,
							date_time_value(hour,Date,H),	write(H),nl,				
							date_time_value(minute,Date,Min),	write(M),nl,						
							date_time_value(second,Date,S),	write(S),nl,
							atomics_to_string([Y,M,D,H,Min,S],'-',Fileprefix),
							atom_string(Atom,Fileprefix),
							atom_concat(Atom,'.dot',Filename).
							
gen_rand_graph(BaseSet,M) :-  A=BaseSet,
							cartesian(A,A,Dom), 
							cartesian(Dom,A,Set), 
							rand_sub(Set,M, RandSub), 
%							maplist(bin2tern,RandSub,Triples), 
%							write_latex_triple_set(Triples,0), 
							gen_dot_filename(F), 
							gen_tern_dot(RandSub,F).
							
								