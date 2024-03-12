:-use_module('functions.pro').
:-use_module(library(uuid)).
:-use_module('graphext.pro').
:-use_module('binops.pro').
:-use_module('listpreds.pro').

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
open_dot_file(Filename, Fd, Label) :- open(Filename, write, Fd), 
	write(Fd, 'digraph G {\n'),
	write(Fd,'label=\"'),write(Fd,Label), write(Fd,'\";\n'),
	write(Fd, 'layout=circo;\n'),
	write(Fd, 'rankdir=LR;\n'),
	write(Fd, 'orientation=portrait;\n').
%	write(Fd, 'size =\"4,6\";\n').

close_dot_file(Fd) :- write(Fd,'}\n'), close(Fd).

make_vertex_uuid_pair(V,[V,UUID]) :- uuid(UUID,[format(integer)]).
make_vertex_uuid_pairs(Vs,Pairs) :- maplist(make_vertex_uuid_pair,Vs,Pairs).
writeln(X) :- write(X), nl.
writelist(X) :- maplist(writeln,X).

% Write ternary relations to dot file:
% Build ordered lists for domain and range:
write_ternary_dot_pair([X,Y],Dom,Rng,Fd) :- 
	nth0(Xindex,Dom,X), atom_concat('a',Xindex,Xlabel), 
	nth0(Yindex,Rng,Y), atom_concat('b',Yindex,Ylabel),
	write(Fd,Xlabel), write(Fd,'->'), 
	write(Fd,Ylabel), write(Fd,';\n'),
	list_to_tuple(X,TupleX),
	list_to_tuple(Y,TupleY),
	write(Fd,Xlabel), write(Fd, ' '), write(Fd, '[label=\"'), write(Fd, TupleX), write(Fd, '\"]\n'),
	write(Fd,Ylabel), write(Fd, ' '), write(Fd, '[label=\"'), write(Fd, TupleY), write(Fd, '\"]\n').

write_ternary_dot_pairs(Pairs, Fd) :- domain(Pairs,Dom), range(Pairs,Rng),
		forall(member([X,Y],Pairs),write_ternary_dot_pair([X,Y],Dom,Rng, Fd)).

list_to_tuple([X,Y], Tuple) :-
	!,  % Cut to enforce that the input is a two-element list
	format(atom(Tuple), '(~w,~w)', [X,Y]).
list_to_tuple(Input, Input).
		
gen_tern_dot(Rel,Filename) :- open_dot_file(Filename,Fd,' '), 
							write_ternary_dot_pairs(Rel,Fd), 
							close_dot_file(Fd).

rand_sub(Set,M, RandSub) :- length(Set,N), randset(M,N,S), 
	findall(SN,(member(X,S),nth1(X,Set,SN)),RandSub).

rand_subs3(Set,M,Collection):-
	repeat,
		rand_sub(Set,M,T),rand_sub(Set,M,S),\+S=T,rand_sub(Set,M,R),\+S=R,\+T=R, 
		Collection=[R,T,S],
		(union_collection(Collection,Set)->!;fail).

has_Msubset(X,M,Sub):- has_Msubset(X,M,0,[],[],Sub).
has_Msubset(_,M,M,_,Sub,Sub).
has_Msubset(X,M,M0,SortedSubs,Sub0,Sub):-
	write('SortedSubs before: '), write(SortedSubs),nl,
	member(Y,X),\+member(Y,Sub0),
	union(Sub0,[Y],Sub1),
	sort(Sub1,SortedSub),
	\+member(SortedSub,SortedSubs),
	union([SortedSub],SortedSubs,NewSortedSubs),
	write('SortedSubs after: '), write(NewSortedSubs), nl,
	M1 is M0 + 1,
	has_Msubset(X,M,M1,NewSortedSubs,Sub1,Sub).

bin2tern([[X,Y],Z],[X,Y,Z]).
tern2bin([X,Y,Z],[[X,Y],Z]).

write_latex_triple([X,Y,Z]) :- write('('),write(X),write(','),write(Y),write(','), write(Z),write(')').

write_latex_triple_set([H|T],0) :- write('\\set{'), write_latex_triple(H), 
								(T=[]->write('}');write(','),write_latex_triple_set(T,1)).
write_latex_triple_set([H|T],1) :-	write_latex_triple(H), 
								(T=[]->write('}');write(','),write_latex_triple_set(T,1)).	
								
gen_dot_filename(Filename) :- gen_filename('.dot',Filename).
gen_dot_filename(Path,Filename) :- gen_filename(Path,'.dot',Filename).

gen_filename(Ext, Filename) :- get_time(TS), stamp_date_time(TS,Date,local), 
							date_time_value(year,Date,Y),	write(Y),nl,						
							date_time_value(month,Date,M),	write(M),nl,
							date_time_value(day,Date,D),	write(D),nl,
							date_time_value(hour,Date,H),	write(H),nl,				
							date_time_value(minute,Date,Min),	write(M),nl,						
							date_time_value(second,Date,S),	write(S),nl,
							Secs is floor(S*100),
							atomics_to_string([Y,M,D,H,Min,Secs],'-',Fileprefix),
							atom_string(Atom,Fileprefix),
							atom_concat(Atom,Ext,Filename).

gen_filename(Path,Ext, Filename) :- get_time(TS), stamp_date_time(TS,Date,local), 
							date_time_value(year,Date,Y),	write(Y),nl,						
							date_time_value(month,Date,M),	write(M),nl,
							date_time_value(day,Date,D),	write(D),nl,
							date_time_value(hour,Date,H),	write(H),nl,				
							date_time_value(minute,Date,Min),	write(M),nl,						
							date_time_value(second,Date,S),	write(S),nl,
							Secs is floor(S*100),
							atomics_to_string([Y,M,D,H,Min,Secs],'-',Fileprefix),
							atom_string(Atom,Fileprefix),
							atom_concat(Path,Atom, Atom2),
							atom_concat(Atom2,Ext,Filename).

gen_rand_graph(BaseSet,M, RandSub) :-  A=BaseSet,
							cartesian(A,A,Dom), 
							cartesian(Dom,A,Set), 
							rand_sub(Set,M, RandSub).
%							maplist(bin2tern,RandSub,Triples), 
%							write_latex_triple_set(Triples,0), 

gen_rand_pair_graph(BaseSet,M, RandSub) :-	cartesian(BaseSet,BaseSet,PairSet),
									cartesian(PairSet,PairSet,PairGraph),
									rand_sub(PairGraph,M, RandSub).

make_dot_file(GraphPairs) :- gen_dot_filename(F), 
							open_dot_file(F,Fd,' '),  
							write_dot_pairs(GraphPairs,Fd),
							close_dot_file(Fd).

complete_graph(Set,U) :- cartesian(Set,Set,S2), maplist(pair_to_edge,S2,Es), vertices_edges_to_ugraph([],Es,U).   

% Write binary relation pairs to dot file:
write_dot_pair([X,Y],Fd) :- X=Y, uuid(Xlabel,[format(integer)]),Ylabel=Xlabel,
							write(Fd,Xlabel), write(Fd,'->'), write(Fd,Ylabel), write(Fd,';\n'),
							write(Fd,Xlabel), write(Fd, ' '), write(Fd, '[label=\"'), 
							write(Fd, X), write(Fd, '\"]\n').
write_dot_pair([X,Y],Fd) :- uuid(Xlabel,[format(integer)]),uuid(Ylabel,[format(integer)]),
							write(Fd,Xlabel), write(Fd,'->'), write(Fd,Ylabel), write(Fd,';\n'),
							write(Fd,Xlabel), write(Fd, ' '), write(Fd, '[label=\"'), 
							write(Fd, X), write(Fd, '\"]\n'),
							write(Fd,Ylabel), write(Fd, ' '), write(Fd, '[label=\"'), 
							write(Fd, Y), write(Fd, '\"]\n').

write_dot_pairs(Pairs,Fd) :- forall(member([X,Y],Pairs),write_dot_pair([X,Y],Fd)).
list_to_atom([X,Y],Atom) :- atomic_list_concat(['[',X,',',Y,']'],Atom).
edge_to_dot(X-Y,VPairs,DotEdge) :- member([X,Xuid],VPairs), member([Y,Yuid],VPairs), 
									atomic_list_concat([Xuid,'->',Yuid,';\n'],DotEdge).

ugraph_to_dot(U,Label) :- gen_dot_filename(F), 
					open_dot_file(F,Fd, Label),  
					vertices(U,Vs),
					make_vertex_uuid_pairs(Vs,VPairs),
					maplist(vertex_pair_to_label_string,VPairs,LPairs),
					findall(X,(member(X,LPairs),write(Fd,X)),_),
					edges(U,Es),
					findall(DotEdge, 
						(member(Edge,Es),edge_to_dot(Edge,VPairs,DotEdge),write(Fd,DotEdge)),
						_),
					close_dot_file(Fd).

ugraph_to_dot(U,Label,F) :-  
					open_dot_file(F,Fd, Label),  
					vertices(U,Vs),
					make_vertex_uuid_pairs(Vs,VPairs),
					maplist(vertex_pair_to_label_string,VPairs,LPairs),
					findall(X,(member(X,LPairs),write(Fd,X)),_),
					edges(U,Es),
					findall(DotEdge, 
						(member(Edge,Es),edge_to_dot(Edge,VPairs,DotEdge),write(Fd,DotEdge)),
						_),
					close_dot_file(Fd).


ugraph_to_dot(U,Label,Nonblank,F) :-  
					open_dot_file(F,Fd, Label),  
					vertices(U,Vs),
					make_vertex_uuid_pairs(Vs,VPairs),
					findall(L,
						(member(VPair,VPairs),vertex_pair_to_label_string(Nonblank,VPair,L)),LPairs),
					findall(X,(member(X,LPairs),write(Fd,X)),_),
					edges(U,Es),
					findall(DotEdge, 
						(member(Edge,Es),edge_to_dot(Edge,VPairs,DotEdge),write(Fd,DotEdge)),
						_),
					close_dot_file(Fd),
					open('dotbatch', append, Batch),
					atomic_list_concat(['/usr/bin/dot -Tjpg ',F,' -O\n'],Cmd),
					write(Batch,Cmd),
					close(Batch),
					shell('chmod +x dotbatch'),
					!.

vertex_pair_to_label_string([V,UUID],Label) :- \+is_list(V), 
								atomic_list_concat([UUID,' [label=\"',V,'\"]\n'],Label).
vertex_pair_to_label_string([V,UUID],Label) :- is_list(V), list_to_atom(V,Vatom),
								atomic_list_concat([UUID,' [label=\"',Vatom,'\"]\n'],Label).

vertex_pair_to_label_string(Nonblank,[V,UUID],Label) :- \+is_list(V), \+V=Nonblank, 
								atomic_list_concat([UUID,' [label=\"',' ','\"]\n'],Label).
vertex_pair_to_label_string(V,[V,UUID],Label) :- \+is_list(V), 
								atomic_list_concat([UUID,' [label=\"',V,'\"]\n'],Label).

% Should go into extension of set manipulation library:
union_collection(Collection,Union) :- union_collection(Collection,[],Union).
union_collection([],Union1,Union):- list_to_set(Union1,Union).
union_collection([H|T],ThusFar,Union) :- union(H,ThusFar,Next),
										union_collection(T,Next,Union).
disjoint(A,B) :- intersection(A,B,[]).
disjoint_from_collection(A,Collection) :- subtract(Collection,[A],C),
			forall(member(X,C),disjoint(X,A)).
disjoint_collection(C) :- forall(member(X,C),disjoint_from_collection(X,C)).

%set_equal(A,B) :- forall(member(X,A),member(X,B)), forall(member(Y,B),member(Y,A)).
partition(C,A) :- disjoint_collection(C), union_collection(C,B), set_equal(A,B).

binop_range_to_ugraph_component(BinOp,Z,U) :- range(BinOp,Rng), member(Z,Rng),
									findall(Pairs,(member([Pairs,Z],BinOp)),Vs),
									complete_graph(Vs,U).
findall_components(BinOp,ComponentList) :- range(BinOp,Rng),
						findall(Es,(member(X,Rng),binop_range_to_ugraph_component(BinOp,X,UC),edges(UC,Es)),ComponentList).
binop_range_to_ugraph(BinOp,U) :- findall_components(BinOp,L),
							union_collection(L,AllEs), vertices_edges_to_ugraph([],AllEs,U).

binop_to_dot(BinOp) :- binop_range_to_ugraph(BinOp,U), ugraph_to_dot(U).

								