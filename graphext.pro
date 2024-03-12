% Extensions to the ugraph.pl library of graph functions
:-module('graphext',[ commute_edge/2,
			commute_all_edges/3,
			symmetric_closure/2,
			reflexive_edge/2,
			make_all_verts_reflexive/2,
			reflexive_closure/2,
			pair_to_edge/2,
			pairs_to_edges/2,
			union_of_ugraphs/3
			]
		).

commute_edge(X-Y,Y-X).
commute_all_edges(Edges,NewEdges,UnionOfEdges) :- maplist(commute_edge,Edges,NewEdges), 
										union(Edges,NewEdges,UnionOfEdges).
symmetric_closure(G,H) :- edges(G,Es), commute_all_edges(Es,_,UnionOfEdges),
							vertices(G,Vs), vertices_edges_to_ugraph(Vs,UnionOfEdges,H).
reflexive_edge(X,X-X).
make_all_verts_reflexive(Vs,NewEdges) :- maplist(reflexive_edge,Vs,NewEdges).

reflexive_closure(G,H) :- vertices(G,Vs), make_all_verts_reflexive(Vs,RefEdges),
							edges(G,Es), union(Es,RefEdges,NewEdges),
							vertices_edges_to_ugraph([],NewEdges,H).

pair_to_edge([X,Y],X-Y).
pairs_to_edges(Pairs,Edges) :- maplist(pair_to_edge,Pairs,Edges).

union_of_ugraphs(Ugraph,[],Ugraph).
union_of_ugraphs(Ugraph,[H|T],Ugraphs):-union(Ugraph,H,Tmp),
										union_of_ugraphs(Tmp,T,Ugraphs).
