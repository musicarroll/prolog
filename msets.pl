% Utility to find max of two numbers
% max_multiplicity/3 - defines the max multiplicity predicate.
max_multiplicity(X, Y, Max) :-
    Max is max(X, Y).

% Utility to find min of two numbers
% min_multiplicity/3 - defines the min multiplicity predicate.
min_multiplicity(X, Y, Min) :-
    Min is min(X, Y).

% Complement operation: negate multiplicities
% mset_complement/2 - defines the mset complement predicate.
mset_complement([], []).
% mset_complement/2 - defines the mset complement predicate.
mset_complement([Elem-M|MSet], [Elem-NM|CompMSet]) :-
    NM is -M,
% mset_complement/2 - defines the mset complement predicate.
    mset_complement(MSet, CompMSet).

% Union of two msets
% mset_union/3 - defines the mset union predicate.
mset_union([], [], []).
% mset_union/3 - defines the mset union predicate.
mset_union([X-MX|MSet1], [X-MY|MSet2], [X-MU|MSetUnion]) :-
    (
        (MX >= 0, MY >= 0 -> max_multiplicity(MX, MY, MU));  % Both positive
        (MX < 0, MY < 0 -> min_multiplicity(MX, MY, MU));    % Both negative
        (max_multiplicity(MX, MY, MU))  % Mixed polarity case, use max
    ),
% mset_union/3 - defines the mset union predicate.
    mset_union(MSet1, MSet2, MSetUnion).
% mset_union/3 - defines the mset union predicate.
mset_union([X-MX|MSet1], [], [X-MX|MSetUnion]) :-
% mset_union/3 - defines the mset union predicate.
    mset_union(MSet1, [], MSetUnion).
% mset_union/3 - defines the mset union predicate.
mset_union([], [X-MY|MSet2], [X-MY|MSetUnion]) :-
% mset_union/3 - defines the mset union predicate.
    mset_union([], MSet2, MSetUnion).

% Intersection of two msets
% mset_intersection/3 - defines the mset intersection predicate.
mset_intersection([], [], []).
% mset_intersection/3 - defines the mset intersection predicate.
mset_intersection([X-MX|MSet1], [X-MY|MSet2], [X-MI|MSetIntersection]) :-
    (
        (MX >= 0, MY >= 0 -> min_multiplicity(MX, MY, MI));  % Both positive
        (MX < 0, MY < 0 -> max_multiplicity(MX, MY, MI));    % Both negative
        (min_multiplicity(MX, MY, MI))  % Mixed polarity case, use min
    ),
% mset_intersection/3 - defines the mset intersection predicate.
    mset_intersection(MSet1, MSet2, MSetIntersection).
% mset_intersection/3 - defines the mset intersection predicate.
mset_intersection([X-MX|MSet1], [], [X-MX|MSetIntersection]) :-
% mset_intersection/3 - defines the mset intersection predicate.
    mset_intersection(MSet1, [], MSetIntersection).
% mset_intersection/3 - defines the mset intersection predicate.
mset_intersection([], [X-MY|MSet2], [X-MY|MSetIntersection]) :-
% mset_intersection/3 - defines the mset intersection predicate.
    mset_intersection([], MSet2, MSetIntersection).

% De Morgan Test
% test_de_morgan/3 - defines the test de morgan predicate.
test_de_morgan(MSetA, MSetB, MSetC) :-
    % De Morgan 1: complement of the union of two sets is the intersection of their complements
    mset_union(MSetA, MSetB, UnionAB),
    mset_complement(UnionAB, ComplementUnionAB),
    mset_complement(MSetA, ComplementA),
    mset_complement(MSetB, ComplementB),
    mset_intersection(ComplementA, ComplementB, IntersectionCompAB),
    writeln('De Morgan 1:'),
    writeln('Complement of Union A U B: '), writeln(ComplementUnionAB),
    writeln('Intersection of Complement A and Complement B: '), writeln(IntersectionCompAB),
    (ComplementUnionAB = IntersectionCompAB -> writeln('De Morgan 1 holds.'); writeln('De Morgan 1 fails.')),
    
    % De Morgan 2: complement of the intersection of two sets is the union of their complements
    mset_intersection(MSetA, MSetB, IntersectionAB),
    mset_complement(IntersectionAB, ComplementIntersectionAB),
    mset_union(ComplementA, ComplementB, UnionCompAB),
    writeln('De Morgan 2:'),
    writeln('Complement of Intersection A ∩ B: '), writeln(ComplementIntersectionAB),
    writeln('Union of Complement A and Complement B: '), writeln(UnionCompAB),
    (ComplementIntersectionAB = UnionCompAB -> writeln('De Morgan 2 holds.'); writeln('De Morgan 2 fails.')).

% Example test
% run_test/0 - defines the run test predicate.
run_test :-
    % Define three msets for testing
    MSetA = [a-1, b-2, c-(-1)],
    MSetB = [a-(-2), b-1, d-(-1)],
    MSetC = [a-2, c-1, d-1],
    % Run the De Morgan tests
% test_de_morgan/3 - defines the test de morgan predicate.
    test_de_morgan(MSetA, MSetB, MSetC).


% Generate the possible multiplicities for an element based on its current multiplicity
% possible_multiplicities/2 - defines the possible multiplicities predicate.
possible_multiplicities(Elem-Count, [Elem-Multiplicities]) :-
    (   Count > 0
    ->  findall(X, between(0, Count, X), Multiplicities)  % Positive multiplicity, range from 0 to Count
    ;   Count < 0
    ->  findall(X, between(Count, 0, X), Multiplicities)  % Negative multiplicity, range from Count to 0
    ;   Multiplicities = [0]  % If multiplicity is 0, only 0 is possible
    ).

% Helper predicate to compute the Cartesian product of possible multiplicities
% cartesian_product/2 - defines the cartesian product predicate.
cartesian_product([], [[]]).
% cartesian_product/2 - defines the cartesian product predicate.
cartesian_product([[Elem-Multiplicities]|Rest], Product) :-
    cartesian_product(Rest, SubProduct),
    findall([Elem-Mult|SubP], (member(Mult, Multiplicities), member(SubP, SubProduct)), Product).

% Generate all msubsets of a given mset
% msubsets/2 - defines the msubsets predicate.
msubsets(MSet, MSubsets) :-
    maplist(possible_multiplicities, MSet, Possibilities),
% cartesian_product/2 - defines the cartesian product predicate.
    cartesian_product(Possibilities, MSubsets).

% Example mset: {-a, 2b}
% mset_msubsets([a-(-1), b-2], Subsets).
