:- module(multiset_operations, [
    full_bit_vector/2,           % Expands a partial mset to a full bit vector
    mset_normalize/2,            % Normalizes a partial mset to a full set
    mset_intersect/3,            % Computes the intersection of two multisets
    mset_union/3,                % Computes the union of two multisets
    mset_intersect_alt/3,        % Alternative intersection of two multisets
    mset_union_alt/3,            % Alternative union of two multisets
    mset_complement_with_max/3,  % Complement relative to a maximum multiplicity
    melcomplement_list/2,        % Complement each element of a multiset
    mset_relative_complement/3,  % Relative complement of two multisets
    mset_symmetric_difference/3, % Symmetric difference of two multisets
    mset_relative_complement_alt/3, % Alternative relative complement
    mset_symmetric_difference_alt/3, % Alternative symmetric difference
    mset_relative_complement_with_max/4, % Relative complement with max bound
    mset_symmetric_difference_with_max/4, % Symmetric difference with max bound
    mset_subset/2,               % Subset relation using standard intersection
    mset_subset_alt/2,           % Subset relation using alternative intersection
    all_msubsets/2               % Generate all multiset subsets
]).

:- use_module(universe, [u/1]).
:- use_module(element_operations, 
    [melintersect/3, melunion/3, 
    melintersect_alt/3, melunion_alt/3,
    melcomplement/2, 
    melcomplement_with_max/3,
    melrelative_complement/3,
    melrelative_complement_alt/3,
    melsymmetric_difference/3, 
    melsymmetric_difference_alt/3]).

% Relative complement with max
mset_relative_complement_with_max(A, B, R, M) :-
    mset_complement_with_max(B, ComplementB, M),
    mset_intersect_alt(A, ComplementB, R).

% Symmetric difference with max
mset_symmetric_difference_with_max(A, B, R, M) :-
    mset_relative_complement_with_max(A, B, ComplementAB, M),
    mset_relative_complement_with_max(B, A, ComplementBA, M),
    mset_union_alt(ComplementAB, ComplementBA, R).

mset_complement_with_max([], [], _).
mset_complement_with_max([[N, X] | Rest], [[R, X] | RestComplement], M) :-
    melcomplement_with_max([N, X], [R, X], M),
    mset_complement_with_max(Rest, RestComplement, M).

% full_bit_vector/2: Normalize a partial mset to a full bit vector
full_bit_vector(Partial, Full) :-
    findall([0, X], u(X), DefaultVector), % Create the default vector with 0s for all universe elements
    update_vector(Partial, DefaultVector, Full), !. % Ensure determinism

% update_vector/3: Update the default vector with multiplicities from the partial list
update_vector([], Full, Full) :- !. % Cut ensures no backtracking
update_vector([[M, X] | Rest], Default, Updated) :-
    select([0, X], Default, [M, X], Intermediate), % Replace [0, X] with [M, X]
    update_vector(Rest, Intermediate, Updated).

% mset_normalize/2: Normalize a partial mset
mset_normalize(Partial, Full) :-
    full_bit_vector(Partial, Full).

% mset_intersect/3: Compute the intersection of two multisets
mset_intersect(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1), % Normalize the first multiset
    mset_normalize(MSet2, Norm2), % Normalize the second multiset
    maplist(melintersect, Norm1, Norm2, Result), !. % Enforce determinism with a cut

% mset_union/3: Compute the union of two multisets
mset_union(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1), % Normalize the first multiset
    mset_normalize(MSet2, Norm2), % Normalize the second multiset
    maplist(melunion, Norm1, Norm2, Result), !. % Enforce determinism with a cut

% mset_intersect_alt/3: Alternative multiset intersection
mset_intersect_alt(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1), % Normalize the first mset
    mset_normalize(MSet2, Norm2), % Normalize the second mset
    maplist(melintersect_alt, Norm1, Norm2, Result). % Apply melintersect_alt element-wise

% mset_union_alt/3: Alternative multiset union
mset_union_alt(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1), % Normalize the first mset
    mset_normalize(MSet2, Norm2), % Normalize the second mset
    maplist(melunion_alt, Norm1, Norm2, Result). % Apply melunion_alt element-wise


% melcomplement_list/2: Apply complement to each element
melcomplement_list(MSet, Complement) :-
    maplist(melcomplement, MSet, Complement).


% Relative Complement: A \ B
mset_relative_complement(MSet1, MSet2, Result) :-
    mset_normalize(MSet2, Norm2),  % Normalize B
    melcomplement_list(Norm2, Complement),  % Complement of B
    mset_intersect(MSet1, Complement, Result), !.  % A ∩ complement(B), enforce determinism

% Symmetric Difference: A Δ B
mset_symmetric_difference(MSet1, MSet2, Result) :-
    mset_relative_complement(MSet1, MSet2, R1),  % A \ B
    mset_relative_complement(MSet2, MSet1, R2),  % B \ A
    mset_union(R1, R2, Result), !.  % (A \ B) ∪ (B \ A), enforce determinism

% mset_relative_complement_alt/3: Alternative relative complement
mset_relative_complement_alt(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1), % Normalize the first mset
    mset_normalize(MSet2, Norm2), % Normalize the second mset
    maplist(melrelative_complement_alt, Norm1, Norm2, Result). % Apply element-wise

% mset_symmetric_difference_alt/3: Alternative symmetric difference
mset_symmetric_difference_alt(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1), % Normalize the first mset
    mset_normalize(MSet2, Norm2), % Normalize the second mset
    maplist(melsymmetric_difference_alt, Norm1, Norm2, Result). % Apply element-wise

% mset_subset/2: True if A is a subset of B
mset_subset(A, B) :-
    mset_normalize(A, FullA),
    mset_normalize(B, FullB),
    mset_intersect(FullA, FullB, FullA).

% all_msubsets/2: Generates all msubsets of a given multiset.
all_msubsets(ParentSet, Subsets) :-
    findall(Subset, generate_subset(ParentSet, Subset), Subsets).

% generate_subset/2: Recursively generate subsets.
% Base case: The only subset of an empty set is the empty set.
generate_subset([], []).

% Recursive step: Generate subsets by picking multiplicities for the current element.
generate_subset([[M, X] | Rest], [[NewM, X] | Subset]) :-
    (M >= 0 -> between(0, M, NewM)           % For positive/zero multiplicities
    ; M < 0 -> between(M, 0, NewM)),         % For negative multiplicities
    generate_subset(Rest, Subset).

mset_subset_alt(A, B) :-
    mset_normalize(A, FullA),
    mset_normalize(B, FullB),
    mset_intersect_alt(FullA, FullB, Intersection),
    Intersection == FullA.

% Alternative subset generation utilities removed.  They duplicated
% all_msubsets/2 and produced incorrect results.
