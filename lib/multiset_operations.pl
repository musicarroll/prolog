:- module(multiset_operations, [
    full_bit_vector/2,
    mset_normalize/2,
    mset_intersect/3,
    mset_union/3,
    mset_intersect_alt/3,
    mset_union_alt/3,
    mset_complement_with_max/3,
    melcomplement_list/2,
    mset_relative_complement/3,
    mset_symmetric_difference/3,
    mset_relative_complement_alt/3,
    mset_symmetric_difference_alt/3,
    mset_relative_complement_with_max/4,
    mset_symmetric_difference_with_max/4,
    mset_subset/2,
    mset_subset_alt/2,
    msubset/2,
    all_msubsets/2,
    all_msubsets_with_max/3,
    all_msubsets_on_support/2,
    power_msubsets/2
]).

:- use_module(universe, [u/1]).
:- use_module(element_operations, [
    melintersect/3, melunion/3,
    melintersect_alt/3, melunion_alt/3,
    melcomplement/2,
    melcomplement_with_max/3,
    melrelative_complement/3,
    melrelative_complement_alt/3,
    melsymmetric_difference/3,
    melsymmetric_difference_alt/3
]).

mset_relative_complement_with_max(A, B, R, M) :-
    mset_complement_with_max(B, ComplementB, M),
    mset_intersect_alt(A, ComplementB, R).

mset_symmetric_difference_with_max(A, B, R, M) :-
    mset_relative_complement_with_max(A, B, ComplementAB, M),
    mset_relative_complement_with_max(B, A, ComplementBA, M),
    mset_union_alt(ComplementAB, ComplementBA, R).

mset_complement_with_max([], [], _).
mset_complement_with_max([[N, X] | Rest], [[R, Y] | RestComplement], M) :-
    melcomplement_with_max([N, X], [R, Y], M),
    mset_complement_with_max(Rest, RestComplement, M).

full_bit_vector(Partial, Full) :-
    findall([0, X], u(X), DefaultVector),
    update_vector(Partial, DefaultVector, Full), !.

update_vector([], Full, Full) :- !.
update_vector([[M, X] | Rest], Default, Updated) :-
    select([0, X], Default, [M, X], Intermediate),
    update_vector(Rest, Intermediate, Updated).

mset_normalize(Partial, Full) :-
    full_bit_vector(Partial, Full).

mset_intersect(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1),
    mset_normalize(MSet2, Norm2),
    maplist(melintersect, Norm1, Norm2, Result), !.

mset_union(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1),
    mset_normalize(MSet2, Norm2),
    maplist(melunion, Norm1, Norm2, Result), !.

mset_intersect_alt(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1),
    mset_normalize(MSet2, Norm2),
    maplist(melintersect_alt, Norm1, Norm2, Result).

mset_union_alt(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1),
    mset_normalize(MSet2, Norm2),
    maplist(melunion_alt, Norm1, Norm2, Result).

melcomplement_list(MSet, Complement) :-
    maplist(melcomplement, MSet, Complement).

mset_relative_complement(MSet1, MSet2, Result) :-
    mset_normalize(MSet2, Norm2),
    melcomplement_list(Norm2, Complement),
    mset_intersect(MSet1, Complement, Result), !.

mset_symmetric_difference(MSet1, MSet2, Result) :-
    mset_relative_complement(MSet1, MSet2, R1),
    mset_relative_complement(MSet2, MSet1, R2),
    mset_union(R1, R2, Result), !.

mset_relative_complement_alt(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1),
    mset_normalize(MSet2, Norm2),
    maplist(melrelative_complement_alt, Norm1, Norm2, Result).

mset_symmetric_difference_alt(MSet1, MSet2, Result) :-
    mset_normalize(MSet1, Norm1),
    mset_normalize(MSet2, Norm2),
    maplist(melsymmetric_difference_alt, Norm1, Norm2, Result).

mset_subset(A, B) :-
    mset_normalize(A, FullA),
    mset_normalize(B, FullB),
    mset_intersect(FullA, FullB, FullA).

% Alias using the terminology from the paper
msubset(A, B) :- mset_subset(A, B).

all_msubsets(ParentSet, Subsets) :-
    findall(Subset, generate_subset(ParentSet, Subset), Subsets).

generate_subset([], []).
generate_subset([[M, X] | Rest], [[NewM, X] | Subset]) :-
    (M >= 0 -> between(0, M, NewM)
    ; M < 0 -> between(M, 0, NewM)),
    generate_subset(Rest, Subset).

mset_subset_alt(A, B) :-
    mset_normalize(A, FullA),
    mset_normalize(B, FullB),
    mset_intersect_alt(FullA, FullB, Intersection),
    Intersection == FullA.

% --- Signed msubset enumeration with explicit maximum bound ---

% all_msubsets_with_max(+Parent, +Max, -Subsets)
% Generates all A such that A ⊑ Parent under the signed order and
% 0 <= multiplicity <= Max for every element. For x ∈ U+ choose 0..P(x),
% for x ∈ U- choose P(x)..Max.
all_msubsets_with_max(Parent, M, Subsets) :-
    integer(M), M >= 0,
    mset_normalize(Parent, NormP),
    findall(Subset, generate_subset_signed(NormP, M, Subset), Subsets).

generate_subset_signed([], _, []).
generate_subset_signed([[Mp, X] | Rest], M, [[NewM, X] | Subset]) :-
    ( is_positive_atom(X) -> Min is 0, Max is min(Mp, M), between(Min, Max, NewM)
    ; is_negative_atom(X) -> between(Mp, M, NewM)
    ),
    generate_subset_signed(Rest, M, Subset).

is_positive_atom(X) :- atom(X), atom_concat(_, '+', X).
is_negative_atom(X) :- atom(X), atom_concat(_, '-', X).

% --- Finite enumeration on support ---
% all_msubsets_on_support(+Parent, -Subsets)
% Enumerates A such that A ⪯ Parent, fixing negatives to Parent on its support
% and forcing zeros off-support. This yields a finite family useful as a
% “power mset” analogue under the signed model.
all_msubsets_on_support(Parent, Subsets) :-
    mset_normalize(Parent, NormP),
    findall(Subset, gen_on_support(NormP, Subset), Subsets).

gen_on_support([], []).
gen_on_support([[Mp, X] | Rest], [[NewM, X] | Subset]) :-
    ( is_positive_atom(X) -> ( Mp > 0 -> between(0, Mp, NewM) ; NewM = 0 )
    ; is_negative_atom(X) -> ( Mp > 0 -> NewM = Mp ; NewM = 0 )
    ),
    gen_on_support(Rest, Subset).

% Canonical API for the on-support finite family (Power Multiset)
power_msubsets(Parent, Subsets) :- all_msubsets_on_support(Parent, Subsets).
