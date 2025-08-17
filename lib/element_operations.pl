
:- module(element_operations, [
    melintersect/3,
    melunion/3,
    melintersect_alt/3,
    melunion_alt/3,
    melcomplement_with_max/3,
    melcomplement/2,
    melrelative_complement/3,
    melsymmetric_difference/3,
    melrelative_complement_alt/3,
    melsymmetric_difference_alt/3,
    melsymmetric_difference_with_max/4,
    melrelative_complement_with_max/4
]).

:- use_module(universe, [u/1]).

% melintersect/3: Intersection operation
melintersect([M, X], [N, X], [R, X]) :-
    ( M < 0, N < 0 -> R is max(M, N), !
    ; M < 0, N = 0 -> R is max(M, N), !
    ; M < 0, N > 0 -> R is max(M, N), !
    ; M = 0, N < 0 -> R is max(M, N), !
    ; M = 0, N = 0 -> R is 0, !
    ; M = 0, N > 0 -> R is min(M, N), !
    ; M > 0, N < 0 -> R is min(M, N), !
    ; M > 0, N = 0 -> R is min(M, N), !
    ; M > 0, N > 0 -> R is min(M, N)
    ).
% melintersect_alt/3: Alternative intersection operation
melintersect_alt([M, X], [N, X], [R, X]) :-
    ( M < 0, N < 0 -> R is max(M, N), !
    ; M < 0, N = 0 -> R is max(M, N), !
    ; M < 0, N > 0 -> R is M + N, ! % Use sum for mixed cases
    ; M = 0, N < 0 -> R is max(M, N), !
    ; M = 0, N = 0 -> R is 0, !
    ; M = 0, N > 0 -> R is min(M, N), !
    ; M > 0, N < 0 -> R is M + N, ! % Use sum for mixed cases
    ; M > 0, N = 0 -> R is min(M, N), !
    ; M > 0, N > 0 -> R is min(M, N)
    ).


% melunion/3: Union operation
melunion([M, X], [N, X], [R, X]) :-
    ( M < 0, N < 0 -> R is min(M, N), !
    ; M < 0, N = 0 -> R is min(M, N), !
    ; M < 0, N > 0 -> R is min(M, N), !
    ; M = 0, N < 0 -> R is min(M, N), !
    ; M = 0, N = 0 -> R is 0, !
    ; M = 0, N > 0 -> R is max(M, N), !
    ; M > 0, N < 0 -> R is max(M, N), !
    ; M > 0, N = 0 -> R is max(M, N), !
    ; M > 0, N > 0 -> R is max(M, N)
    ).

% melunion_alt/3: Alternative union operation
melunion_alt([M, X], [N, X], [R, X]) :-
    ( M < 0, N < 0 -> R is min(M, N), !
    ; M < 0, N = 0 -> R is min(M, N), !
    ; M < 0, N > 0 -> R is M + N, !
    ; M = 0, N < 0 -> R is min(M, N), !
    ; M = 0, N = 0 -> R is 0, !
    ; M = 0, N > 0 -> R is max(M, N), !
    ; M > 0, N < 0 -> R is M + N, !
    ; M > 0, N = 0 -> R is max(M, N), !
    ; M > 0, N > 0 -> R is max(M, N)
    ).

melcomplement_with_max([N, X], [R, X], M) :-
    (N > 0 -> R is N - M;
        N < 0 -> R is N + M;
        R is 0).
    

% melcomplement/2: Negate the multiplicity
melcomplement([M, X], [R, X]) :-
    u(X), % Ensure valid element
    R is -M, !.

% Element-level relative complement with max
melrelative_complement_with_max([M1, X], [M2, X], [R, X], M) :-
    melcomplement_with_max([M2, X], [C, X], M),
    melintersect_alt([M1, X], [C, X], [R, X]).

% Element-level symmetric difference with max
melsymmetric_difference_with_max([M1, X], [M2, X], [R, X], M) :-
    melrelative_complement_with_max([M1, X], [M2, X], [RC1, X], M),
    melrelative_complement_with_max([M2, X], [M1, X], [RC2, X], M),
    melunion_alt([RC1, X], [RC2, X], [R, X]).


% Relative Complement: A \ B = A ∩ complement(B)
melrelative_complement([M, X], [N, X], [R, X]) :-
    melcomplement([N, X], [C, X]),  % Compute complement of B
    melintersect([M, X], [C, X], [R, X]), !.  % Compute intersection with A and enforce determinism

% Symmetric Difference: A Δ B = (A \ B) ∪ (B \ A)
melsymmetric_difference([M, X], [N, X], [R, X]) :-
    melrelative_complement([M, X], [N, X], [R1, X]),  % A \ B
    melrelative_complement([N, X], [M, X], [R2, X]),  % B \ A
    melunion([R1, X], [R2, X], [R, X]), !.  % (A \ B) ∪ (B \ A), enforce determinism

% melrelative_complement_alt/3: Alternative relative complement
melrelative_complement_alt([M, X], [N, X], [R, X]) :-
    melcomplement([N, X], [CompN, X]),      % Complement N
    melintersect_alt([M, X], [CompN, X], [R, X]). % Intersection with complement

% melsymmetric_difference_alt/3: Alternative symmetric difference
melsymmetric_difference_alt([M, X], [N, X], [R, X]) :-
    melrelative_complement_alt([M, X], [N, X], [C1, X]), % A \ B
    melrelative_complement_alt([N, X], [M, X], [C2, X]), % B \ A
    melunion_alt([C1, X], [C2, X], [R, X]).             % Union of results
