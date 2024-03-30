% Original octonion multiplication table with all terms symbolic
:-module('octonions',[
    octonion_mult/1,
    complete_octonion_table/1
    ]
).
octonion_mult([
    [['1','1'],'1'], [['1','e1'],'e1'], [['1','e2'],'e2'], [['1','e3'],'e3'], [['1','e4'],'e4'], [['1','e5'],'e5'], [['1','e6'],'e6'], [['1','e7'],'e7'],
    [['e1','1'],'e1'], [['e1','e1'],'-1'], [['e1','e2'],'e3'], [['e1','e3'],'-e2'], [['e1','e4'],'e5'], [['e1','e5'],'-e4'], [['e1','e6'],'-e7'], [['e1','e7'],'e6'],
    [['e2','1'],'e2'], [['e2','e1'],'-e3'], [['e2','e2'],'-1'], [['e2','e3'],'e1'], [['e2','e4'],'e6'], [['e2','e5'],'e7'], [['e2','e6'],'-e4'], [['e2','e7'],'-e5'],
    [['e3','1'],'e3'], [['e3','e1'],'e2'], [['e3','e2'],'-e1'], [['e3','e3'],'-1'], [['e3','e4'],'e7'], [['e3','e5'],'-e6'], [['e3','e6'],'e5'], [['e3','e7'],'-e4'],
    [['e4','1'],'e4'], [['e4','e1'],'-e5'], [['e4','e2'],'-e6'], [['e4','e3'],'-e7'], [['e4','e4'],'-1'], [['e4','e5'],'e1'], [['e4','e6'],'e2'], [['e4','e7'],'e3'],
    [['e5','1'],'e5'], [['e5','e1'],'e4'], [['e5','e2'],'-e7'], [['e5','e3'],'e6'], [['e5','e4'],'-e1'], [['e5','e5'],'-1'], [['e5','e6'],'-e3'], [['e5','e7'],'e2'],
    [['e6','1'],'e6'], [['e6','e1'],'e7'], [['e6','e2'],'e4'], [['e6','e3'],'-e5'], [['e6','e4'],'-e2'], [['e6','e5'],'e3'], [['e6','e6'],'-1'], [['e6','e7'],'-e1'],
    [['e7','1'],'e7'], [['e7','e1'],'-e6'], [['e7','e2'],'e5'], [['e7','e3'],'e4'], [['e7','e4'],'-e3'], [['e7','e5'],'-e2'], [['e7','e6'],'e1'], [['e7','e7'],'-1']
]).

% Generate terms with negatives
negate_term('1', '-1').
negate_term('-1', '1').
negate_term(Term, NegTerm) :-
    atom(Term),
    atom_concat('-', Rest, Term),
    atom_concat('', Rest, NegTerm).
negate_term(Term, NegTerm) :-
    atom(Term),
    \+ atom_concat('-', _, Term),
    atom_concat('-', Term, NegTerm).

% Generate terms with negatives, avoiding duplicates
generate_negatives([], []).
generate_negatives([[[A,B],C]|Rest], [[[A,B],C], [[NegA,B],NegC], [[A,NegB],NegC], [[NegA,NegB],C]|NegRest]) :-
    negate_term(A, NegA),
    negate_term(B, NegB),
    negate_term(C, NegC),
    % Ensure that A and B are not both negated to avoid duplicates
    (A \= NegA; B \= NegB),
    generate_negatives(Rest, NegRest).


% Get the complete table including terms with negatives, ensuring uniqueness
complete_octonion_table(UniqueTable) :-
    octonion_mult(OriginalTable),
    generate_negatives(OriginalTable, Negatives),
    append(OriginalTable, Negatives, CompleteTable),
    list_to_set(CompleteTable, UniqueTable).

