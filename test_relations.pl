:-use_module(library(plunit)).
:-use_module('binops.pro').


:- begin_tests(binary_operations).

% test/1 - defines the test predicate.
test(index_to_word) :-
    binops:index_to_word(3, 4, ['a', 'b'], Word),
% assertion/4 - defines the assertion predicate.
    assertion(Word == [a, a, b, b]). % Corrected expectation

% test/1 - defines the test predicate.
test(gen_nth_binop_lex) :-
    binops:gen_nth_binop_lex(['a', 'b'], 3, 4, BinOp),
% assertion/4 - defines the assertion predicate.
    assertion(BinOp == [a, a, b, a]).

% test/1 - defines the test predicate.
test(word_to_index_first) :-
        binops:word_to_index(['a', 'a', 'a', 'a'], 4, ['a', 'b'], N),
        assertion(N == 1),
        !.
    
% test/1 - defines the test predicate.
test(word_to_index_second) :-
        binops:word_to_index(['a', 'a', 'a', 'b'], 4, ['a', 'b'], N),
        assertion(N == 2),
        !.
        
:- end_tests(binary_operations).
