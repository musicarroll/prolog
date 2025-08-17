:-use_module(library(plunit)).
:-use_module('json_grammar.pl').

:- begin_tests(json_parser).


% test/1 - defines the test predicate.
test(simple_object) :-
    string_codes("{\"name\":\"John\"}", Codes),
    phrase(json_object(JSON), Codes),
    assertion(JSON == [name-'John']),
    !.

% test/1 - defines the test predicate.
test(nested_object) :-
    string_codes("{\"person\":{\"name\":\"John\", \"age\":\"30\"}}", Codes),
    phrase(json_object(JSON), Codes),
    assertion(JSON == [person-[name-'John', age-'30']]),
    !.

% test/1 - defines the test predicate.
test(multiple_key_value_pairs) :-
    string_codes("{\"name\":\"John\", \"age\":\"30\", \"city\":\"New York\"}", Codes),
    phrase(json_object(JSON), Codes),
    assertion(JSON == [name-'John', age-'30', city-'New York']),
    !.

% test/1 - defines the test predicate.
test(empty_object) :-
    string_codes("{}", Codes),
    phrase(json_object(JSON), Codes),
% assertion/1 - defines the assertion predicate.
    assertion(JSON == []).

% test/1 - defines the test predicate.
test(spaces_around_colon_and_commas) :-
    string_codes("{\"name\" : \"John\", \"age\" : \"30\", \"city\" : \"New York\"}", Codes),
    phrase(json_object(JSON), Codes),
% assertion/3 - defines the assertion predicate.
    assertion(JSON == [name-'John', age-'30', city-'New York']).

:- end_tests(json_parser).
