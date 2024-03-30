:-use_module(library(plunit)).
:-use_module('json_grammar.pl').

:- begin_tests(json_parser).


test(simple_object) :-
    string_codes("{\"name\":\"John\"}", Codes),
    phrase(json_object(JSON), Codes),
    assertion(JSON == [name-'John']),
    !.

test(nested_object) :-
    string_codes("{\"person\":{\"name\":\"John\", \"age\":\"30\"}}", Codes),
    phrase(json_object(JSON), Codes),
    assertion(JSON == [person-[name-'John', age-'30']]),
    !.

test(multiple_key_value_pairs) :-
    string_codes("{\"name\":\"John\", \"age\":\"30\", \"city\":\"New York\"}", Codes),
    phrase(json_object(JSON), Codes),
    assertion(JSON == [name-'John', age-'30', city-'New York']),
    !.

test(empty_object) :-
    string_codes("{}", Codes),
    phrase(json_object(JSON), Codes),
    assertion(JSON == []).

test(spaces_around_colon_and_commas) :-
    string_codes("{\"name\" : \"John\", \"age\" : \"30\", \"city\" : \"New York\"}", Codes),
    phrase(json_object(JSON), Codes),
    assertion(JSON == [name-'John', age-'30', city-'New York']).

:- end_tests(json_parser).
