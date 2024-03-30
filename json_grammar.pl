% json_grammar.pl
:-module('json_grammar.pl',
    [
        json_string//1,
        json_object//1
        ]).
% Entry point for parsing a JSON object.
json_object(JSON) --> 
    "{", ws, json_pairs(JSON), ws, "}".

% Parse key-value pairs within an object.
json_pairs([Key-Value|Pairs]) -->
    json_string(Key), ws, ":", ws, json_value(Value),
    ws, json_pairs_rest(Pairs).
json_pairs([]) --> [].

% Handle the rest of the key-value pairs, if any.
json_pairs_rest(Pairs) -->
    ",", ws, json_pairs(Pairs).
json_pairs_rest([]) --> [].

% Parse a JSON value (currently simplified to only accept strings or nested objects).
json_value(Value) --> 
    json_string(Value) ;
    json_object(Value).

% A simple way to parse strings enclosed in quotes.
% This rule accumulates characters until it encounters a closing quote.
% Correctly parse a quoted string as a JSON key or value, excluding the quotes.
json_string(String) -->
    "\"", string_chars(Chars), "\"", { atom_codes(String, Chars) }.

% Accumulate characters until the closing quote, excluding the quote itself.
string_chars([C|Cs]) --> [C], { C \= 34 }, string_chars(Cs). % 34 is the ASCII code for "
string_chars([]) --> [].

% Collect characters until the closing quote.
% This is a simplified model and does not handle escape sequences.
chars([Char|Chars]) -->
    [Char],
    { \+ member(Char, ['\"']) }, % Stop at the closing quote.
    chars(Chars).
chars([]) --> [].

% Skip whitespace characters.
ws --> [W], { char_type(W, space) }, ws.
ws --> [].
