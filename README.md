# prolog
This project contains swi-prolog predicates to process functions, relations and binary operations.  The binary operations can be converted to ugraphs by identifying pairs of operands that yield the same values under the given binary operation.  The resulting ugraphs can be converted to graphviz files for easy visualization.

## Contributor Guide

See the Repository Guidelines in AGENTS.md for project structure, coding style, testing, and PR expectations: [AGENTS.md](AGENTS.md).

## Quickstart

- Load the facade module for multisets:
  - In SWI-Prolog: `?- [multisets].`
  - Example queries:
    - `u('a+').` or `opposite('a+','a-').`
    - `mset_union([[2,'a+']], [[1,'a+']], R).`
    - `power_msubsets([[2,'a+'], [1,'b-']], Subs).`

- Load underlying lib modules directly (optional):
  - `?- [lib/universe, lib/element_operations, lib/multiset_operations].`

- Run tests:
  - All tests: `./test_all.sh`
  - Multiset suite only: `./test_multisets.sh`
