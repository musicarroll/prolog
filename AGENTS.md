# Repository Guidelines

## Project Structure & Modules
- Root: core SWI‑Prolog modules in `.pro` (e.g., `functions.pro`, `binops.pro`, `listpreds.pro`), examples/utilities in `.pl` (e.g., `family.pl`, `json_grammar.pl`).
- `lib/`: multiset components (`universe.pl`, `element_operations.pl`, `multiset_operations.pl`).
- `tests/`: plunit suites (e.g., `tests/multiset_tests.pl`) and root test files `test_*.pl`.
- Scripts: `test_all.sh` (run full suite), `test_multisets.sh` (multiset tests).
- Docs/assets: `book/`, `dot/`, `svg/`, LaTeX sources like `msets.tex`.
- Integration: `query_prolog.py` demo via PySWIP.

## Build, Test, and Development
- Prereqs: SWI‑Prolog (swipl). Optional: Python 3 + `pyswip`.
- Run all tests: `./test_all.sh`
- Run one suite: `swipl -q -s test_functions.pl -g run_tests,halt -t halt`
- Multisets only: `./test_multisets.sh`
- REPL quickstart: `swipl -q -l functions.pro` then query, e.g., `functions:powerset([a,b], P).`
- Python demo: `python3 query_prolog.py` (ensures `pyswip` installed).

## Coding Style & Naming
- Indentation: 4 spaces; no tabs. One clause per line.
- Modules: declare exports at top, e.g., `:- module(functions, [powerset/2, cartesian_product/3, ...]).`
- Names: predicates `lower_snake_case`; variables capitalized; atoms lowercase.
- Files: core modules in `.pro`; tests in `tests/` or `test_*.pl`.
- Comments: brief `%` header above predicates (pattern used across repo).

## Testing Guidelines
- Framework: `plunit` with `:- begin_tests(name). … :- end_tests(name).`
- Location: add new suites under `tests/` or as `test_<area>.pl` in root mirroring the module under test.
- Conventions: use `assertion/1` or `test(Name, [true(Goal)])` for clarity; keep tests deterministic.
- Run: see commands above; prefer `test_all.sh` before submitting.

## Commit & Pull Requests
- Commits: short, imperative summaries (e.g., “Organize multiset code and prune unused features”); group related changes; reference issues/PRs when applicable.
- PRs: include what/why, linked issues, test plan (commands + expected output), and screenshots/logs when helpful (e.g., timing from `test_order_set_and_cartesian.pl`).

## Security & Configuration
- Avoid adding networked side‑effects. Keep Prolog code pure/deterministic where possible.
- Note dependencies when touching integration (`pyswip`, Graphviz if generating visualizations).
