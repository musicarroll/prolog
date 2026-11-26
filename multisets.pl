% Multisets facade module
%
% This file provides a single entry point that re-exports the
% consolidated multiset implementation from the lib/ modules.
% Use this module if you want a convenient import that brings the
% universe, element operations, and multiset operations into scope.

:- module(multisets, []).

% Re-export the library modules to expose their public APIs via this module.
:- reexport('lib/universe').
:- reexport('lib/element_operations').
:- reexport('lib/multiset_operations').
