% Peano arithmetic
% See https://en.wikipedia.org/wiki/Peano_axioms

:- op(1200, xfx, :+).

% add
'urn:example:add'(A, 0, A).
'urn:example:add'(A, s(B), s(C)) :-
    'urn:example:add'(A, B, C).

% multiply
'urn:example:multiply'(_, 0, 0).
'urn:example:multiply'(A, s(B), C) :-
    'urn:example:multiply'(A, B, D),
    'urn:example:add'(A, D, C).

% factorial
'urn:example:factorial'(A, B) :-
    fac(A, s(0), B).

fac(0, A, A).
fac(s(A), B, C) :-
    'urn:example:multiply'(B, s(A), D),
    fac(A, D, C).

% query
true :+
    'urn:example:multiply'(s(0), s(s(0)), A),
    'urn:example:add'(A, s(s(s(0))), B),
    'urn:example:factorial'(B, _).
