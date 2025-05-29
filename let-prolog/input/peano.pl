% Peano arithmetic
% See https://en.wikipedia.org/wiki/Peano_axioms

:- op(1200, xfx, :+).

% add
add(A, 0, A).
add(A, s(B), s(C)) :-
    add(A, B, C).

% multiply
multiply(_, 0, 0).
multiply(A, s(B), C) :-
    multiply(A, B, D),
    add(A, D, C).

% factorial
factorial(A, B) :-
    fac(A, s(0), B).

fac(0, A, A).
fac(s(A), B, C) :-
    multiply(B, s(A), D),
    fac(A, D, C).

% query
true :+
    multiply(s(0), s(s(0)), A),
    add(A, s(s(s(0))), B),
    factorial(B, _).
