% Mathematical library for derivatives
% See https://en.wikipedia.org/wiki/Derivative

:- op(1200, xfx, :+).

'urn:example:derivative'(A, B, C) :-
    derivative(A, B, C).

derivative(U, X, Y) :-
    d(U, X, V),
    s(V, W),
    s(W, Y).

integral(U, X, Y) :-
    d(V, X, U),
    s(V, W),
    s(W, Y).

% derivative
d(U+V, X, DU+DV) :-
    d(U, X, DU),
    d(V, X, DV),
    !.
d(U-V, X, DU-DV) :-
    d(U, X, DU),
    d(V, X, DV),
    !.
d(U*V, X, DU*V+U*DV) :-
    d(U, X, DU),
    d(V, X, DV),
    !.
d(U/V, X, (DU*V-U*DV)/V^2) :-
    !,
    d(U, X, DU),
    d(V, X, DV),
    !.
d(U^N, X, DU*N*U^N1) :-
    N1 is N - 1,
    d(U, X, DU),
    !.
d(-U, X, -DU) :-
    d(U, X, DU),
    !.
d(exp(U), X, DU*exp(U)) :-
    d(U, X, DU),
    !.
d(log(U), X, DU/U) :-
    d(U, X, DU),
    !.
d(erf(U), X, DU*sqrt(pi)/2*exp(-U^2)) :-
    d(U, X, DU),
    !.
d(X, X, 1) :-
    !.
d(_, _, 0).

% simplification
s(A, A) :-
    atom(A),
    !.
s(A, A) :-
    number(A),
    !.
s(0+A, A) :-
    !.
s(A+0, A) :-
    !.
s(0-A, -A) :-
    !.
s(A-0, A) :-
    !.
s(0*_, 0) :-
    !.
s(_*0, 0) :-
    !.
s(1*A, A) :-
    !.
s(A*1, A) :-
    !.
s(0/A, 0) :-
    A =\= 0,
    !.
s(_/0, 0) :-
    throw(divide_by_zero).
s(1/A, 1/A) :-
    !.
s(A/1, A) :-
    !.
s(0^_, 0) :-
    !.
s(_^0, 1) :-
    !.
s(1^_, 1) :-
    !.
s(A^1, A) :-
    !.
s(A, B) :-
    A =.. [_, C, D],
    number(C),
    number(D),
    !,
    B is A.
s(A, B) :-
    A =.. [C, D, E],
    !,
    s(D, F),
    s(E, G),
    B =.. [C, F, G].
s(A, A).

% query
true :+ 'urn:example:derivative'((x+1)*((x^2+2)*(x^3+3)), x, _).
true :+ 'urn:example:derivative'(x/x/x/x/x/x/x/x/x/x, x, _).
true :+ 'urn:example:derivative'(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, _).
true :+ 'urn:example:derivative'(x*x*x*x*x*x*x*x*x*x, x, _).
