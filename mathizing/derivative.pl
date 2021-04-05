% See https://en.wikipedia.org/wiki/Derivative

d(U+V, X, DU+DV) :-
    !,
    d(U, X, DU),
    d(V, X, DV).
d(U-V, X, DU-DV) :-
    !,
    d(U, X, DU),
    d(V, X, DV).
d(U*V, X, DU*V+U*DV) :-
    !,
    d(U, X, DU),
    d(V, X, DV).
d(U/V, X, (DU*V-U*DV)/V^2) :-
    !,
    d(U, X, DU),
    d(V, X, DV).
d(U^N, X, DU*N*U^N1) :-
    !,
    N1 is N - 1,
    d(U, X, DU).
d(-U, X, -DU) :-
    !,
    d(U, X, DU).
d(exp(U), X, exp(U)*DU) :-
    !,
    d(U, X, DU).
d(log(U), X, DU/U) :-
    !,
    d(U, X, DU).
d(sqrt(pi)*erf(X)/2, X, exp(-X^2)) :-
    !.
d(X, X, 1) :-
    !.
d(_, _, 0).

% test cases
case(d((x+1)*((x^2+2)*(x^3+3)), x, _)).
case(d(((((((((x/x)/x)/x)/x)/x)/x)/x)/x)/x, x, _)).
case(d(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, _)).
case(d(((((((((x*x)*x)*x)*x)*x)*x)*x)*x)*x, x, _)).
case(d(_, x, 1*6*x^5)).
case(d(_, x, exp(-x^2))).

test :-
    case(A),
    A,
    writeln(A),
    fail.
test :-
    halt.
