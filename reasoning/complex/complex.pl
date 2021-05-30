% ----------------------------------------
% Mathematical library for complex numbers
% ----------------------------------------

'<http://josd.github.io/eye/math#complex_exponentiation>'([[A, B], [C, D]], [E, F]) :-
    polaire([A, B], [R, T]),
    E is R^C*exp(-D*T)*cos(D*log(R)+C*T),
    F is R^C*exp(-D*T)*sin(D*log(R)+C*T).

polaire([X, Y], [R, Tp]) :-
    R is sqrt(X**2+Y**2),
    T is acos(abs(X)/R),
    cadran(X, Y, T, Tp).

cadran(X, Y, T, Tp) :-
    X >= 0,
    Y >= 0,
    Tp = T.
cadran(X, Y, T, Tp) :-
    X < 0,
    Y >= 0,
    Tp is pi-T.
cadran(X, Y, T, Tp) :-
    X < 0,
    Y < 0,
    Tp is T+pi.
cadran(X, Y, T, Tp) :-
    X >= 0,
    Y < 0,
    Tp is 2*pi-T.
