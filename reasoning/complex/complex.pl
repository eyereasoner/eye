% ----------------------------------------
% Mathematical library for complex numbers
% ----------------------------------------

% Inspired by http://alain.colmerauer.free.fr/alcol/ArchivesPublications/Equation4/Equation4.pdf

'<http://josd.github.io/eye/math#complex_exponentiation>'([[A, B], [C, D]], [E, F]) :-
    polaire([A, B], [R, T]),
    E is R**C*exp(-D*T)*cos(D*log(R)+C*T),
    F is R**C*exp(-D*T)*sin(D*log(R)+C*T).

'<http://josd.github.io/eye/math#complex_asin>'([A, B], [C, D]) :-
    E is (sqrt((1+A)^2+B^2)-sqrt((1-A)^2+B^2))/2,
    F is (sqrt((1+A)^2+B^2)+sqrt((1-A)^2+B^2))/2,
    C is asin(E),
    D is log(F+sqrt(F^2-1)).

'<http://josd.github.io/eye/math#complex_acos>'([A, B], [C, D]) :-
    E is (sqrt((1+A)^2+B^2)-sqrt((1-A)^2+B^2))/2,
    F is (sqrt((1+A)^2+B^2)+sqrt((1-A)^2+B^2))/2,
    C is acos(E),
    D is -log(F+sqrt(F^2-1)).

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
