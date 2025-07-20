% See https://en.wikipedia.org/wiki/Complex_number

:- op(1200, xfx, :+).

sum([[A, B], [C, D]], [E, F]) :-
    E is A+C,
    F is B+D.

difference([[A, B], [C, D]], [E, F]) :-
    E is A-C,
    F is B-D.

product([[A, B], [C, D]], [E, F]) :-
    E is A*C-B*D,
    F is A*D+B*C.

quotient([[A, B], [C, D]], [E, F]) :-
    E is (A*C+B*D)/(C^2+D^2),
    F is (B*C-A*D)/(C^2+D^2).

exponentiation([[A, B], [C, D]], [E, F]) :-
    polar([A, B], [G, H]),
    E is G^C*exp(-D*H)*cos(D*log(G)+C*H),
    F is G^C*exp(-D*H)*sin(D*log(G)+C*H).

log([[A, B], [C, D]], [E, F]) :-
    polar([A, B], [G, H]),
    polar([C, D], [I, J]),
    K is log(G),
    L is log(I),
    quotient([[L, J], [K, H]], [E, F]).

sin([A, B], [C, D]) :-
    C is sin(A)*(exp(B)+exp(-B))/2,
    D is cos(A)*(exp(B)-exp(-B))/2.

cos([A, B], [C, D]) :-
    C is cos(A)*(exp(B)+exp(-B))/2,
    D is -sin(A)*(exp(B)-exp(-B))/2.

tan(A, B) :-
    sin(A, C),
    cos(A, D),
    quotient([C, D], B).

asin([A, B], [C, D]) :-
    E is (sqrt((1+A)^2+B^2)-sqrt((1-A)^2+B^2))/2,
    F is (sqrt((1+A)^2+B^2)+sqrt((1-A)^2+B^2))/2,
    C is asin(E),
    D is log(F+sqrt(F^2-1)).

acos([A, B], [C, D]) :-
    E is (sqrt((1+A)^2+B^2)-sqrt((1-A)^2+B^2))/2,
    F is (sqrt((1+A)^2+B^2)+sqrt((1-A)^2+B^2))/2,
    C is acos(E),
    D is -log(F+sqrt(F^2-1)).

atan(A, B) :-
    difference([[0, 1], A], C),
    sum([[0, 1], A], D),
    quotient([C, D], E),
    log([[2.718281828459045, 0], E], F),
    quotient([F, [0, 2]], B).

polar([A, B], [C, D]) :-
    C is sqrt(A^2+B^2),
    E is acos(abs(A)/C),
    angular(A, B, E, D).

angular(A, B, C, D) :-
    A >= 0,
    B >= 0,
    D = C.
angular(A, B, C, D) :-
    A < 0,
    B >= 0,
    D is pi-C.
angular(A, B, C, D) :-
    A < 0,
    B < 0,
    D is C+pi.
angular(A, B, C, D) :-
    A >= 0,
    B < 0,
    D is 2*pi-C.

% query
true :+ quotient([[1, 0], [0, 1]], _).
true :+ exponentiation([[-1, 0], [0.5, 0]], _).
true :+ exponentiation([[2.718281828459045, 0], [0, pi]], _).
true :+ log([[2.718281828459045, 0], [-1, 0]], _).
true :+ log([[0, 1], [0, 1]], _).
true :+ sin([1.570796326794897, 1.316957896924817], _).
true :+ cos([0, -1.316957896924817], _).
true :+ tan([1.338972522294493, 0.4023594781085251], _).
true :+ asin([2, 0], _).
true :+ acos([2, 0], _).
true :+ atan([1, 2], _).
