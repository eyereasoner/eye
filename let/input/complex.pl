% See https://en.wikipedia.org/wiki/Complex_number

complex_sum([[A, B], [C, D]], [E, F]) :-
    E is A+C,
    F is B+D.

complex_difference([[A, B], [C, D]], [E, F]) :-
    E is A-C,
    F is B-D.

complex_product([[A, B], [C, D]], [E, F]) :-
    E is A*C-B*D,
    F is A*D+B*C.

complex_quotient([[A, B], [C, D]], [E, F]) :-
    E is (A*C+B*D)/(C^2+D^2),
    F is (B*C-A*D)/(C^2+D^2).

complex_exponentiation([[A, B], [C, D]], [E, F]) :-
    polar([A, B], [G, H]),
    E is G^C*exp(-D*H)*cos(D*log(G)+C*H),
    F is G^C*exp(-D*H)*sin(D*log(G)+C*H).

complex_log([[A, B], [C, D]], [E, F]) :-
    polar([A, B], [G, H]),
    polar([C, D], [I, J]),
    K is log(G),
    L is log(I),
    complex_quotient([[L, J], [K, H]], [E, F]).

complex_sin([A, B], [C, D]) :-
    C is sin(A)*(exp(B)+exp(-B))/2,
    D is cos(A)*(exp(B)-exp(-B))/2.

complex_cos([A, B], [C, D]) :-
    C is cos(A)*(exp(B)+exp(-B))/2,
    D is -sin(A)*(exp(B)-exp(-B))/2.

complex_tan(A, B) :-
    sin(A, C),
    cos(A, D),
    complex_quotient([C, D], B).

complex_asin([A, B], [C, D]) :-
    E is (sqrt((1+A)^2+B^2)-sqrt((1-A)^2+B^2))/2,
    F is (sqrt((1+A)^2+B^2)+sqrt((1-A)^2+B^2))/2,
    C is asin(E),
    D is log(F+sqrt(F^2-1)).

complex_acos([A, B], [C, D]) :-
    E is (sqrt((1+A)^2+B^2)-sqrt((1-A)^2+B^2))/2,
    F is (sqrt((1+A)^2+B^2)+sqrt((1-A)^2+B^2))/2,
    C is acos(E),
    D is -log(F+sqrt(F^2-1)).

complex_atan(A, B) :-
    complex_difference([[0, 1], A], C),
    complex_sum([[0, 1], A], D),
    complex_quotient([C, D], E),
    log([[2.718281828459045, 0], E], F),
    complex_uotient([F, [0, 2]], B).

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
true :+ complex_quotient([[1, 0], [0, 1]], _).
true :+ complex_exponentiation([[-1, 0], [0.5, 0]], _).
true :+ complex_exponentiation([[2.718281828459045, 0], [0, pi]], _).
true :+ complex_log([[2.718281828459045, 0], [-1, 0]], _).
true :+ complex_log([[0, 1], [0, 1]], _).
true :+ complex_sin([1.570796326794897, 1.316957896924817], _).
true :+ complex_cos([0, -1.316957896924817], _).
true :+ complex_tan([1.338972522294493, 0.4023594781085251], _).
true :+ complex_asin([2, 0], _).
true :+ complex_acos([2, 0], _).
true :+ complex_atan([1, 2], _).
