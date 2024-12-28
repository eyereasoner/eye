% See https://en.wikipedia.org/wiki/Complex_number

'<urn:example:complex:sum>'([[A, B], [C, D]], [E, F]) :-
    E is A+C,
    F is B+D.

'<urn:example:complex:difference>'([[A, B], [C, D]], [E, F]) :-
    E is A-C,
    F is B-D.

'<urn:example:complex:product>'([[A, B], [C, D]], [E, F]) :-
    E is A*C-B*D,
    F is A*D+B*C.

'<urn:example:complex:quotient>'([[A, B], [C, D]], [E, F]) :-
    E is (A*C+B*D)/(C^2+D^2),
    F is (B*C-A*D)/(C^2+D^2).

'<urn:example:complex:exponentiation>'([[A, B], [C, D]], [E, F]) :-
    polar([A, B], [G, H]),
    E is G^C*exp(-D*H)*cos(D*log(G)+C*H),
    F is G^C*exp(-D*H)*sin(D*log(G)+C*H).

'<urn:example:complex:log>'([[A, B], [C, D]], [E, F]) :-
    polar([A, B], [G, H]),
    polar([C, D], [I, J]),
    K is log(G),
    L is log(I),
    '<urn:example:complex:quotient>'([[L, J], [K, H]], [E, F]).

'<urn:example:complex:sin>'([A, B], [C, D]) :-
    C is sin(A)*(exp(B)+exp(-B))/2,
    D is cos(A)*(exp(B)-exp(-B))/2.

'<urn:example:complex:cos>'([A, B], [C, D]) :-
    C is cos(A)*(exp(B)+exp(-B))/2,
    D is -sin(A)*(exp(B)-exp(-B))/2.

'<urn:example:complex:tan>'(A, B) :-
    '<urn:example:complex:sin>'(A, C),
    '<urn:example:complex:cos>'(A, D),
    '<urn:example:complex:quotient>'([C, D], B).

'<urn:example:complex:asin>'([A, B], [C, D]) :-
    E is (sqrt((1+A)^2+B^2)-sqrt((1-A)^2+B^2))/2,
    F is (sqrt((1+A)^2+B^2)+sqrt((1-A)^2+B^2))/2,
    C is asin(E),
    D is log(F+sqrt(F^2-1)).

'<urn:example:complex:acos>'([A, B], [C, D]) :-
    E is (sqrt((1+A)^2+B^2)-sqrt((1-A)^2+B^2))/2,
    F is (sqrt((1+A)^2+B^2)+sqrt((1-A)^2+B^2))/2,
    C is acos(E),
    D is -log(F+sqrt(F^2-1)).

'<urn:example:complex:atan>'(A, B) :-
    '<urn:example:complex:difference>'([[0, 1], A], C),
    '<urn:example:complex:sum>'([[0, 1], A], D),
    '<urn:example:complex:quotient>'([C, D], E),
    X is 0+e,
    '<urn:example:complex:log>'([[X, 0], E], F),
    '<urn:example:complex:quotient>'([F, [0, 2]], B).

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

% queries
true :+ '<urn:example:complex:quotient>'([[1, 0], [0, 1]], _).
true :+ '<urn:example:complex:exponentiation>'([[-1, 0], [0.5, 0]], _).
true :+ '<urn:example:complex:exponentiation>'([[e, 0], [0, pi]], _).
true :+ '<urn:example:complex:log>'([[e, 0], [-1, 0]], _).
true :+ '<urn:example:complex:log>'([[0, 1], [0, 1]], _).
true :+ '<urn:example:complex:sin>'([1.570796326794897, 1.316957896924817], _).
true :+ '<urn:example:complex:cos>'([0, -1.316957896924817], _).
true :+ '<urn:example:complex:tan>'([1.338972522294493, 0.4023594781085251], _).
true :+ '<urn:example:complex:asin>'([2, 0], _).
true :+ '<urn:example:complex:acos>'([2, 0], _).
true :+ '<urn:example:complex:atan>'([1, 2], _).
