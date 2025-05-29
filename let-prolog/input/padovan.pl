% See https://en.wikipedia.org/wiki/Padovan_sequence

:- op(1200, xfx, :+).

padovan(A, B) :-
    padovan(A, 0, 1, 1, B).

padovan(0, A, _, _, A).
padovan(1, _, A, _, A).
padovan(2, _, _, A, A).
padovan(A, B, C, D, E) :-
    A > 2,
    F is A-1,
    G is B+C,
    padovan(F, C, D, G, E).

plastic_ratio(A, B) :-
    padovan(A, C),
    D is A+1,
    padovan(D, E),
    B is E/C.

% query
true :+ padovan(1, _).
true :+ padovan(2, _).
true :+ padovan(3, _).
true :+ padovan(4, _).
true :+ padovan(5, _).
true :+ padovan(91, _).
true :+ padovan(283, _).
true :+ padovan(3674, _).
true :+ plastic_ratio(1, _).
true :+ plastic_ratio(10, _).
true :+ plastic_ratio(100, _).
true :+ plastic_ratio(1000, _).
