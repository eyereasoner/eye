% See https://en.wikipedia.org/wiki/Padovan_sequence

:- op(1200, xfx, :+).

'urn:example:padovan'(A, B) :-
    padovan(A, 0, 1, 1, B).

padovan(0, A, _, _, A).
padovan(1, _, A, _, A).
padovan(2, _, _, A, A).
padovan(A, B, C, D, E) :-
    A > 2,
    F is A-1,
    G is B+C,
    padovan(F, C, D, G, E).

'urn:example:plastic_ratio'(A, B) :-
    'urn:example:padovan'(A, C),
    D is A+1,
    'urn:example:padovan'(D, E),
    B is E/C.

% query
true :+ 'urn:example:padovan'(1, _).
true :+ 'urn:example:padovan'(2, _).
true :+ 'urn:example:padovan'(3, _).
true :+ 'urn:example:padovan'(4, _).
true :+ 'urn:example:padovan'(5, _).
true :+ 'urn:example:padovan'(91, _).
true :+ 'urn:example:padovan'(283, _).
true :+ 'urn:example:padovan'(3674, _).
true :+ 'urn:example:plastic_ratio'(1, _).
true :+ 'urn:example:plastic_ratio'(10, _).
true :+ 'urn:example:plastic_ratio'(100, _).
true :+ 'urn:example:plastic_ratio'(1000, _).
