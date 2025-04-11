% Ackermann function
% See https://en.wikipedia.org/wiki/Ackermann_function

:- op(1200, xfx, :+).

% ackermann(x, y, z)
'urn:example:ackermann'([A, B], C) :-
    D is B+3,
    ackermann(A, D, 2, E),
    C is E-3.

% succ (x=0)
ackermann(0, A, _, B) :-
    !,
    B is A+1.

% sum (x=1)
ackermann(1, A, B, C) :-
    !,
    C is A+B.

% product (x=2)
ackermann(2, A, B, C) :-
    !,
    C is A*B.

% exponentiation (x=3), tetration (x=4), pentation (x=5), hexation (x=6), etc
ackermann(_, 0, _, 1) :-
    !.
ackermann(A, B, C, D) :-
    E is B-1,
    ackermann(A, E, C, F),
    G is A-1,
    ackermann(G, F, C, D).

% query
true :+ 'urn:example:ackermann'([0, 6], _).
true :+ 'urn:example:ackermann'([1, 2], _).
true :+ 'urn:example:ackermann'([1, 7], _).
true :+ 'urn:example:ackermann'([2, 2], _).
true :+ 'urn:example:ackermann'([2, 9], _).
true :+ 'urn:example:ackermann'([3, 4], _).
true :+ 'urn:example:ackermann'([3, 14], _).
true :+ 'urn:example:ackermann'([4, 0], _).
true :+ 'urn:example:ackermann'([4, 1], _).
true :+ 'urn:example:ackermann'([4, 2], _).
true :+ 'urn:example:ackermann'([5, 0], _).
