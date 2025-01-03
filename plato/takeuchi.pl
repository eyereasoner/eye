% Takeuchi function
% See https://en.wikipedia.org/wiki/Tak_(function)

:- op(1200, xfx, :+).

'<urn:example:tak>'([X, Y, Z], Z) :-
    X =< Y,
    !.
'<urn:example:tak>'([X, Y, Z], A) :-
    X1 is X-1,
    '<urn:example:tak>'([X1, Y, Z], A1),
    Y1 is Y-1,
    '<urn:example:tak>'([Y1, Z, X], A2),
    Z1 is Z-1,
    '<urn:example:tak>'([Z1, X, Y], A3),
    '<urn:example:tak>'([A1, A2, A3], A).

% query
true :+ '<urn:example:tak>'([34, 13, 8], _).
