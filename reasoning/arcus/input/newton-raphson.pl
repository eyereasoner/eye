% Newton-Raphson method
% See https://en.wikipedia.org/wiki/Newton%27s_method

% functions
f(1, X, Y) :- Y is X*X - 2.
f(2, X, Y) :- Y is log(X) - 1.
f(3, X, Y) :- Y is sin(X).

% function derivatives
fd(1, X, Y) :- Y is 2*X.
fd(2, X, Y) :- Y is 1/X.
fd(3, X, Y) :- Y is cos(X).

% Newton-Raphson iteration
'urn:example:findRoot'([N, X, Tolerance], Root) :-
    f(N, X, FX),
    fd(N, X, FDX),
    (   abs(FX) < Tolerance
    ->  Root = X
    ;   NewX is X - FX/FDX,
        'urn:example:findRoot'([N, NewX, Tolerance], Root)
    ).

% query
true :+ 'urn:example:findRoot'([1, 1.0, 1.0e-15], _).
true :+ 'urn:example:findRoot'([2, 2.0, 1.0e-15], _).
true :+ 'urn:example:findRoot'([3, 3.0, 1.0e-15], _).
