% Newton-Raphson method

% functions
f(1, X, Y) :- Y is X*X - 2.
f(2, X, Y) :- Y is log(X) - 1.
f(3, X, Y) :- Y is sin(X).

% function derivatives
fd(1, X, Y) :- Y is 2*X.
fd(2, X, Y) :- Y is 1/X.
fd(3, X, Y) :- Y is cos(X).

% Newton-Raphson iteration
'<https://eyereasoner.github.io/ns#findRoot>'([N, X, Tolerance], Root) :-
    f(N, X, FX),
    fd(N, X, FDX),
    (   abs(FX) < Tolerance
    ->  Root = X
    ;   NewX is X - FX/FDX,
        '<https://eyereasoner.github.io/ns#findRoot>'([N, NewX, Tolerance], Root)
    ).
