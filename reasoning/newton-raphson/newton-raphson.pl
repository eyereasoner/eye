% Newton-Raphson method

% function f(X) = X^2 - 2
f(X, Y) :- Y is X*X - 2.

% derivative f'(X) = 2X
f_derivative(X, Y) :- Y is 2*X.

% Newton-Raphson iteration
'<https://eyereasoner.github.io/ns#findRoot>'([X, Tolerance], Root) :-
    f(X, FX),
    f_derivative(X, FDX),
    (   abs(FX) < Tolerance
    ->  Root = X
    ;   NewX is X - FX/FDX,
        '<https://eyereasoner.github.io/ns#findRoot>'([NewX, Tolerance], Root)
    ).
