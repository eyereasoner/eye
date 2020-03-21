% ====================================
% Solving system of 4 linear equations
% ====================================
%
% See https://en.wikipedia.org/wiki/System_of_linear_equations

:- use_module(library(clpr)).

:-
    {   X1+X2 = 856,
        X3+X4 = 1308,
        0.07*X1-0.93*X3 = 0,
        -0.89*X2+0.11*X4 = 0
    },
    format('@prefix : <http://josd.github.io/eye/reasoning/4eq#>.~n'),
    format('~n'),
    format('[] :A ((1 1 0 0) (0 0 1 1) (0.07 0 -0.93 0) (0 -0.89 0 0.11));~n'),
    format('    :b (856 1308 0 0);~n'),
    format('    :x (~w ~w ~w ~w).~n', [X1, X2, X3, X4]),
    halt.
