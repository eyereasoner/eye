% ====================================
% Solving system of 4 linear equations
% ====================================
%
% See https://en.wikipedia.org/wiki/System_of_linear_equations

:- use_module(library(clpr)).

go :-
    {   X1+X2 = 856,
        X3+X4 = 1308,
        0.07*X1-0.93*X3 = 0,
        -0.89*X2+0.11*X4 = 0
    },
    writeln([X1, X2, X3, X4]).
