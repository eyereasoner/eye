:- use_module(library(clpr)).

pfx(':', '<https://raw.githubusercontent.com/josd/eye/master/reasoning/4eq#>').

'<https://raw.githubusercontent.com/josd/eye/master/reasoning/4eq#solve>'([A, B], X) :-
    A = [[A11, A12, A13, A14],
         [A21, A22, A23, A24],
         [A31, A32, A33, A34],
         [A41, A42, A43, A44]],
    B = [B1, B2, B3, B4],
    {   A11*X1+A12*X2+A13*X3+A14*X4 = B1,
        A21*X1+A22*X2+A23*X3+A24*X4 = B2,
        A31*X1+A32*X2+A33*X3+A34*X4 = B3,
        A41*X1+A42*X2+A43*X3+A44*X4 = B4
    },
    X = [X1, X2, X3, X4].
