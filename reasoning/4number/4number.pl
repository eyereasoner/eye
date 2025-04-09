:- use_module(library(clpfd)).

'<https://eyereasoner.github.io/ns#4number>'([A, B, C, D], true) :-
    A + B + C + D #= 26,
    A * B * C * D #= -9100,
    A^2 + B^2 + C^2 + D^2 #= 994,
    A^3 + B^3 + C^3 + D^3 #= 15086,
    label([A, B, C, D]),
    !.
