% Calculating the area of a polygon

:- op(1200, xfx, :+).

'<https://eyereasoner.github.io/ns#area>'([_], 0).
'<https://eyereasoner.github.io/ns#area>'([[A, B], [C, D]|E], F) :-
    '<https://eyereasoner.github.io/ns#area>'([[C, D]|E], G),
    F is (A*D-B*C)/2+G.

% query
true :+ '<https://eyereasoner.github.io/ns#area>'([[3, 2], [6, 2], [7, 6], [4, 6], [5, 5], [5, 3], [3, 2]], _).
