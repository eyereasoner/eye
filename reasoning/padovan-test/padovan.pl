% See https://en.wikipedia.org/wiki/Padovan_sequence

:- op(1200, xfx, :+).

'<https://eyereasoner.github.io/ns#padovan>'(A, B) :-
    padovan(A, 0, 1, 1, B).

padovan(0, A, _, _, A).
padovan(1, _, A, _, A).
padovan(2, _, _, A, A).
padovan(A, B, C, D, E) :-
    A > 2,
    F is A-1,
    G is B+C,
    padovan(F, C, D, G, E).

'<https://eyereasoner.github.io/ns#plastic_ratio>'(A, B) :-
    '<https://eyereasoner.github.io/ns#padovan>'(A, C),
    D is A+1,
    '<https://eyereasoner.github.io/ns#padovan>'(D, E),
    B is E/C.

% query
true :+ '<https://eyereasoner.github.io/ns#padovan>'(1, _).
true :+ '<https://eyereasoner.github.io/ns#padovan>'(2, _).
true :+ '<https://eyereasoner.github.io/ns#padovan>'(3, _).
true :+ '<https://eyereasoner.github.io/ns#padovan>'(4, _).
true :+ '<https://eyereasoner.github.io/ns#padovan>'(5, _).
true :+ '<https://eyereasoner.github.io/ns#padovan>'(91, _).
true :+ '<https://eyereasoner.github.io/ns#padovan>'(283, _).
true :+ '<https://eyereasoner.github.io/ns#padovan>'(3674, _).
true :+ '<https://eyereasoner.github.io/ns#plastic_ratio>'(1, _).
true :+ '<https://eyereasoner.github.io/ns#plastic_ratio>'(10, _).
true :+ '<https://eyereasoner.github.io/ns#plastic_ratio>'(100, _).
true :+ '<https://eyereasoner.github.io/ns#plastic_ratio>'(1000, _).
