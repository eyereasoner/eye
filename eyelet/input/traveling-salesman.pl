% Traveling salesman problem
% See https://en.wikipedia.org/wiki/Travelling_salesman_problem
% Original code from https://chat.mistral.ai/chat

:- op(1200, xfx, :+).

% Define the distance matrix for the cities
distance([aaa, bbb], 10).
distance([aaa, ccc], 15).
distance([aaa, ddd], 20).
distance([aaa, eee], 25).
distance([aaa, fff], 30).

distance([bbb, ccc], 35).
distance([bbb, ddd], 40).
distance([bbb, eee], 20).
distance([bbb, fff], 25).

distance([ccc, ddd], 30).
distance([ccc, eee], 15).
distance([ccc, fff], 10).

distance([ddd, eee], 10).
distance([ddd, fff], 35).
distance([ddd, ggg], 5).

distance([eee, fff], 20).

% Calculate the total distance of a tour
total_distance([_], 0).
total_distance([X, Y|Tail], D) :-
    (   distance([X, Y], D1)
    ;   distance([Y, X], D1)
    ),
    total_distance([Y|Tail], D2),
    D is D1 + D2.

% Solve the TSP
tsp([Start|Cities], [Start|Tour], Distance) :-
    permutation(Cities, Tour),
    total_distance([Start|Tour], Distance).

% Find the optimal tour
optimalTour(Cities, Optimal) :-
    findall([T, D], tsp(Cities, T, D), [Sol|Sols]),
    min_distance([Sol|Sols], Sol, Optimal).

min_distance([], A, A) :-
    !.
min_distance([[A, B]|C], [_, E], F) :-
    B < E,
    !,
    min_distance(C, [A, B], F).
min_distance([[_, _]|A], B, C) :-
    min_distance(A, B, C).

% query
true :+ optimalTour([aaa, bbb, ccc, ddd, eee, fff, ggg], _).
