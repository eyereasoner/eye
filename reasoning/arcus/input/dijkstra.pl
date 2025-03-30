% Dijkstra's algorithm to find the shortest path
% See https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
% Original from https://github.com/agx-r/Dijkstra-s-Algorithm

:- op(1200, xfx, :+).

:- discontiguous((:+)/2).

:- dynamic(edge/2).

% context
'urn:example:dijkstra'(A, B) :- dijkstra(A, B).

% edges
edge([a, b], 4).
edge([a, c], 2).
edge([b, c], 1).
edge([b, d], 5).
edge([c, d], 8).
edge([c, e], 10).
edge([d, e], 2).
edge([d, f], 6).
edge([e, f], 3).

edge([A, B], C) :+
    edge([B, A], C).

% Dijkstra's algorithm
dijkstra([Start, Goal], [Path, Cost]) :-
    dijkstra([[0, Start]], Goal, [], RevPath, Cost),
    reverse(RevPath, Path).

dijkstra([[Cost, Goal|Path]|_], Goal, _, [Goal|Path], Cost).
dijkstra([[Cost, Node|Path]|Queue], Goal, Visited, ResultPath, ResultCost) :-
    findall([NewCost, Neighbor, Node|Path],
        (   edge([Node, Neighbor], Weight),
            \+ member(Neighbor, Visited),
            NewCost is Cost + Weight),
        Neighbors),
    append(Queue, Neighbors, NewQueue),
    sort(NewQueue, SortedQueue),
    dijkstra(SortedQueue, Goal, [Node|Visited], ResultPath, ResultCost).

% query
true :+ 'urn:example:dijkstra'([a, f], [_, _]).
