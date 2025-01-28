% Dijkstra's algorithm to find the shortest path
% Original from https://github.com/agx-r/Dijkstra-s-Algorithm

:- op(1200, xfx, :+).

:- discontiguous (:+)/2.

:- dynamic('<urn:example:edge>'/2).

% edges
'<urn:example:edge>'([a, b], 4).
'<urn:example:edge>'([a, c], 2).
'<urn:example:edge>'([b, c], 1).
'<urn:example:edge>'([b, d], 5).
'<urn:example:edge>'([c, d], 8).
'<urn:example:edge>'([c, e], 10).
'<urn:example:edge>'([d, e], 2).
'<urn:example:edge>'([d, f], 6).
'<urn:example:edge>'([e, f], 3).

'<urn:example:edge>'([A, B], C) :+
    '<urn:example:edge>'([B, A], C).

% Dijkstra's algorithm
'<urn:example:dijkstra>'([Start, Goal], [Path, Cost]) :-
    dijkstra([[0, Start]], Goal, [], RevPath, Cost),
    reverse(RevPath, Path).

dijkstra([[Cost, Goal|Path]|_], Goal, _, [Goal|Path], Cost).
dijkstra([[Cost, Node|Path]|Queue], Goal, Visited, ResultPath, ResultCost) :-
    findall([NewCost, Neighbor, Node|Path],
        (   '<urn:example:edge>'([Node, Neighbor], Weight),
            \+ member(Neighbor, Visited),
            NewCost is Cost + Weight),
        Neighbors),
    append(Queue, Neighbors, NewQueue),
    sort(NewQueue, SortedQueue),
    dijkstra(SortedQueue, Goal, [Node|Visited], ResultPath, ResultCost).

% query
true :+ '<urn:example:dijkstra>'([a, f], [_, _]).
