% Dijkstra's algorithm to find the shortest path
% thanks to DeepSeek

:- op(1200, xfx, :+).

:- use_module(library(sort)).

% Facts: Define the graph as edges with weights
'<urn:example:edge>'([a, b], 2).
'<urn:example:edge>'([a, c], 4).
'<urn:example:edge>'([b, c], 1).
'<urn:example:edge>'([b, d], 7).
'<urn:example:edge>'([c, d], 3).

% Dijkstra's algorithm to find the shortest path
'<urn:example:shortest_path>'([Start, End], [Path, Cost]) :-
    dijkstra([[Start, 0]], End, [], ReversedPath, Cost),
    reverse(ReversedPath, Path).

dijkstra([[Node, Cost]|_], Node, Visited, [Node|Visited], Cost) :-
    !.
dijkstra([[Node, Cost]|RestPaths], End, Visited, Path, TotalCost) :-
    findall([Neighbor, NewCost], 
        (   '<urn:example:edge>'([Node, Neighbor], EdgeCost), 
            \+ member(Neighbor, Visited), 
            NewCost is Cost+EdgeCost
        ), 
        Neighbors
    ),
    append(RestPaths, Neighbors, AllPaths),
    predsort(compare_paths, AllPaths, SortedPaths),
    dijkstra(SortedPaths, End, [Node|Visited], Path, TotalCost).

% compare paths based on cost
compare_paths(<, [_, Cost1], [_, Cost2]) :-
    Cost1<Cost2,
    !.
compare_paths(>, [_, Cost1], [_, Cost2]) :-
    Cost1>Cost2,
    !.
compare_paths(=, [_, Cost], [_, Cost]).

% query
true :+ '<urn:example:shortest_path>'([a, d], [_, _]).
