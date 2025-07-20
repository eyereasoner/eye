% Eulerian path
% See https://en.wikipedia.org/wiki/Eulerian_path

:- op(1200, xfx, :+).

% Given edges
edge(v1, v2).
edge(v1, v3).
edge(v1, v5).
edge(v1, v6).
edge(v2, v3).
edge(v2, v4).
edge(v2, v6).
edge(v3, v4).
edge(v3, v6).
edge(v4, v5).
edge(v4, v6).

% Define undirected adjacency
adjacent(V, U) :- edge(V, U).
adjacent(V, U) :- edge(U, V).

% Collect all vertices
vertices(Vertices) :-
    findall(V, (edge(V, _); edge(_, V)), All),
    sort(All, Vertices).

% Calculate degree of a vertex
degree(V, D) :-
    findall(1, adjacent(V, _), List),
    length(List, D).

% Check if vertex has odd degree
odd_degree(V) :-
    degree(V, Deg),
    Deg mod 2 =:= 1.

% Find all vertices with odd degree
find_odd_vertices(Odds) :-
    findall(V, (vertices(Vs), member(V, Vs), odd_degree(V)), Odds).

% Create sorted edge representation for undirected graph
make_sorted_edge(A, B, sorted(X, Y)) :-
    ( A @< B -> X = A, Y = B ; X = B, Y = A ).

% DFS to find Eulerian path
eulerian_path(Path) :-
    find_odd_vertices(Odds),
    (   Odds = [Start | _]  % Start at an odd-degree vertex if exists
    ;   Odds = [],          % Otherwise, start at any vertex with edges
        vertices(Vs),
        member(Start, Vs),
        degree(Start, Deg),
        Deg > 0
    ),
    findall(Edge, (edge(A, B), make_sorted_edge(A, B, Edge)), Edges),
    dfs(Start, [Start], Path, Edges).

dfs(Current, Visited, Path, RemainingEdges) :-
    (   RemainingEdges = []
    ->  reverse(Visited, Path)  % All edges used; path found
    ;   % Try each adjacent vertex
        adjacent(Current, Next),
        make_sorted_edge(Current, Next, Edge),
        select(Edge, RemainingEdges, NewRemaining), % Use edge
        dfs(Next, [Next | Visited], Path, NewRemaining)
    ).

% query
true :+ eulerian_path(_).

