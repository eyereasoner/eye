% Hamiltonian path
% See https://en.wikipedia.org/wiki/Hamiltonian_path

:- op(1200, xfx, :+).

% Given edges
edge(v1,v2).
edge(v1,v3).
edge(v1,v5).
edge(v1,v6).
edge(v2,v3).
edge(v2,v4).
edge(v2,v6).
edge(v3,v4).
edge(v3,v6).
edge(v4,v5).
edge(v4,v6).

% Define undirected adjacency
adjacent(V, U) :- edge(V, U).
adjacent(V, U) :- edge(U, V).

% Collect all vertices
vertices(Vertices) :-
    findall(V, (edge(V, _); edge(_, V)), All),
    sort(All, Vertices).

% Hamiltonian path predicate
hamiltonian_path(Path) :-
    vertices(Vs),
    length(Vs, N),
    length(Path, N),
    Path = [Start | _],
    member(Start, Vs),
    ham_path(Path, [Start]).

% Base case: single vertex
ham_path([_], _).

% Recursive case: extend path
ham_path([A,B|T], Visited) :-
    adjacent(A, B),
    \+ member(B, Visited),
    ham_path([B|T], [B|Visited]).

% query
true :+ hamiltonian_path(_).

