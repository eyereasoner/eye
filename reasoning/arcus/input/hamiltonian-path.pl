% Hamiltonian path
% See https://en.wikipedia.org/wiki/Hamiltonian_path
% Original code from https://webcms3.cse.unsw.edu.au/static/uploads/course/COMP4418/19T3/d112d4466d89b3455df9abb7188d5b318b81cc3fffa4fb1a32f43e68c1c57173/0c.Prolog.pdf

:- op(1200, xfx, :+).

% edges
edge(1, 5).
edge(1, 7).
edge(2, 1).
edge(2, 7).
edge(3, 1).
edge(3, 6).
edge(4, 3).
edge(4, 5).
edge(5, 8).
edge(6, 4).
edge(6, 5).
edge(7, 5).
edge(8, 6).
edge(8, 7).

% path
path(Node, Node, _, [Node]).
path(Start, Finish, Visited, [Start|Path]) :-
    edge(Start, X),
    \+member(X, Visited),
    path(X, Finish, [X|Visited], Path).

% hamiltonian
'urn:example:hamiltonianPath'(P) :-
    findall(V, (edge(V, _); edge(_, V)), Fs),
    sort(Fs, Vs),
    member(S, Vs),
    path(S, _, [S], P),
    sort(P, Vs).

% query
true :+ 'urn:example:hamiltonianPath'(_).
