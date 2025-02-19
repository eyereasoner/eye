% Monkey and bananas planning problem
% Original code at https://www.cs.toronto.edu/~hector/PublicTCSlides.pdf

:- op(1200, xfx, :+).

% plan: list of moves from initial state to goal state
'<urn:example:plan>'(L) :-
    initial_state(I),
    goal_state(G),
    reachable(I, L, G).

% reachable(S1, L, S2): S2 is reachable from S1 using moves L
reachable(S, [], S).
reachable(S1, [M|L], S3) :-
    legal_move(S1, M, S2),
    reachable(S2, L, S3).

% initial state of bananas, monkey, box, on_box and has_bananas
initial_state([loc1, loc2, loc3, n, n]).

% goal state where the monkey has the bananas
goal_state([_, _, _, _, y]).

% legal_move(BeforeState, Move, AfterState)
legal_move([B, M, M, n, H], climb_on, [B, M, M, y, H]).
legal_move([B, M, M, y, H], climb_off, [B, M, M, n, H]).
legal_move([B, B, B, y, n], grab, [B, B, B, y, y]).
legal_move([B, M, M, n, H], push(X), [B, X, X, n, H]) :-
    member(X, [loc1, loc2, loc3]),
    X \= M.
legal_move([B, M, L, n, H], go(X), [B, X, L, n, H]) :-
    member(X, [loc1, loc2, loc3]),
    X \= M.

% query
(true :+ '<urn:example:plan>'(L)) :-
    between(1, 5, I),
    length(L, I).
