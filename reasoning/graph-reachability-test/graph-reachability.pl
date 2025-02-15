% graph reachability

:- op(1200, xfx, :+).

% context
'<https://eyereasoner.github.io/ns#isReachable>'(Start, Goal) :-
    is_reachable(Start, Goal).

% graph representation as directed edges
edge(a, b).
edge(a, c).
edge(b, d).
edge(c, e).
edge(d, f).
edge(e, f).
edge(f, g).

% base case
reachable(Node, Node, _).

% recursive case
reachable(Start, Goal, Visited) :-
    edge(Start, Next),
    \+ member(Next, Visited),
    reachable(Next, Goal, [Next|Visited]).

% reachability check
is_reachable(Start, Goal) :-
    reachable(Start, Goal, [Start]).

% query
true :+ '<https://eyereasoner.github.io/ns#isReachable>'(a, f).
true :+ \+ '<https://eyereasoner.github.io/ns#isReachable>'(b, e).
true :+ '<https://eyereasoner.github.io/ns#isReachable>'(c, g).
