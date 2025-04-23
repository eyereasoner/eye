% graph reachability
% See https://en.wikipedia.org/wiki/Reachability

:- op(1200, xfx, :+).

% context
'urn:example:isReachable'(Start, Goal) :-
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
true :+ 'urn:example:isReachable'(a, f).
true :+ \+ 'urn:example:isReachable'(b, e).
true :+ 'urn:example:isReachable'(c, g).
