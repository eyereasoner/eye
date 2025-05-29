% Traversing graph paths

:- op(1200, xfx, :+).

oneway(paris, orleans).
oneway(paris, chartres).
oneway(paris, amiens).
oneway(orleans, blois).
oneway(orleans, bourges).
oneway(blois, tours).
oneway(chartres, lemans).
oneway(lemans, angers).
oneway(lemans, tours).
oneway(angers, nantes).

path([A, B], go(A, B, goal)) :-
    oneway(A, B).
path([A, C], go(A, B, D)) :-
    oneway(A, B),
    path([B, C], D).

% query
true :+ path([_, nantes], _).
