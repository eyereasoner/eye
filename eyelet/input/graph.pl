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

path(A, B) :+
    oneway(A, B).
path(A, C) :+
    path(A, B),
    path(B, C).

% query
true :+ path(_, nantes).
