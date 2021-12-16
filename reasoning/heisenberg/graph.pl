% Traversing graph paths

:- dynamic(path/2).

oneway(paris,orleans).
oneway(paris,chartres).
oneway(paris,amiens).
oneway(orleans,blois).
oneway(orleans,bourges).
oneway(blois,tours).
oneway(chartres,lemans).
oneway(lemans,angers).
oneway(lemans,tours).
oneway(angers,nantes).

oneway(A,B) => path(A,B).
path(A,B),path(B,C) => path(A,C).

% query implies goal
path(_CITY,nantes) => goal.
