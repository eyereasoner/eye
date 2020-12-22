% Traversing graph paths

:- initialization(test).

:- op(1150,xfx,-:).

:- dynamic((-:)/2).
:- dynamic(path/2).

test :-
    % query implies goal
    assertz((path(paris,nantes) -: goal)),
    consult(retina),
    retina,
    write('true.'),
    nl.

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

oneway(A,B) -: path(A,B).
path(A,B),path(B,C) -: path(A,C).
