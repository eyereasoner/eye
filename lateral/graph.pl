% Traversing graph paths

webize(el/1,'https://josd.github.io/eye/lateral/ns#').

el(oneway(el(paris),el(orleans))).
el(oneway(el(paris),el(chartres))).
el(oneway(el(paris),el(amiens))).
el(oneway(el(orleans),el(blois))).
el(oneway(el(orleans),el(bourges))).
el(oneway(el(blois),el(tours))).
el(oneway(el(chartres),el(lemans))).
el(oneway(el(lemans),el(angers))).
el(oneway(el(lemans),el(tours))).
el(oneway(el(angers),el(nantes))).

el(path(A,B)) :-
    el(oneway(A,B)).
el(path(A,B)) :-
    el(oneway(A,C)),
    el(path(C,B)).

% test cases
case(webize(_NS,_P)).
case(el(path(_CITY,el(nantes)))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
