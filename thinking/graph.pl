% Traversing graph paths

webize(eye/1,'https://josd.github.io/eye/thinking/ns#').

eye(oneway(eye(paris),eye(orleans))).
eye(oneway(eye(paris),eye(chartres))).
eye(oneway(eye(paris),eye(amiens))).
eye(oneway(eye(orleans),eye(blois))).
eye(oneway(eye(orleans),eye(bourges))).
eye(oneway(eye(blois),eye(tours))).
eye(oneway(eye(chartres),eye(lemans))).
eye(oneway(eye(lemans),eye(angers))).
eye(oneway(eye(lemans),eye(tours))).
eye(oneway(eye(angers),eye(nantes))).

eye(path(A,B)) :-
    eye(oneway(A,B)).
eye(path(A,B)) :-
    eye(oneway(A,C)),
    eye(path(C,B)).

% test cases
case(webize(_NS,_P)).
case(eye(path(_CITY,eye(nantes)))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
