% Calculating the area of a polygon

ld(el/1,'https://josd.github.io/eye/lateral/ns#').

el(area([_],0)).
el(area([[A,B],[C,D]|E],F)) :-
    el(area([[C,D]|E],G)),
    F is (A*D-B*C)/2+G.

% test cases
case(ld(_NS,_P)).
case(el(area([[3,2],[6,2],[7,6],[4,6],[5,5],[5,3],[3,2]],_ANSWER))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
