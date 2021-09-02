% Calculating the area of a polygon

webize(eye/1,'https://josd.github.io/eye/thinking/ns#').

eye(area([_],0)).
eye(area([[A,B],[C,D]|E],F)) :-
    eye(area([[C,D]|E],G)),
    F is (A*D-B*C)/2+G.

% test cases
case(webize(_NS,_P)).
case(eye(area([[3,2],[6,2],[7,6],[4,6],[5,5],[5,3],[3,2]],_ANSWER))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
