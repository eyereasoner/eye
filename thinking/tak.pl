% Takeuchi function

webize(eye/1,'https://josd.github.io/eye/thinking/ns#').

eye(tak([X,Y,Z],Z)) :-
    X =< Y,
    !.
eye(tak([X,Y,Z],A)) :-
    X1 is X-1,
    eye(tak([X1,Y,Z],A1)),
    Y1 is Y-1,
    eye(tak([Y1,Z,X],A2)),
    Z1 is Z-1,
    eye(tak([Z1,X,Y],A3)),
    eye(tak([A1,A2,A3],A)).

% test cases
case(webize(_NS,_P)).
case(eye(tak([34,13,8],_A))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
