% Towers of Hanoi

webize(eye/1,'https://josd.github.io/eye/thinking/ns#').

eye(move(0,[_,_,_])) :-
    !.
eye(move(N,[A,B,C])) :-
    M is N-1,
    eye(move(M,[A,C,B])),
    eye(move(M,[C,B,A])).

% test cases
case(webize(_NS,_P)).
case(eye(move(14,['left','centre','right']))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
