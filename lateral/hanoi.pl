% Towers of Hanoi

wrapper(el,'https://josd.github.io/eye/lateral/ns#').

el(move(0,[_,_,_])) :-
    !.
el(move(N,[A,B,C])) :-
    M is N-1,
    el(move(M,[A,C,B])),
    el(move(M,[C,B,A])).

% test cases
case(wrapper(_NS,_P)).
case(el(move(14,['left','centre','right']))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
