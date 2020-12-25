% Towers of Hanoi

:- initialization(test).

test :-
    hanoi(21),
    write('true.'),
    nl.

move(0,_,_,_).
move(N,A,B,C) :-
    M is N-1,
    move(M,A,C,B),
    move(M,C,B,A).

hanoi(N) :-
    move(N,'left','centre','right').
