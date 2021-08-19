% Towers of Hanoi

move(0,_,_,_) :-
    !.
move(N,A,B,C) :-
    M is N-1,
    move(M,A,C,B),
    move(M,C,B,A).

% test cases
case(move(14,'left','centre','right')).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
