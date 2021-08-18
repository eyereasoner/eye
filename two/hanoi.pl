% Towers of Hanoi

nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_move(0,_,_,_) :-
    !.
etc_move(N,A,B,C) :-
    M is N-1,
    etc_move(M,A,C,B),
    etc_move(M,C,B,A).

% test cases
case(etc_move(14,etc_left,etc_centre,etc_right)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
