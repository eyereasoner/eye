% Towers of Hanoi

move(0,[_,_,_]) :-
    !.
move(N,[A,B,C]) :-
    M is N-1,
    move(M,[A,C,B]),
    move(M,[C,B,A]).

% query implies goal
move(14,[left,centre,right]) => goal.
