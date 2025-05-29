% Towers of Hanoi
% See https://en.wikipedia.org/wiki/Tower_of_Hanoi

:- op(1200, xfx, :+).

move(0, [_, _, _]) :-
    !.
move(N, [A, B, C]) :-
    M is N-1,
    move(M, [A, C, B]),
    move(M, [C, B, A]).

% query
true :+ move(14, [left, centre, right]).
