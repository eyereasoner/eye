% N-Queens puzzle
% See https://en.wikipedia.org/wiki/Eight_queens_puzzle
% Original code at https://hanslen.github.io/2016/05/02/AI-problem-N-queens-problem-%E2%80%93-solved-in-prolog/

:- op(1200, xfx, :+).

'urn:example:queens'(N, Qs) :-
    range(1, N, Us),
    queens(Us, [], Qs).

% queens(+Unplaced, ?Placed, ?Queens)
queens([], Qs, Qs).
queens(Us, Ps, Qs) :-
    select(Q, Us, Us1),
    \+attack(Q, 1, Ps),
    queens(Us1, [Q|Ps], Qs).

% range(+I, +J, -Ns): Ns is the list of integers between I and J inclusive
range(J, J, [J]).
range(I, J, [I|Ns]) :-
    I < J,
    I1 is I+1,
    range(I1, J, Ns).

% attack(+Q, +N, +Qs): queen in row Q attacks from distance N one or more of the queens in rows Qs on a diagonal
attack(X, N, [Y|_]) :-
    X is Y+N.
attack(X, N, [Y|_]) :-
    X is Y-N.
attack(X, N, [_|Ys]) :-
    N1 is N+1,
    attack(X, N1, Ys).

% query
true :+ 'urn:example:queens'(8, _).
