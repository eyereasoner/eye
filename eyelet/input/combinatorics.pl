% Combinatorics
% See https://en.wikipedia.org/wiki/Combinatorics

:- op(1200, xfx, :+).

% combination
combination([0, _], []).
combination([I, As], Bs) :-
    I > 0,
    select(B, As, Cs),
    J is I-1,
    combination([J, Cs], Ds),
    sort([B|Ds], Bs).

% query
true :+ combination([0, [1, 2, 3, 4, 5]], _).
true :+ combination([1, [1, 2, 3, 4, 5]], _).
true :+ combination([2, [1, 2, 3, 4, 5]], _).
true :+ combination([3, [1, 2, 3, 4, 5]], _).
true :+ combination([4, [1, 2, 3, 4, 5]], _).
true :+ combination([5, [1, 2, 3, 4, 5]], _).
