% Peasant calculations
% See https://en.wikipedia.org/wiki/Ancient_Egyptian_multiplication

:- op(1200, xfx, :+).

prod([0, _], 0).
prod([X, Y], Z) :-
    X =\= 0,
    0 =:= X rem 2,
    S is X//2,
    T is Y+Y,
    prod([S, T], Z).
prod([X, Y], Z) :-
    X =\= 0,
    1 =:= X rem 2,
    S is X//2,
    T is Y+Y,
    prod([S, T], R),
    Z is R+Y.

pow([_, 0], 1).
pow([X, Y], Z) :-
    Y =\= 0,
    0 =:= Y rem 2,
    S is X*X,
    T is Y//2,
    pow([S, T], Z).
pow([X, Y], Z) :-
    Y =\= 0,
    1 =:= Y rem 2,
    S is X*X,
    T is Y//2,
    pow([S, T], R),
    Z is R*X.

% query
true :+ prod([3, 0], _).
true :+ prod([5, 6], _).
true :+ prod([238, 13], _).
true :+ prod([8367238, 27133], _).
true :+ prod([62713345408367238, 40836723862713345], _).
true :+ prod([4083672386271334562713345408367238, 4083672386271334562713345408367238], _).
true :+ pow([3, 0], _).
true :+ pow([5, 6], _).
true :+ pow([238, 13], _).
true :+ pow([8367238, 2713], _).
