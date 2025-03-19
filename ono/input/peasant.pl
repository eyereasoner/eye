% Peasant calculations
% See https://en.wikipedia.org/wiki/Ancient_Egyptian_multiplication

:- op(1200, xfx, :+).

'urn:example:prod'([0, _], 0).
'urn:example:prod'([X, Y], Z) :-
    X =\= 0,
    0 =:= X rem 2,
    S is X//2,
    T is Y+Y,
    'urn:example:prod'([S, T], Z).
'urn:example:prod'([X, Y], Z) :-
    X =\= 0,
    1 =:= X rem 2,
    S is X//2,
    T is Y+Y,
    'urn:example:prod'([S, T], R),
    Z is R+Y.

'urn:example:pow'([_, 0], 1).
'urn:example:pow'([X, Y], Z) :-
    Y =\= 0,
    0 =:= Y rem 2,
    S is X*X,
    T is Y//2,
    'urn:example:pow'([S, T], Z).
'urn:example:pow'([X, Y], Z) :-
    Y =\= 0,
    1 =:= Y rem 2,
    S is X*X,
    T is Y//2,
    'urn:example:pow'([S, T], R),
    Z is R*X.

% query
true :+ 'urn:example:prod'([3, 0], _).
true :+ 'urn:example:prod'([5, 6], _).
true :+ 'urn:example:prod'([238, 13], _).
true :+ 'urn:example:prod'([8367238, 27133], _).
true :+ 'urn:example:prod'([62713345408367238, 40836723862713345], _).
true :+ 'urn:example:prod'([4083672386271334562713345408367238, 4083672386271334562713345408367238], _).
true :+ 'urn:example:pow'([3, 0], _).
true :+ 'urn:example:pow'([5, 6], _).
true :+ 'urn:example:pow'([238, 13], _).
true :+ 'urn:example:pow'([8367238, 2713], _).
