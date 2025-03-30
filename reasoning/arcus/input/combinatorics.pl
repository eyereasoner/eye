% Combinatorics
% See https://en.wikipedia.org/wiki/Combinatorics

:- op(1200, xfx, :+).

% combination
'urn:example:combination'([0, _], []).
'urn:example:combination'([I, As], Bs) :-
    I > 0,
    select(B, As, Cs),
    J is I-1,
    'urn:example:combination'([J, Cs], Ds),
    sort([B|Ds], Bs).

% permutation
'urn:example:permutation'(A, B) :-
    permutation(A, B).

% query
true :+ 'urn:example:combination'([0, [1, 2, 3, 4, 5]], _).
true :+ 'urn:example:combination'([1, [1, 2, 3, 4, 5]], _).
true :+ 'urn:example:combination'([2, [1, 2, 3, 4, 5]], _).
true :+ 'urn:example:combination'([3, [1, 2, 3, 4, 5]], _).
true :+ 'urn:example:combination'([4, [1, 2, 3, 4, 5]], _).
true :+ 'urn:example:combination'([5, [1, 2, 3, 4, 5]], _).
true :+ 'urn:example:permutation'([1, 2, 3, 4, 5], _).
true :+ 'urn:example:permutation'([r, v, o], _).
