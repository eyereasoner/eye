% Kaprekar's constant
% See https://en.wikipedia.org/wiki/6174

:- op(1200, xfx, :+).

% recursive case till 6174 is reached
'urn:example:kaprekar'(A, B, C) :-
    A =\= 0,
    'urn:example:numberToDigits'(A, D),
    keysort(D, E),
    reverse(E, F),
    'urn:example:digitsToNumber'(E, G),
    'urn:example:digitsToNumber'(F, H),
    I is H-G,
    J is B+1,
    (   I =:= 6174
    ->  C = J
    ;   'urn:example:kaprekar'(I, J, C)
    ).

% convert 4 digit number to digits
'urn:example:numberToDigits'(A, [B-0, C-0, D-0, E-0]) :-
    B is A // 1000,
    F is A rem 1000,
    C is F // 100,
    G is F rem 100,
    D is G // 10,
    E is G rem 10.

% convert 4 digits to number
'urn:example:digitsToNumber'([A-0, B-0, C-0, D-0], E) :-
    E is A*1000+B*100+C*10+D.

% recursion count
'urn:example:recursionCount'(I, J) :-
    'urn:example:kaprekar'(I, 0, J).

% query
(true :+ 'urn:example:recursionCount'(I, _)) :-
    between(1, 1000, I).
