% Kaprekar's constant
% See https://en.wikipedia.org/wiki/6174

:- op(1200, xfx, :+).

% recursive case till 6174 is reached
kaprekar(A, B, C) :-
    A =\= 0,
    numberToDigits(A, D),
    keysort(D, E),
    reverse(E, F),
    digitsToNumber(E, G),
    digitsToNumber(F, H),
    I is H-G,
    J is B+1,
    (   I =:= 6174
    ->  C = J
    ;   kaprekar(I, J, C)
    ).

% convert 4 digit number to digits
numberToDigits(A, [B-0, C-0, D-0, E-0]) :-
    B is A // 1000,
    F is A rem 1000,
    C is F // 100,
    G is F rem 100,
    D is G // 10,
    E is G rem 10.

% convert 4 digits to number
digitsToNumber([A-0, B-0, C-0, D-0], E) :-
    E is A*1000+B*100+C*10+D.

% recursion count
recursionCount(I, J) :-
    kaprekar(I, 0, J).

% query
(true :+ recursionCount(I, _)) :-
    between(1, 10000, I).
