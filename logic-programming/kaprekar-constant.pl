% Kaprekar's constant
% See https://en.wikipedia.org/wiki/6174

% recursive case till 6174 is reached
'<urn:example:kaprekar>'(A, [B, C]) :-
    A =\= 0,
    number_to_digits(A, D),
    sort(0, @=<, D, E),
    sort(0, @>=, D, F),
    digits_to_number(E, G),
    digits_to_number(F, H),
    I is H-G,
    J is B+1,
    (   I =:= 6174
    ->  C = J
    ;   '<urn:example:kaprekar>'(I, [J, C])
    ).

% convert 4 digit number to digits
number_to_digits(A, [B, C, D, E]) :-
    B is A // 1000,
    F is A rem 1000,
    C is F // 100,
    G is F rem 100,
    D is G // 10,
    E is G rem 10.

% convert 4 digits to number
digits_to_number([A, B, C, D], E) :-
    E is A*1000+B*100+C*10+D.

% query
(true :+ '<urn:example:kaprekar>'(I, [0, _])) :-
    between(0, 9999, I).
