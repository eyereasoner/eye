% Kaprekar's constant

% recursive case till 6174 is reached
'<https://eyereasoner.github.io/ns#kaprekar-constant>'(A, [B, C]) :-
    A =\= 0,
    '<https://eyereasoner.github.io/ns#numberToDigits>'(A, D),
    keysort(D, E),
    reverse(E, F),
    '<https://eyereasoner.github.io/ns#digitsToNumber>'(E, G),
    '<https://eyereasoner.github.io/ns#digitsToNumber>'(F, H),
    I is H-G,
    J is B+1,
    (   I =:= 6174
    ->  C = J
    ;   '<https://eyereasoner.github.io/ns#kaprekar-constant>'(I, [J, C])
    ).

% convert 4 digit number to digits
'<https://eyereasoner.github.io/ns#numberToDigits>'(A, [B-0, C-0, D-0, E-0]) :-
    B is A // 1000,
    F is A rem 1000,
    C is F // 100,
    G is F rem 100,
    D is G // 10,
    E is G rem 10.

% convert 4 digits to number
'<https://eyereasoner.github.io/ns#digitsToNumber>'([A-0, B-0, C-0, D-0], E) :-
    E is A*1000+B*100+C*10+D.

% recursion count
'<https://eyereasoner.github.io/ns#recursionCount>'(I, J) :-
    '<https://eyereasoner.github.io/ns#kaprekar-constant>'(I, [0, J]).
