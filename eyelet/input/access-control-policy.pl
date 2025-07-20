% Access control policy example

:- op(1200, xfx, :+).

policy(test1, 'PolicyX').
has(test1, 'A').
has(test1, 'B').
has(test1, 'C').
policy('PolicyX').
allOf('PolicyX', 'A').
allOf('PolicyX', 'B').
anyOf('PolicyX', 'C').
noneOf('PolicyX', 'D').

pass(A, allOfTest) :-
    policy(B, A),
    policy(A),
    \+ (
        allOf(A, C),
        \+ has(B, C)
    ).

pass(A, anyOfTest) :-
    policy(B, A),
    policy(A),
    findall(C,
        (
            anyOf(A, C),
            has(B, C)
        ),
        D
    ),
    length(D, E),
    E \= 0.

pass(A, noneOfTest) :-
    policy(B, A),
     policy(A),
    findall(C,
        (
            noneOf(A, C),
            has(B, C)
        ),
        D
    ),
    length(D, 0).

% query
true :+
    policy(A),
    pass(A, allOfTest),
    pass(A, anyOfTest),
    pass(A, noneOfTest).
