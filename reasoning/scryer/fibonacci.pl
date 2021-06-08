% See https://en.wikipedia.org/wiki/Fibonacci_number

:- use_module(library(dcgs)).
:- use_module(library(lists)).

fibonacci(A, B) :-
    fibonacci(A, [0], [1], B).

fibonacci(0, A, _, A).
fibonacci(1, _, A, A).
fibonacci(A, B, C, D) :-
    A > 1,
    E is A-1,
    sum(B, C, F),
    fibonacci(E, C, F, D).

% Original code from https://codereview.stackexchange.com/questions/85225/adding-elements-in-two-lists-as-numbers
sum(Xs0, Ys0, Ls) :-
    reverse(Xs0, Xs),
    reverse(Ys0, Ys),
    phrase(sum(Xs, Ys, 0), Ls0),
    reverse(Ls0, Ls).

sum([], [], 0) -->
    [].
sum([], [], 1) -->
    [1].
sum([], [Y|Ys], Carry0) -->
    {   N0 is Y + Carry0,
        N is N0 mod 1000,
        Carry is N0 // 1000
    },
    [N],
    sum([], Ys, Carry).
sum([X|Xs], [Y|Ys], Carry0) -->
    {   N0 is X + Y + Carry0,
        N is N0 mod 1000,
        Carry is N0 // 1000
    },
    [N],
    sum(Xs, Ys, Carry).

% test cases
case(fibonacci(0, _)).
case(fibonacci(1, _)).
case(fibonacci(6, _)).
case(fibonacci(91, _)).
case(fibonacci(283, _)).
case(fibonacci(3674, _)).

test :-
    case(A),
    A,
    write('[] :scryer-result "'),
    write(A),
    write('".'),
    nl,
    fail.
test :-
    halt.
