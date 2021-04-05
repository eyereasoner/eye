% See https://en.wikipedia.org/wiki/Fibonacci_number

fib(A, B) :-
    fib(A, 1, 1, B).

fib(0, _, A, A).
fib(1, _, A, A).
fib(A, B, C, D) :-
    A > 1,
    E is A-1,
    F is B+C,
    fib(E, C, F, D).

% test cases
case(fib(0, _)).
case(fib(1, _)).
case(fib(13, _)).
case(fib(65, _)).

test :-
    case(A),
    A,
    writeln(A),
    fail.
test :-
    halt.
