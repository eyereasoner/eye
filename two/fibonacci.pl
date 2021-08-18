% See https://en.wikipedia.org/wiki/Fibonacci_number

nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_fibonacci(A,B) :-
    etc_fibonacci(A,0,1,B).

etc_fibonacci(0,A,_,A).
etc_fibonacci(1,_,A,A).
etc_fibonacci(A,B,C,D) :-
    A > 1,
    E is A-1,
    F is B+C,
    etc_fibonacci(E,C,F,D).

% test cases
case(etc_fibonacci(0,_ANSWER)).
case(etc_fibonacci(1,_ANSWER)).
case(etc_fibonacci(6,_ANSWER)).
case(etc_fibonacci(91,_ANSWER)).
case(etc_fibonacci(283,_ANSWER)).
case(etc_fibonacci(3674,_ANSWER)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
