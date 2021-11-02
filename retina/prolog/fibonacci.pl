% See https://en.wikipedia.org/wiki/Fibonacci_number

fibonacci(A,B) :-
    fibonacci(A,0,1,B).

fibonacci(0,A,_,A).
fibonacci(1,_,A,A).
fibonacci(A,B,C,D) :-
    A > 1,
    E is A-1,
    F is B+C,
    fibonacci(E,C,F,D).

golden_ratio(A,B) :-
    fibonacci(A,C),
    D is A+1,
    fibonacci(D,E),
    B is E/C.

% query implies goal
fibonacci(1,_ANSWER) -: goal.
fibonacci(2,_ANSWER) -: goal.
fibonacci(3,_ANSWER) -: goal.
fibonacci(4,_ANSWER) -: goal.
fibonacci(5,_ANSWER) -: goal.
fibonacci(6,_ANSWER) -: goal.
fibonacci(7,_ANSWER) -: goal.
fibonacci(8,_ANSWER) -: goal.
fibonacci(9,_ANSWER) -: goal.
fibonacci(10,_ANSWER) -: goal.
fibonacci(11,_ANSWER) -: goal.
fibonacci(12,_ANSWER) -: goal.
fibonacci(13,_ANSWER) -: goal.
fibonacci(14,_ANSWER) -: goal.
fibonacci(15,_ANSWER) -: goal.
fibonacci(16,_ANSWER) -: goal.
fibonacci(17,_ANSWER) -: goal.
fibonacci(18,_ANSWER) -: goal.
fibonacci(19,_ANSWER) -: goal.
fibonacci(20,_ANSWER) -: goal.
fibonacci(91,_ANSWER) -: goal.
fibonacci(283,_ANSWER) -: goal.
fibonacci(3674,_ANSWER) -: goal.
golden_ratio(1,_ANSWER) -: goal.
golden_ratio(10,_ANSWER) -: goal.
golden_ratio(100,_ANSWER) -: goal.
golden_ratio(1000,_ANSWER) -: goal.
