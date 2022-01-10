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

% query
query(fibonacci(1,_ANSWER)).
query(fibonacci(2,_ANSWER)).
query(fibonacci(3,_ANSWER)).
query(fibonacci(4,_ANSWER)).
query(fibonacci(5,_ANSWER)).
query(fibonacci(6,_ANSWER)).
query(fibonacci(7,_ANSWER)).
query(fibonacci(8,_ANSWER)).
query(fibonacci(9,_ANSWER)).
query(fibonacci(10,_ANSWER)).
query(fibonacci(11,_ANSWER)).
query(fibonacci(12,_ANSWER)).
query(fibonacci(13,_ANSWER)).
query(fibonacci(14,_ANSWER)).
query(fibonacci(15,_ANSWER)).
query(fibonacci(16,_ANSWER)).
query(fibonacci(17,_ANSWER)).
query(fibonacci(18,_ANSWER)).
query(fibonacci(19,_ANSWER)).
query(fibonacci(20,_ANSWER)).
query(fibonacci(91,_ANSWER)).
query(fibonacci(283,_ANSWER)).
query(fibonacci(3674,_ANSWER)).
query(golden_ratio(1,_ANSWER)).
query(golden_ratio(10,_ANSWER)).
query(golden_ratio(100,_ANSWER)).
query(golden_ratio(1000,_ANSWER)).
