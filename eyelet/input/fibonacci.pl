% See https://en.wikipedia.org/wiki/Fibonacci_number

:- op(1200, xfx, :+).

fibonacci(A, B) :-
    fibonacci(A, 0, 1, B).

fibonacci(0, A, _, A).
fibonacci(1, _, A, A).
fibonacci(A, B, C, D) :-
    A > 1,
    E is A-1,
    F is B+C,
    fibonacci(E, C, F, D).

golden_ratio(A, B) :-
    fibonacci(A, C),
    D is A+1,
    fibonacci(D, E),
    B is E/C.

% query
true :+ fibonacci(1, _).
true :+ fibonacci(2, _).
true :+ fibonacci(3, _).
true :+ fibonacci(4, _).
true :+ fibonacci(5, _).
true :+ fibonacci(91, _).
true :+ fibonacci(283, _).
true :+ fibonacci(3674, _).
true :+ golden_ratio(1, _).
true :+ golden_ratio(10, _).
true :+ golden_ratio(100, _).
true :+ golden_ratio(1000, _).
