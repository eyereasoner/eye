% See https://en.wikipedia.org/wiki/Fibonacci_number

'https://eyereasoner.github.io/see#fibonacci'(A,B) :-
    fibonacci(A,0,1,B).

fibonacci(0,A,_,A).
fibonacci(1,_,A,A).
fibonacci(A,B,C,D) :-
    A > 1,
    E is A-1,
    F is B+C,
    fibonacci(E,C,F,D).

'https://eyereasoner.github.io/see#golden_ratio'(A,B) :-
    'https://eyereasoner.github.io/see#fibonacci'(A,C),
    D is A+1,
    'https://eyereasoner.github.io/see#fibonacci'(D,E),
    B is E/C.

% query
query('https://eyereasoner.github.io/see#fibonacci'(1,_ANSWER)).
query('https://eyereasoner.github.io/see#fibonacci'(2,_ANSWER)).
query('https://eyereasoner.github.io/see#fibonacci'(3,_ANSWER)).
query('https://eyereasoner.github.io/see#fibonacci'(4,_ANSWER)).
query('https://eyereasoner.github.io/see#fibonacci'(5,_ANSWER)).
query('https://eyereasoner.github.io/see#fibonacci'(91,_ANSWER)).
query('https://eyereasoner.github.io/see#fibonacci'(283,_ANSWER)).
query('https://eyereasoner.github.io/see#fibonacci'(3674,_ANSWER)).
query('https://eyereasoner.github.io/see#golden_ratio'(1,_ANSWER)).
query('https://eyereasoner.github.io/see#golden_ratio'(10,_ANSWER)).
query('https://eyereasoner.github.io/see#golden_ratio'(100,_ANSWER)).
query('https://eyereasoner.github.io/see#golden_ratio'(1000,_ANSWER)).

test :-
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
