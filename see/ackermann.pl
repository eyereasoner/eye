% Ackermann function
% See https://en.wikipedia.org/wiki/Ackermann_function

% ackermann(x,y,z)
'https://eyereasoner.github.io/see#ackermann'(A,B,C) :-
    D is B+3,
    ackermann(A,D,2,E),
    C is E-3.

% succ (x=0)
ackermann(0,A,_,B) :-
    !,
    B is A+1.

% sum (x=1)
ackermann(1,A,B,C) :-
    !,
    C is A+B.

% product (x=2)
ackermann(2,A,B,C) :-
    !,
    C is A*B.

% exponentiation (x=3),tetration (x=4),pentation (x=5),hexation (x=6),etc
ackermann(_,0,_,1) :-
    !.
ackermann(A,B,C,D) :-
    E is B-1,
    ackermann(A,E,C,F),
    G is A-1,
    ackermann(G,F,C,D).

% query
query('https://eyereasoner.github.io/see#ackermann'(0,6,_ANSWER)).
query('https://eyereasoner.github.io/see#ackermann'(1,2,_ANSWER)).
query('https://eyereasoner.github.io/see#ackermann'(1,7,_ANSWER)).
query('https://eyereasoner.github.io/see#ackermann'(2,2,_ANSWER)).
query('https://eyereasoner.github.io/see#ackermann'(2,9,_ANSWER)).
query('https://eyereasoner.github.io/see#ackermann'(3,4,_ANSWER)).
query('https://eyereasoner.github.io/see#ackermann'(3,14,_ANSWER)).
query('https://eyereasoner.github.io/see#ackermann'(4,0,_ANSWER)).
query('https://eyereasoner.github.io/see#ackermann'(4,1,_ANSWER)).
query('https://eyereasoner.github.io/see#ackermann'(4,2,_ANSWER)).
query('https://eyereasoner.github.io/see#ackermann'(5,0,_ANSWER)).

test :-
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
