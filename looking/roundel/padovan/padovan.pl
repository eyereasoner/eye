% See https://en.wikipedia.org/wiki/Padovan_sequence

'https://josd.github.io/eye/ns#padovan'(A,B) :-
    padovan(A,0,1,1,B).

padovan(0,A,_,_,A).
padovan(1,_,A,_,A).
padovan(2,_,_,A,A).
padovan(A,B,C,D,E) :-
    A > 2,
    F is A-1,
    G is B+C,
    padovan(F,C,D,G,E).

'https://josd.github.io/eye/ns#plastic_ratio'(A,B) :-
    'https://josd.github.io/eye/ns#padovan'(A,C),
    D is A+1,
    'https://josd.github.io/eye/ns#padovan'(D,E),
    B is E/C.

% query
query('https://josd.github.io/eye/ns#padovan'(1,_ANSWER)).
query('https://josd.github.io/eye/ns#padovan'(2,_ANSWER)).
query('https://josd.github.io/eye/ns#padovan'(3,_ANSWER)).
query('https://josd.github.io/eye/ns#padovan'(4,_ANSWER)).
query('https://josd.github.io/eye/ns#padovan'(5,_ANSWER)).
query('https://josd.github.io/eye/ns#padovan'(91,_ANSWER)).
query('https://josd.github.io/eye/ns#padovan'(283,_ANSWER)).
query('https://josd.github.io/eye/ns#padovan'(3674,_ANSWER)).
query('https://josd.github.io/eye/ns#plastic_ratio'(1,_ANSWER)).
query('https://josd.github.io/eye/ns#plastic_ratio'(10,_ANSWER)).
query('https://josd.github.io/eye/ns#plastic_ratio'(100,_ANSWER)).
query('https://josd.github.io/eye/ns#plastic_ratio'(1000,_ANSWER)).

run :-
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
