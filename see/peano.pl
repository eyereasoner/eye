% Peano arithmetic
% See https://en.wikipedia.org/wiki/Peano_axioms

% add
'https://eyereasoner.github.io/see#add'(A,0,A).
'https://eyereasoner.github.io/see#add'(A,s(B),s(C)) :-
    'https://eyereasoner.github.io/see#add'(A,B,C).

% multiply
'https://eyereasoner.github.io/see#multiply'(_,0,0).
'https://eyereasoner.github.io/see#multiply'(A,s(B),C) :-
    'https://eyereasoner.github.io/see#multiply'(A,B,D),
    'https://eyereasoner.github.io/see#add'(A,D,C).

% factorial
'https://eyereasoner.github.io/see#factorial'(A,B) :-
    fac(A,s(0),B).

fac(0,A,A).
fac(s(A),B,C) :-
    'https://eyereasoner.github.io/see#multiply'(B,s(A),D),
    fac(A,D,C).

% query
query(
    (
        'https://eyereasoner.github.io/see#multiply'(s(0),s(s(0)),A),
        'https://eyereasoner.github.io/see#add'(A,s(s(s(0))),B),
        'https://eyereasoner.github.io/see#factorial'(B,_C)
    )
).

test :-
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
