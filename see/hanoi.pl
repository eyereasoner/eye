% Towers of Hanoi

'https://eyereasoner.github.io/see#move'(0,_,_,_) :-
    !.
'https://eyereasoner.github.io/see#move'(N,A,B,C) :-
    M is N-1,
    'https://eyereasoner.github.io/see#move'(M,A,C,B),
    'https://eyereasoner.github.io/see#move'(M,C,B,A).

% query
query('https://eyereasoner.github.io/see#move'(14,left,centre,right)).

test :-
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
