% Calculating the area of a polygon

'https://josd.github.io/roundel#area'([_],0).
'https://josd.github.io/roundel#area'([[A,B],[C,D]|E],F) :-
    'https://josd.github.io/roundel#area'([[C,D]|E],G),
    F is (A*D-B*C)/2+G.

% query
query('https://josd.github.io/roundel#area'([[3,2],[6,2],[7,6],[4,6],[5,5],[5,3],[3,2]],_ANSWER)).

run :-
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
