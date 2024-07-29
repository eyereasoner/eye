% Traversing graph paths

'https://eyereasoner.github.io/see#oneway'('urn:example:paris','urn:example:orleans').
'https://eyereasoner.github.io/see#oneway'('urn:example:paris','urn:example:chartres').
'https://eyereasoner.github.io/see#oneway'('urn:example:paris','urn:example:amiens').
'https://eyereasoner.github.io/see#oneway'('urn:example:orleans','urn:example:blois').
'https://eyereasoner.github.io/see#oneway'('urn:example:orleans','urn:example:bourges').
'https://eyereasoner.github.io/see#oneway'('urn:example:blois','urn:example:tours').
'https://eyereasoner.github.io/see#oneway'('urn:example:chartres','urn:example:lemans').
'https://eyereasoner.github.io/see#oneway'('urn:example:lemans','urn:example:angers').
'https://eyereasoner.github.io/see#oneway'('urn:example:lemans','urn:example:tours').
'https://eyereasoner.github.io/see#oneway'('urn:example:angers','urn:example:nantes').

'https://eyereasoner.github.io/see#path'(A,B) :-
    'https://eyereasoner.github.io/see#oneway'(A,B).
'https://eyereasoner.github.io/see#path'(A,C) :-
    'https://eyereasoner.github.io/see#oneway'(A,B),
    'https://eyereasoner.github.io/see#path'(B,C).

% query
query('https://eyereasoner.github.io/see#path'(_City,'urn:example:nantes')).

test :-
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
