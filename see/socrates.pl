% Socrates is a mortal

'urn:example:Man'('urn:example:Socrates').

'urn:example:Mortal'(X) :-
    'urn:example:Man'(X).

% query
query('urn:example:Mortal'(_IND)).

test :-
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
