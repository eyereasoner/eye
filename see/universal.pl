% Examples of universal statements

% Every x: type(x,Resource)
'http://www.w3.org/2000/01/rdf-schema#Resource'(_).

% Everybody loves somebody who is lonely
'urn:example:loves'(A,skolem(A)).
'urn:example:is'(skolem(_),'urn:example:lonely').

% query
query('http://www.w3.org/2000/01/rdf-schema#Resource'('urn:example:Pat')).
query(
    (
        'urn:example:loves'('urn:example:Bob',X),
        'urn:example:is'(X,'urn:example:lonely')
    )
).

test :-
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
