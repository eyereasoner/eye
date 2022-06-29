% Socrates is a mortal

'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('http://example.org/ns#Socrates','http://example.org/ns#Man').

'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X,'http://example.org/ns#Mortal') :-
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X,'http://example.org/ns#Man').

% query
query('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_IND,_CLASS)).

run :-
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
