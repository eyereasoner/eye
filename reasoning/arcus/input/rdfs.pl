% RDF Schema
% See https://www.w3.org/TR/rdf-schema/

:- op(1200, xfx, :+).

:- discontiguous((:+)/2).

% rdfs:domain
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2000/01/rdf-schema#domain'(C, B),
    call(C, A, _).

% rdfs:range
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2000/01/rdf-schema#range'(C, B),
    call(C, _, A).

% rdfs:subClassOf
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B) :+
    'http://www.w3.org/2000/01/rdf-schema#subClassOf'(C, B),
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, C).

'http://www.w3.org/2000/01/rdf-schema#subClassOf'(A, B) :+
    'http://www.w3.org/2000/01/rdf-schema#subClassOf'(A, C),
    'http://www.w3.org/2000/01/rdf-schema#subClassOf'(C, B).

'http://www.w3.org/2000/01/rdf-schema#domain'(A, B) :+
    'http://www.w3.org/2000/01/rdf-schema#subClassOf'(C, B),
    'http://www.w3.org/2000/01/rdf-schema#domain'(A, C).

'http://www.w3.org/2000/01/rdf-schema#range'(A, B) :+
    'http://www.w3.org/2000/01/rdf-schema#subClassOf'(C, B),
    'http://www.w3.org/2000/01/rdf-schema#range'(A, C).

% rdfs:subPropertyOf
Z :+
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(D, A),
    call(D, B, C),
    Z =.. [A, B, C].

'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(A, B) :+
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(A, C),
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(C, B).

'http://www.w3.org/2000/01/rdf-schema#domain'(A, B) :+
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(A, C),
    'http://www.w3.org/2000/01/rdf-schema#domain'(C, B).

'http://www.w3.org/2000/01/rdf-schema#range'(A, B) :+
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(A, C),
    'http://www.w3.org/2000/01/rdf-schema#range'(C, B).

% test data
'http://www.w3.org/2000/01/rdf-schema#domain'('urn:example:p', 'C').
'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'('urn:example:p', 'urn:example:q').
'urn:example:p'('urn:example:s', 'urn:example:o').

% query
true :+ 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, _).
true :+ 'urn:example:q'(_, _).
