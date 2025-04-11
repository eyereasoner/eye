:- op(1200, xfx, :+).

answer('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:z', 'urn:example:N10000')).

step((true:+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, 'urn:example:N10000')), 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:z', 'urn:example:N10000'), true).
