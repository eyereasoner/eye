:- op(1200, xfx, :+).

answer('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:s', 'C')).
answer('urn:example:q'('urn:example:s', 'urn:example:o')).

step(('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B):+'http://www.w3.org/2000/01/rdf-schema#domain'(C, B), call(C, A, _)), ('http://www.w3.org/2000/01/rdf-schema#domain'('urn:example:p', 'C'), call('urn:example:p', 'urn:example:s', 'urn:example:o')), 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:s', 'C')).
step((A:+'http://www.w3.org/2000/01/rdf-schema#subPropertyOf'(B, C), call(B, D, E), A=..[C, D, E]), ('http://www.w3.org/2000/01/rdf-schema#subPropertyOf'('urn:example:p', 'urn:example:q'), call('urn:example:p', 'urn:example:s', 'urn:example:o'), 'urn:example:q'('urn:example:s', 'urn:example:o')=..['urn:example:q', 'urn:example:s', 'urn:example:o']), 'urn:example:q'('urn:example:s', 'urn:example:o')).
step((true:+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, _)), 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:s', 'C'), true).
step((true:+'urn:example:q'(_, _)), 'urn:example:q'('urn:example:s', 'urn:example:o'), true).
