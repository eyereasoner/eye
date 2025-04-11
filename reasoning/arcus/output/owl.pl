:- op(1200, xfx, :+).

answer('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:large', 'Size')).
answer('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:medium', 'Size')).
answer('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:small', 'Size')).
answer('urn:example:q'('urn:example:o', 'urn:example:s')).

step((A:+'http://www.w3.org/2002/07/owl#inverseOf'(B, C), call(B, D, E), A=..[C, E, D]), ('http://www.w3.org/2002/07/owl#inverseOf'('urn:example:p', 'urn:example:q'), call('urn:example:p', 'urn:example:s', 'urn:example:o'), 'urn:example:q'('urn:example:o', 'urn:example:s')=..['urn:example:q', 'urn:example:o', 'urn:example:s']), 'urn:example:q'('urn:example:o', 'urn:example:s')).
step(('http://www.w3.org/2002/07/owl#inverseOf'(A, B):+'http://www.w3.org/2002/07/owl#inverseOf'(B, A)), 'http://www.w3.org/2002/07/owl#inverseOf'('urn:example:p', 'urn:example:q'), 'http://www.w3.org/2002/07/owl#inverseOf'('urn:example:q', 'urn:example:p')).
step(('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B):+'http://www.w3.org/2002/07/owl#oneOf'(B, C), member(A, C)), ('http://www.w3.org/2002/07/owl#oneOf'('Size', ['urn:example:large', 'urn:example:medium', 'urn:example:small']), member('urn:example:large', ['urn:example:large', 'urn:example:medium', 'urn:example:small'])), 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:large', 'Size')).
step(('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B):+'http://www.w3.org/2002/07/owl#oneOf'(B, C), member(A, C)), ('http://www.w3.org/2002/07/owl#oneOf'('Size', ['urn:example:large', 'urn:example:medium', 'urn:example:small']), member('urn:example:medium', ['urn:example:large', 'urn:example:medium', 'urn:example:small'])), 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:medium', 'Size')).
step(('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A, B):+'http://www.w3.org/2002/07/owl#oneOf'(B, C), member(A, C)), ('http://www.w3.org/2002/07/owl#oneOf'('Size', ['urn:example:large', 'urn:example:medium', 'urn:example:small']), member('urn:example:small', ['urn:example:large', 'urn:example:medium', 'urn:example:small'])), 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:small', 'Size')).
step((true:+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, _)), 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:large', 'Size'), true).
step((true:+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, _)), 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:medium', 'Size'), true).
step((true:+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, _)), 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:small', 'Size'), true).
step((true:+'urn:example:q'(_, _)), 'urn:example:q'('urn:example:o', 'urn:example:s'), true).
