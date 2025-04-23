% Examples of universal statements

:- op(1200, xfx, :+).

% Every x: type(x, Resource)
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, 'http://www.w3.org/2000/01/rdf-schema#Resource').

% Everybody loves somebody who is lonely
'urn:example:loves'(A, 'urn:example:Lonely'(skolem(A))).

% query
true :+ 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:Pat', 'http://www.w3.org/2000/01/rdf-schema#Resource').
true :+ 'urn:example:loves'('urn:example:Bob', _).
