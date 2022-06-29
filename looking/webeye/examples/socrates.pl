% Socrates is a mortal

:- dynamic('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'/2).

true => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('http://example.org/ns#Socrates','http://example.org/ns#Man').

'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X,'http://example.org/ns#Man') => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X,'http://example.org/ns#Mortal').

% query
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_IND,_CLASS) => true.
