% Proof by contradiction

:- op(1200, xfx, :+).

:- dynamic(type/2).

% context
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, B) :- type(A, B).

% facts
type('<urn:example:Socrates>', '<urn:example:Human>').

% all humans are mortal
type(X, '<urn:example:Mortal>') :+
    type(X, '<urn:example:Human>').

% assert the negation of the query
false :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, '<urn:example:Mortal>').
