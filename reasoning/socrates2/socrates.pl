% Socrates inference

% facts
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Human>').
'<http://www.w3.org/2000/01/rdf-schema#subClassOf>'('<urn:example:Human>', '<urn:example:Mortal>').

% subclass rule
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, B) :+
    '<http://www.w3.org/2000/01/rdf-schema#subClassOf>'(C, B),
    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, C).

% query
true :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, _).
