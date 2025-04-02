% Socrates is a mortal

'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Man>').

'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(X, '<urn:example:Mortal>') :+
    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(X, '<urn:example:Man>').
