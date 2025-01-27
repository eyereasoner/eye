:- op(1200, xfx, :+).

answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Human>')).
answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Mortal>')).

% proof steps
step(('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, B):+'<http://www.w3.org/2000/01/rdf-schema#subClassOf>'(C, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, C)), ('<http://www.w3.org/2000/01/rdf-schema#subClassOf>'('<urn:example:Human>', '<urn:example:Mortal>'), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Human>')), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Mortal>')).
step((true:+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, _)), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Human>'), true).
step((true:+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, _)), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Mortal>'), true).
