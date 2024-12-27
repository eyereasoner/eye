:- op(1200, xfx, :=).

answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Man>')).
answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Mortal>')).

%
% Proof steps
%

step(('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<urn:example:Mortal>'):='<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<urn:example:Man>')), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Man>'), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Socrates>', '<urn:example:Mortal>')).
