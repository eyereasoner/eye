% example of rule derivation

:- op(1200, xfx, :+).

% facts
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Minka>', '<urn:example:Cat>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:Charly>', '<urn:example:Dog>').

% rule
(
    '<urn:example:is>'('<urn:example:test>', true) :+
        '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, '<urn:example:Dog>')
) :+
    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, '<urn:example:Cat>').

% query
true :+ '<urn:example:is>'('<urn:example:test>', true).
