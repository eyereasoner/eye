% Proof by contradiction
% See https://en.wikipedia.org/wiki/Proof_by_contradiction

:- op(1200, xfx, :+).

% facts
'urn:example:Human'('urn:example:Socrates').

% all humans are mortal
'urn:example:Mortal'(X) :+
    'urn:example:Human'(X).

% assert the negation of the query
false :+ 'urn:example:Mortal'(_).
