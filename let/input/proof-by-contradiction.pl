% Proof by contradiction
% See https://en.wikipedia.org/wiki/Proof_by_contradiction

% facts
'Human'('Socrates').

% all humans are mortal
'Mortal'(X) :+
    'Human'(X).

% assert the negation of the query
false :+ 'Mortal'(_).
