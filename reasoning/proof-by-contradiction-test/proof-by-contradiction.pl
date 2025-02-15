% Proof by contradiction

:- op(1200, xfx, :+).

:- dynamic(type/2).

% context
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, B) :- type(A, B).

% facts
type('<https://eyereasoner.github.io/ns#Socrates>', '<https://eyereasoner.github.io/ns#Human>').

% all humans are mortal
type(X, '<https://eyereasoner.github.io/ns#Mortal>') :+
    type(X, '<https://eyereasoner.github.io/ns#Human>').

% assert the negation of the query
false :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, '<https://eyereasoner.github.io/ns#Mortal>').
