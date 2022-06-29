% Traversing graph paths

:- dynamic('https://josd.github.io/eye/ns#oneway'/2).
:- dynamic('https://josd.github.io/eye/ns#path'/2).

true =>
    'https://josd.github.io/eye/ns#oneway'('http://example.org/ns#paris','http://example.org/ns#orleans'),
    'https://josd.github.io/eye/ns#oneway'('http://example.org/ns#paris','http://example.org/ns#chartres'),
    'https://josd.github.io/eye/ns#oneway'('http://example.org/ns#paris','http://example.org/ns#amiens'),
    'https://josd.github.io/eye/ns#oneway'('http://example.org/ns#orleans','http://example.org/ns#blois'),
    'https://josd.github.io/eye/ns#oneway'('http://example.org/ns#orleans','http://example.org/ns#bourges'),
    'https://josd.github.io/eye/ns#oneway'('http://example.org/ns#blois','http://example.org/ns#tours'),
    'https://josd.github.io/eye/ns#oneway'('http://example.org/ns#chartres','http://example.org/ns#lemans'),
    'https://josd.github.io/eye/ns#oneway'('http://example.org/ns#lemans','http://example.org/ns#angers'),
    'https://josd.github.io/eye/ns#oneway'('http://example.org/ns#lemans','http://example.org/ns#tours'),
    'https://josd.github.io/eye/ns#oneway'('http://example.org/ns#angers','http://example.org/ns#nantes').

'https://josd.github.io/eye/ns#oneway'(A,B) => 'https://josd.github.io/eye/ns#path'(A,B).
'https://josd.github.io/eye/ns#path'(A,B),'https://josd.github.io/eye/ns#path'(B,C) => 'https://josd.github.io/eye/ns#path'(A,C).

% query
'https://josd.github.io/eye/ns#path'(_CITY,'http://example.org/ns#nantes') => true.
