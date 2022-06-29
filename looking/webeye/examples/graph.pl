% Traversing graph paths

:- dynamic('https://idlabresearch.github.io/ns#oneway'/2).
:- dynamic('https://idlabresearch.github.io/ns#path'/2).

true =>
    'https://idlabresearch.github.io/ns#oneway'('http://example.org/ns#paris','http://example.org/ns#orleans'),
    'https://idlabresearch.github.io/ns#oneway'('http://example.org/ns#paris','http://example.org/ns#chartres'),
    'https://idlabresearch.github.io/ns#oneway'('http://example.org/ns#paris','http://example.org/ns#amiens'),
    'https://idlabresearch.github.io/ns#oneway'('http://example.org/ns#orleans','http://example.org/ns#blois'),
    'https://idlabresearch.github.io/ns#oneway'('http://example.org/ns#orleans','http://example.org/ns#bourges'),
    'https://idlabresearch.github.io/ns#oneway'('http://example.org/ns#blois','http://example.org/ns#tours'),
    'https://idlabresearch.github.io/ns#oneway'('http://example.org/ns#chartres','http://example.org/ns#lemans'),
    'https://idlabresearch.github.io/ns#oneway'('http://example.org/ns#lemans','http://example.org/ns#angers'),
    'https://idlabresearch.github.io/ns#oneway'('http://example.org/ns#lemans','http://example.org/ns#tours'),
    'https://idlabresearch.github.io/ns#oneway'('http://example.org/ns#angers','http://example.org/ns#nantes').

'https://idlabresearch.github.io/ns#oneway'(A,B) => 'https://idlabresearch.github.io/ns#path'(A,B).
'https://idlabresearch.github.io/ns#path'(A,B),'https://idlabresearch.github.io/ns#path'(B,C) => 'https://idlabresearch.github.io/ns#path'(A,C).

% query
'https://idlabresearch.github.io/ns#path'(_CITY,'http://example.org/ns#nantes') => true.
