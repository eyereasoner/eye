% Traversing graph paths

:- op(1200, xfx, :+).

'urn:example:oneway'('urn:example:paris', 'urn:example:orleans').
'urn:example:oneway'('urn:example:paris', 'urn:example:chartres').
'urn:example:oneway'('urn:example:paris', 'urn:example:amiens').
'urn:example:oneway'('urn:example:orleans', 'urn:example:blois').
'urn:example:oneway'('urn:example:orleans', 'urn:example:bourges').
'urn:example:oneway'('urn:example:blois', 'urn:example:tours').
'urn:example:oneway'('urn:example:chartres', 'urn:example:lemans').
'urn:example:oneway'('urn:example:lemans', 'urn:example:angers').
'urn:example:oneway'('urn:example:lemans', 'urn:example:tours').
'urn:example:oneway'('urn:example:angers', 'urn:example:nantes').

'urn:example:path'([A, B], go(A, B, goal)) :-
    'urn:example:oneway'(A, B).
'urn:example:path'([A, C], go(A, B, D)) :-
    'urn:example:oneway'(A, B),
    'urn:example:path'([B, C], D).

% query
true :+ 'urn:example:path'([_, 'urn:example:nantes'], _).
