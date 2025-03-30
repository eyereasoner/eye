% Proof by cases
% See https://en.wikipedia.org/wiki/Disjunction_elimination

:- op(1200, xfx, :+).

:- dynamic('urn:example:allPossibleCases'/2).

% water is an inorganic compound
'urn:example:InorganicCompound'('urn:example:water').

% proof by cases
'urn:example:is'(A, 'urn:example:observable') :-
    'urn:example:allPossibleCases'([A], B),
    \+ (member('urn:example:is'(A, C), B),
        \+ ('urn:example:is'(A, 'urn:example:observable') :+ 'urn:example:is'(A, C))
    ).

% water is solid or liquid or gas
'urn:example:allPossibleCases'([A],
        [
            'urn:example:is'(A, 'urn:example:solid'),
            'urn:example:is'(A, 'urn:example:liquid'),
            'urn:example:is'(A, 'urn:example:gas')
        ]
    ) :+ 'urn:example:InorganicCompound'(A).

% solid, liquid and gas things are observable
'urn:example:is'(A, 'urn:example:observable') :+
    'urn:example:is'(A, 'urn:example:solid').

'urn:example:is'(A, 'urn:example:observable') :+
    'urn:example:is'(A, 'urn:example:liquid').

'urn:example:is'(A, 'urn:example:observable') :+
    'urn:example:is'(A, 'urn:example:gas').

% query
true :+ 'urn:example:is'(_, _).
