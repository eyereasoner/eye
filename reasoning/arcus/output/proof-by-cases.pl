:- op(1200, xfx, :+).

answer('urn:example:is'('urn:example:water', 'urn:example:observable')).

step(('urn:example:allPossibleCases'([A], ['urn:example:is'(A, 'urn:example:solid'), 'urn:example:is'(A, 'urn:example:liquid'), 'urn:example:is'(A, 'urn:example:gas')]):+'urn:example:InorganicCompound'(A)), 'urn:example:InorganicCompound'('urn:example:water'), 'urn:example:allPossibleCases'(['urn:example:water'], ['urn:example:is'('urn:example:water', 'urn:example:solid'), 'urn:example:is'('urn:example:water', 'urn:example:liquid'), 'urn:example:is'('urn:example:water', 'urn:example:gas')])).
step((true:+'urn:example:is'(_, _)), 'urn:example:is'('urn:example:water', 'urn:example:observable'), true).
