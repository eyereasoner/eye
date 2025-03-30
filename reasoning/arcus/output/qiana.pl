:- op(1200, xfx, :+).

answer('urn:example:believes'('urn:example:Fabian', ('urn:example:notNecessarilyA'(A, 'urn:example:gold'):+'urn:example:glitter'(A)))).
answer('urn:example:notNecessarilyA'('urn:example:northStar', 'urn:example:gold')).

step((A:+'urn:example:says'('urn:example:Einstein', A)), 'urn:example:says'('urn:example:Einstein', ('urn:example:notNecessarilyA'(B, 'urn:example:gold'):+'urn:example:glitter'(B))), ('urn:example:notNecessarilyA'(B, 'urn:example:gold'):+'urn:example:glitter'(B))).
step((true:+'urn:example:believes'('urn:example:Fabian', _)), 'urn:example:believes'('urn:example:Fabian', ('urn:example:notNecessarilyA'(A, 'urn:example:gold'):+'urn:example:glitter'(A))), true).
step(('urn:example:notNecessarilyA'(A, 'urn:example:gold'):+'urn:example:glitter'(A)), 'urn:example:glitter'('urn:example:northStar'), 'urn:example:notNecessarilyA'('urn:example:northStar', 'urn:example:gold')).
step((true:+'urn:example:notNecessarilyA'(_, _)), 'urn:example:notNecessarilyA'('urn:example:northStar', 'urn:example:gold'), true).
