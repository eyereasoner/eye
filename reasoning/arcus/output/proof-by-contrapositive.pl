:- op(1200, xfx, :+).

answer((false:+'urn:example:is'('urn:example:ground', 'urn:example:wet'))).
answer((false:+'urn:example:is'('urn:example:it', 'urn:example:raining'))).

step(((false:+A):+(B:+A), (false:+B)), (('urn:example:is'('urn:example:ground', 'urn:example:wet'):+'urn:example:is'('urn:example:it', 'urn:example:raining')), (false:+'urn:example:is'('urn:example:ground', 'urn:example:wet'))), (false:+'urn:example:is'('urn:example:it', 'urn:example:raining'))).
step((true:+(false:+_)), (false:+'urn:example:is'('urn:example:ground', 'urn:example:wet')), true).
step((true:+(false:+_)), (false:+'urn:example:is'('urn:example:it', 'urn:example:raining')), true).
