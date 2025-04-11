:- op(1200, xfx, :+).

answer(('urn:example:is'(_, 'urn:example:good'('urn:example:Cobbler')):+true)).

step(('urn:example:is'(_, 'urn:example:good'('urn:example:Cobbler')):+true), true, 'urn:example:is'(sk_0, 'urn:example:good'('urn:example:Cobbler'))).
step((true:+('urn:example:is'(_, 'urn:example:good'(_)):+true)), ('urn:example:is'(_, 'urn:example:good'('urn:example:Cobbler')):+true), true).
