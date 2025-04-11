:- op(1200, xfx, :+).

answer('urn:example:ageAbove'('urn:example:patH', 80)).

step((true:+'urn:example:ageAbove'(_, 80)), 'urn:example:ageAbove'('urn:example:patH', 80), true).
