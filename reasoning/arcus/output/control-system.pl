:- op(1200, xfx, :+).

answer('urn:example:control1'('urn:example:actuator1', 39.27346198678276)).
answer('urn:example:control1'('urn:example:actuator2', 26.08)).

step((true:+'urn:example:control1'(_, _)), 'urn:example:control1'('urn:example:actuator1', 39.27346198678276), true).
step((true:+'urn:example:control1'(_, _)), 'urn:example:control1'('urn:example:actuator2', 26.08), true).
