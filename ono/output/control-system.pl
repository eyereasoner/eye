:- op(1200, xfx, :+).

answer('urn:example:control1'('urn:example:actuator1',39.2734619867828)).
answer('urn:example:control1'('urn:example:actuator2',26.08)).

step((true:+'urn:example:control1'(A,B)),'urn:example:control1'('urn:example:actuator1',39.2734619867828),true).
step((true:+'urn:example:control1'(A,B)),'urn:example:control1'('urn:example:actuator2',26.08),true).
