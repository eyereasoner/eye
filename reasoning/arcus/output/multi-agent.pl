:- op(1200, xfx, :+).

answer('urn:example:obligatory'('urn:example:complete:task'('urn:example:agent2', 'urn:example:task1'))).
answer('urn:example:obligatory'('urn:example:escalate:task'('urn:example:agent1', 'urn:example:task1'))).
answer('urn:example:permitted'('urn:example:execute:task'('urn:example:agent2', 'urn:example:task1'))).
answer('urn:example:violation'('urn:example:task1')).
answer('urn:example:sanction'('urn:example:agent2')).

step((true:+'urn:example:obligatory'(_)), 'urn:example:obligatory'('urn:example:complete:task'('urn:example:agent2', 'urn:example:task1')), true).
step((true:+'urn:example:obligatory'(_)), 'urn:example:obligatory'('urn:example:escalate:task'('urn:example:agent1', 'urn:example:task1')), true).
step((true:+'urn:example:permitted'(_)), 'urn:example:permitted'('urn:example:execute:task'('urn:example:agent2', 'urn:example:task1')), true).
step((true:+'urn:example:violation'(_)), 'urn:example:violation'('urn:example:task1'), true).
step((true:+'urn:example:sanction'(_)), 'urn:example:sanction'('urn:example:agent2'), true).
