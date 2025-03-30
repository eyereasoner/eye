:- op(1200, xfx, :+).

answer('urn:example:plan'([go(loc3), push(loc1), climb_on, grab])).
answer('urn:example:plan'([go(loc1), go(loc3), push(loc1), climb_on, grab])).
answer('urn:example:plan'([go(loc3), push(loc1), climb_on, grab, climb_off])).
answer('urn:example:plan'([go(loc3), push(loc2), push(loc1), climb_on, grab])).

step((true:+'urn:example:plan'([_, _, _, _])), 'urn:example:plan'([go(loc3), push(loc1), climb_on, grab]), true).
step((true:+'urn:example:plan'([_, _, _, _, _])), 'urn:example:plan'([go(loc1), go(loc3), push(loc1), climb_on, grab]), true).
step((true:+'urn:example:plan'([_, _, _, _, _])), 'urn:example:plan'([go(loc3), push(loc1), climb_on, grab, climb_off]), true).
step((true:+'urn:example:plan'([_, _, _, _, _])), 'urn:example:plan'([go(loc3), push(loc2), push(loc1), climb_on, grab]), true).
