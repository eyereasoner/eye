:- op(1200, xfx, :+).

answer('urn:example:isReachable'(a, f)).
answer(\+'urn:example:isReachable'(b, e)).
answer('urn:example:isReachable'(c, g)).

step((true:+'urn:example:isReachable'(a, f)), 'urn:example:isReachable'(a, f), true).
step((true:+ \+'urn:example:isReachable'(b, e)), \+'urn:example:isReachable'(b, e), true).
step((true:+'urn:example:isReachable'(c, g)), 'urn:example:isReachable'(c, g), true).
