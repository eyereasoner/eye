:- op(1200, xfx, :+).

answer('urn:example:dijkstra'([a, f], [[a, c, b, d, e, f], 13])).
answer('urn:example:dijkstra'([a, f], [[a, c, b, d, f], 14])).
answer('urn:example:dijkstra'([a, f], [[a, b, d, e, f], 14])).
answer('urn:example:dijkstra'([a, f], [[a, b, d, f], 15])).
answer('urn:example:dijkstra'([a, f], [[a, c, e, f], 15])).
answer('urn:example:dijkstra'([a, f], [[a, c, d, e, f], 15])).
answer('urn:example:dijkstra'([a, f], [[a, c, d, f], 16])).

step((edge([A, B], C):+edge([B, A], C)), edge([a, b], 4), edge([b, a], 4)).
step((edge([A, B], C):+edge([B, A], C)), edge([a, c], 2), edge([c, a], 2)).
step((edge([A, B], C):+edge([B, A], C)), edge([b, c], 1), edge([c, b], 1)).
step((edge([A, B], C):+edge([B, A], C)), edge([b, d], 5), edge([d, b], 5)).
step((edge([A, B], C):+edge([B, A], C)), edge([c, d], 8), edge([d, c], 8)).
step((edge([A, B], C):+edge([B, A], C)), edge([c, e], 10), edge([e, c], 10)).
step((edge([A, B], C):+edge([B, A], C)), edge([d, e], 2), edge([e, d], 2)).
step((edge([A, B], C):+edge([B, A], C)), edge([d, f], 6), edge([f, d], 6)).
step((edge([A, B], C):+edge([B, A], C)), edge([e, f], 3), edge([f, e], 3)).
step((true:+'urn:example:dijkstra'([a, f], [_, _])), 'urn:example:dijkstra'([a, f], [[a, c, b, d, e, f], 13]), true).
step((true:+'urn:example:dijkstra'([a, f], [_, _])), 'urn:example:dijkstra'([a, f], [[a, c, b, d, f], 14]), true).
step((true:+'urn:example:dijkstra'([a, f], [_, _])), 'urn:example:dijkstra'([a, f], [[a, b, d, e, f], 14]), true).
step((true:+'urn:example:dijkstra'([a, f], [_, _])), 'urn:example:dijkstra'([a, f], [[a, b, d, f], 15]), true).
step((true:+'urn:example:dijkstra'([a, f], [_, _])), 'urn:example:dijkstra'([a, f], [[a, c, e, f], 15]), true).
step((true:+'urn:example:dijkstra'([a, f], [_, _])), 'urn:example:dijkstra'([a, f], [[a, c, d, e, f], 15]), true).
step((true:+'urn:example:dijkstra'([a, f], [_, _])), 'urn:example:dijkstra'([a, f], [[a, c, d, f], 16]), true).
