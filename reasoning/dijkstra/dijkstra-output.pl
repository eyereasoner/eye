:- op(1200, xfx, :+).

answer('<urn:example:dijkstra>'([a, f], [[a, c, b, d, e, f], 13])).
answer('<urn:example:dijkstra>'([a, f], [[a, c, b, d, f], 14])).
answer('<urn:example:dijkstra>'([a, f], [[a, b, d, e, f], 14])).
answer('<urn:example:dijkstra>'([a, f], [[a, b, d, f], 15])).
answer('<urn:example:dijkstra>'([a, f], [[a, c, e, f], 15])).
answer('<urn:example:dijkstra>'([a, f], [[a, c, d, e, f], 15])).
answer('<urn:example:dijkstra>'([a, f], [[a, c, d, f], 16])).

% proof steps
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'([a, b], 4), '<urn:example:edge>'([b, a], 4)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'([a, c], 2), '<urn:example:edge>'([c, a], 2)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'([b, c], 1), '<urn:example:edge>'([c, b], 1)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'([b, d], 5), '<urn:example:edge>'([d, b], 5)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'([c, d], 8), '<urn:example:edge>'([d, c], 8)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'([c, e], 10), '<urn:example:edge>'([e, c], 10)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'([d, e], 2), '<urn:example:edge>'([e, d], 2)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'([d, f], 6), '<urn:example:edge>'([f, d], 6)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'([e, f], 3), '<urn:example:edge>'([f, e], 3)).
step((true:+'<urn:example:dijkstra>'([a, f], [_, _])), '<urn:example:dijkstra>'([a, f], [[a, c, b, d, e, f], 13]), true).
step((true:+'<urn:example:dijkstra>'([a, f], [_, _])), '<urn:example:dijkstra>'([a, f], [[a, c, b, d, f], 14]), true).
step((true:+'<urn:example:dijkstra>'([a, f], [_, _])), '<urn:example:dijkstra>'([a, f], [[a, b, d, e, f], 14]), true).
step((true:+'<urn:example:dijkstra>'([a, f], [_, _])), '<urn:example:dijkstra>'([a, f], [[a, b, d, f], 15]), true).
step((true:+'<urn:example:dijkstra>'([a, f], [_, _])), '<urn:example:dijkstra>'([a, f], [[a, c, e, f], 15]), true).
step((true:+'<urn:example:dijkstra>'([a, f], [_, _])), '<urn:example:dijkstra>'([a, f], [[a, c, d, e, f], 15]), true).
step((true:+'<urn:example:dijkstra>'([a, f], [_, _])), '<urn:example:dijkstra>'([a, f], [[a, c, d, f], 16]), true).
