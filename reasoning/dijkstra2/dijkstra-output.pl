:- op(1200, xfx, :+).

answer('<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:c>', '<urn:example:b>', '<urn:example:d>', '<urn:example:e>', '<urn:example:f>'], 13])).
answer('<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:c>', '<urn:example:b>', '<urn:example:d>', '<urn:example:f>'], 14])).
answer('<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:b>', '<urn:example:d>', '<urn:example:e>', '<urn:example:f>'], 14])).
answer('<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:b>', '<urn:example:d>', '<urn:example:f>'], 15])).
answer('<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:c>', '<urn:example:e>', '<urn:example:f>'], 15])).
answer('<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:c>', '<urn:example:d>', '<urn:example:e>', '<urn:example:f>'], 15])).
answer('<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:c>', '<urn:example:d>', '<urn:example:f>'], 16])).

% proof steps
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'(['<urn:example:a>', '<urn:example:b>'], 4), '<urn:example:edge>'(['<urn:example:b>', '<urn:example:a>'], 4)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'(['<urn:example:a>', '<urn:example:c>'], 2), '<urn:example:edge>'(['<urn:example:c>', '<urn:example:a>'], 2)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'(['<urn:example:b>', '<urn:example:c>'], 1), '<urn:example:edge>'(['<urn:example:c>', '<urn:example:b>'], 1)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'(['<urn:example:b>', '<urn:example:d>'], 5), '<urn:example:edge>'(['<urn:example:d>', '<urn:example:b>'], 5)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'(['<urn:example:c>', '<urn:example:d>'], 8), '<urn:example:edge>'(['<urn:example:d>', '<urn:example:c>'], 8)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'(['<urn:example:c>', '<urn:example:e>'], 10), '<urn:example:edge>'(['<urn:example:e>', '<urn:example:c>'], 10)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'(['<urn:example:d>', '<urn:example:e>'], 2), '<urn:example:edge>'(['<urn:example:e>', '<urn:example:d>'], 2)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'(['<urn:example:d>', '<urn:example:f>'], 6), '<urn:example:edge>'(['<urn:example:f>', '<urn:example:d>'], 6)).
step(('<urn:example:edge>'([A, B], C):+'<urn:example:edge>'([B, A], C)), '<urn:example:edge>'(['<urn:example:e>', '<urn:example:f>'], 3), '<urn:example:edge>'(['<urn:example:f>', '<urn:example:e>'], 3)).
step((true:+'<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [_, _])), '<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:c>', '<urn:example:b>', '<urn:example:d>', '<urn:example:e>', '<urn:example:f>'], 13]), true).
step((true:+'<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [_, _])), '<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:c>', '<urn:example:b>', '<urn:example:d>', '<urn:example:f>'], 14]), true).
step((true:+'<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [_, _])), '<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:b>', '<urn:example:d>', '<urn:example:e>', '<urn:example:f>'], 14]), true).
step((true:+'<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [_, _])), '<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:b>', '<urn:example:d>', '<urn:example:f>'], 15]), true).
step((true:+'<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [_, _])), '<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:c>', '<urn:example:e>', '<urn:example:f>'], 15]), true).
step((true:+'<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [_, _])), '<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:c>', '<urn:example:d>', '<urn:example:e>', '<urn:example:f>'], 15]), true).
step((true:+'<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [_, _])), '<urn:example:dijkstra>'(['<urn:example:a>', '<urn:example:f>'], [['<urn:example:a>', '<urn:example:c>', '<urn:example:d>', '<urn:example:f>'], 16]), true).
