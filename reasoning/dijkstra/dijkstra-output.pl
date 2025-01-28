:- op(1200, xfx, :+).

answer('<urn:example:shortest_path>'([a, d], [[a, b, c, c, d], 6])).

% proof steps
step((true:+'<urn:example:shortest_path>'([a, d], [_, _])), '<urn:example:shortest_path>'([a, d], [[a, b, c, c, d], 6]), true).
