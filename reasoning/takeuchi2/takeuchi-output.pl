:- op(1200, xfx, :+).

answer('<urn:example:tak>'([34, 13, 8], 13)).

% proof steps
step((true:+'<urn:example:tak>'([34, 13, 8], _)), '<urn:example:tak>'([34, 13, 8], 13), true).
