:- op(1200, xfx, :+).

answer('urn:example:pi'(100000, 3.141592653589792)).

step((true:+'urn:example:pi'(100000, _)), 'urn:example:pi'(100000, 3.141592653589792), true).
