:- op(1200, xfx, :+).

answer('<urn:example:compute>'([1, 0, 1, 0, 0, 1], [1, 0, 1, 0, 1, 0, #])).
answer('<urn:example:compute>'([1, 0, 1, 1, 1, 1], [1, 1, 0, 0, 0, 0, #])).
answer('<urn:example:compute>'([1, 1, 1, 1, 1, 1], [1, 0, 0, 0, 0, 0, 0, #])).
answer('<urn:example:compute>'([], [1, #])).
