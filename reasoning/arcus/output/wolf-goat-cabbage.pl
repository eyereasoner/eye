:- op(1200, xfx, :+).

answer('urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, cabbage, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, wolf, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, goat, goat, nothing, wolf, goat, cabbage, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, goat, goat, nothing, cabbage, goat, wolf, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, wolf, wolf, wolf, goat, cabbage, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, wolf, wolf, cabbage, goat, wolf, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, goat, goat, cabbage, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, cabbage, wolf, wolf, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, cabbage, cabbage, cabbage, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, cabbage, nothing, goat, goat, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, cabbage, nothing, nothing, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, wolf, wolf, wolf, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, wolf, cabbage, cabbage, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, wolf, nothing, goat, goat, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, wolf, nothing, nothing, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, goat, goat, wolf, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, cabbage, wolf, goat, cabbage, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, cabbage, cabbage, goat, wolf, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, nothing, nothing, wolf, goat, cabbage, nothing, goat])).
answer('urn:example:solution'([w, w, w, w], [goat, nothing, nothing, nothing, cabbage, goat, wolf, nothing, goat])).

step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, cabbage, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, wolf, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, goat, goat, nothing, wolf, goat, cabbage, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, goat, goat, nothing, cabbage, goat, wolf, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, wolf, wolf, wolf, goat, cabbage, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, wolf, wolf, cabbage, goat, wolf, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, goat, goat, cabbage, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, cabbage, wolf, wolf, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, cabbage, cabbage, cabbage, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, cabbage, nothing, goat, goat, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, wolf, goat, cabbage, nothing, nothing, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, wolf, wolf, wolf, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, wolf, cabbage, cabbage, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, wolf, nothing, goat, goat, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, wolf, nothing, nothing, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, goat, goat, goat, wolf, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, cabbage, wolf, goat, cabbage, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, cabbage, cabbage, cabbage, goat, wolf, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, nothing, nothing, wolf, goat, cabbage, nothing, goat]), true).
step((true:+'urn:example:solution'([w, w, w, w], [_, _, _, _, _, _, _, _, _])), 'urn:example:solution'([w, w, w, w], [goat, nothing, nothing, nothing, cabbage, goat, wolf, nothing, goat]), true).
