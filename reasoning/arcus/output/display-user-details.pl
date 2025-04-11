:- op(1200, xfx, :+).

answer('urn:example:display'(user_1, perm(['Patrick']), not(['Brugge', 1971]))).
answer('urn:example:display'(user_1, perm(['Brugge']), not(['Patrick', 1971]))).
answer('urn:example:display'(user_1, perm([1971]), not(['Patrick', 'Brugge']))).
answer('urn:example:display'(user_1, perm(['Brugge', 'Patrick']), not([1971]))).
answer('urn:example:display'(user_1, perm([1971, 'Patrick']), not(['Brugge']))).
answer('urn:example:display'(user_1, perm([1971, 'Brugge']), not(['Patrick']))).

step((true:+'urn:example:display'(_, _, _)), 'urn:example:display'(user_1, perm(['Patrick']), not(['Brugge', 1971])), true).
step((true:+'urn:example:display'(_, _, _)), 'urn:example:display'(user_1, perm(['Brugge']), not(['Patrick', 1971])), true).
step((true:+'urn:example:display'(_, _, _)), 'urn:example:display'(user_1, perm([1971]), not(['Patrick', 'Brugge'])), true).
step((true:+'urn:example:display'(_, _, _)), 'urn:example:display'(user_1, perm(['Brugge', 'Patrick']), not([1971])), true).
step((true:+'urn:example:display'(_, _, _)), 'urn:example:display'(user_1, perm([1971, 'Patrick']), not(['Brugge'])), true).
step((true:+'urn:example:display'(_, _, _)), 'urn:example:display'(user_1, perm([1971, 'Brugge']), not(['Patrick'])), true).
