display(user_1,perm([Patrick]),not([Brugge,1971])).
display(user_1,perm([Brugge]),not([Patrick,1971])).
display(user_1,perm([1971]),not([Patrick,Brugge])).
display(user_1,perm([Brugge,Patrick]),not([1971])).
display(user_1,perm([1971,Patrick]),not([Brugge])).
display(user_1,perm([1971,Brugge]),not([Patrick])).

step(rule(display(A, B, C), answer(display(A, B, C))), display(user_1, perm(['Patrick']), not(['Brugge', 1971])), answer(display(user_1, perm(['Patrick']), not(['Brugge', 1971])))).
step(rule(display(A, B, C), answer(display(A, B, C))), display(user_1, perm(['Brugge']), not(['Patrick', 1971])), answer(display(user_1, perm(['Brugge']), not(['Patrick', 1971])))).
step(rule(display(A, B, C), answer(display(A, B, C))), display(user_1, perm([1971]), not(['Patrick', 'Brugge'])), answer(display(user_1, perm([1971]), not(['Patrick', 'Brugge'])))).
step(rule(display(A, B, C), answer(display(A, B, C))), display(user_1, perm(['Brugge', 'Patrick']), not([1971])), answer(display(user_1, perm(['Brugge', 'Patrick']), not([1971])))).
step(rule(display(A, B, C), answer(display(A, B, C))), display(user_1, perm([1971, 'Patrick']), not(['Brugge'])), answer(display(user_1, perm([1971, 'Patrick']), not(['Brugge'])))).
step(rule(display(A, B, C), answer(display(A, B, C))), display(user_1, perm([1971, 'Brugge']), not(['Patrick'])), answer(display(user_1, perm([1971, 'Brugge']), not(['Patrick'])))).
