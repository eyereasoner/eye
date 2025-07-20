:- op(1200, xfx, :+).

answer(display(user_1,perm(['Patrick']),not(['Brugge',1971]))).
answer(display(user_1,perm(['Brugge']),not(['Patrick',1971]))).
answer(display(user_1,perm([1971]),not(['Patrick','Brugge']))).
answer(display(user_1,perm(['Brugge','Patrick']),not([1971]))).
answer(display(user_1,perm([1971,'Patrick']),not(['Brugge']))).
answer(display(user_1,perm([1971,'Brugge']),not(['Patrick']))).

step((true:+display(A,B,C)),display(user_1,perm(['Patrick']),not(['Brugge',1971])),true).
step((true:+display(A,B,C)),display(user_1,perm(['Brugge']),not(['Patrick',1971])),true).
step((true:+display(A,B,C)),display(user_1,perm([1971]),not(['Patrick','Brugge'])),true).
step((true:+display(A,B,C)),display(user_1,perm(['Brugge','Patrick']),not([1971])),true).
step((true:+display(A,B,C)),display(user_1,perm([1971,'Patrick']),not(['Brugge'])),true).
step((true:+display(A,B,C)),display(user_1,perm([1971,'Brugge']),not(['Patrick'])),true).
