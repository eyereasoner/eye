:- op(1200, xfx, :+).

answer(type('Socrates', 'Man')).
answer(type('Socrates', 'Mortal')).

step((type(A, 'Mortal'):+type(A, 'Man')), type('Socrates', 'Man'), type('Socrates', 'Mortal')).
step((true:+type(_, _)), type('Socrates', 'Man'), true).
step((true:+type(_, _)), type('Socrates', 'Mortal'), true).
