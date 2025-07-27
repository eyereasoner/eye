type(Socrates,Man).
type(Socrates,Mortal).

step(rule(type(A, B), answer(type(A, B))), type('Socrates', 'Man'), answer(type('Socrates', 'Man'))).
step(rule(type(A, 'Man'), type(A, 'Mortal')), type('Socrates', 'Man'), type('Socrates', 'Mortal')).
step(rule(type(A, B), answer(type(A, B))), type('Socrates', 'Mortal'), answer(type('Socrates', 'Mortal'))).
