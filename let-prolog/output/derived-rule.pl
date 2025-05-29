:- op(1200, xfx, :+).

answer(ascribed(test, true)).

step(((ascribed(test, true):+type(_, 'Dog')):+type(_, 'Cat')), type('Minka', 'Cat'), (ascribed(test, true):+type(_, 'Dog'))).
step((ascribed(test, true):+type(_, 'Dog')), type('Charly', 'Dog'), ascribed(test, true)).
step((true:+ascribed(test, true)), ascribed(test, true), true).
