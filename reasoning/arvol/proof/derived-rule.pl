ascribed(test,true).

step(rule(type(_, 'Cat'), (ascribed(test, true):+type(_, 'Dog'))), type('Minka', 'Cat'), (ascribed(test, true):+type(some0, 'Dog'))).
step(rule(type(_, 'Dog'), ascribed(test, true)), type('Charly', 'Dog'), ascribed(test, true)).
step(rule(ascribed(test, true), answer(ascribed(test, true))), ascribed(test, true), answer(ascribed(test, true))).
