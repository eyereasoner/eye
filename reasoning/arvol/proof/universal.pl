type(Pat,Resource).
loves(Bob,lonely(skolem(Bob))).

step(rule(type('Pat', 'Resource'), answer(type('Pat', 'Resource'))), type('Pat', 'Resource'), answer(type('Pat', 'Resource'))).
step(rule(loves('Bob', A), answer(loves('Bob', A))), loves('Bob', lonely(skolem('Bob'))), answer(loves('Bob', lonely(skolem('Bob'))))).
