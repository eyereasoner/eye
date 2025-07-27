path(angers,nantes).
path(lemans,nantes).
path(chartres,nantes).
path(paris,nantes).

step(rule(oneway(A, B), path(A, B)), oneway(paris, orleans), path(paris, orleans)).
step(rule(oneway(A, B), path(A, B)), oneway(paris, chartres), path(paris, chartres)).
step(rule(oneway(A, B), path(A, B)), oneway(paris, amiens), path(paris, amiens)).
step(rule(oneway(A, B), path(A, B)), oneway(orleans, blois), path(orleans, blois)).
step(rule(oneway(A, B), path(A, B)), oneway(orleans, bourges), path(orleans, bourges)).
step(rule(oneway(A, B), path(A, B)), oneway(blois, tours), path(blois, tours)).
step(rule(oneway(A, B), path(A, B)), oneway(chartres, lemans), path(chartres, lemans)).
step(rule(oneway(A, B), path(A, B)), oneway(lemans, angers), path(lemans, angers)).
step(rule(oneway(A, B), path(A, B)), oneway(lemans, tours), path(lemans, tours)).
step(rule(oneway(A, B), path(A, B)), oneway(angers, nantes), path(angers, nantes)).
step(rule((path(A, B), path(B, C)), path(A, C)), (path(paris, orleans), path(orleans, blois)), path(paris, blois)).
step(rule((path(A, B), path(B, C)), path(A, C)), (path(paris, orleans), path(orleans, bourges)), path(paris, bourges)).
step(rule((path(A, B), path(B, C)), path(A, C)), (path(paris, chartres), path(chartres, lemans)), path(paris, lemans)).
step(rule((path(A, B), path(B, C)), path(A, C)), (path(orleans, blois), path(blois, tours)), path(orleans, tours)).
step(rule((path(A, B), path(B, C)), path(A, C)), (path(chartres, lemans), path(lemans, angers)), path(chartres, angers)).
step(rule((path(A, B), path(B, C)), path(A, C)), (path(chartres, lemans), path(lemans, tours)), path(chartres, tours)).
step(rule((path(A, B), path(B, C)), path(A, C)), (path(lemans, angers), path(angers, nantes)), path(lemans, nantes)).
step(rule(path(A, nantes), answer(path(A, nantes))), path(angers, nantes), answer(path(angers, nantes))).
step(rule(path(A, nantes), answer(path(A, nantes))), path(lemans, nantes), answer(path(lemans, nantes))).
step(rule((path(A, B), path(B, C)), path(A, C)), (path(paris, orleans), path(orleans, tours)), path(paris, tours)).
step(rule((path(A, B), path(B, C)), path(A, C)), (path(paris, chartres), path(chartres, angers)), path(paris, angers)).
step(rule((path(A, B), path(B, C)), path(A, C)), (path(chartres, lemans), path(lemans, nantes)), path(chartres, nantes)).
step(rule((path(A, B), path(B, C)), path(A, C)), (path(paris, lemans), path(lemans, nantes)), path(paris, nantes)).
step(rule(path(A, nantes), answer(path(A, nantes))), path(chartres, nantes), answer(path(chartres, nantes))).
step(rule(path(A, nantes), answer(path(A, nantes))), path(paris, nantes), answer(path(paris, nantes))).
