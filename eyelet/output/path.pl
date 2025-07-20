:- op(1200, xfx, :+).

answer(path([angers,nantes],go(angers,nantes,goal))).
answer(path([paris,nantes],go(paris,chartres,go(chartres,lemans,go(lemans,angers,go(angers,nantes,goal)))))).
answer(path([chartres,nantes],go(chartres,lemans,go(lemans,angers,go(angers,nantes,goal))))).
answer(path([lemans,nantes],go(lemans,angers,go(angers,nantes,goal)))).

step((true:+path([A,nantes],B)),path([angers,nantes],go(angers,nantes,goal)),true).
step((true:+path([A,nantes],B)),path([paris,nantes],go(paris,chartres,go(chartres,lemans,go(lemans,angers,go(angers,nantes,goal))))),true).
step((true:+path([A,nantes],B)),path([chartres,nantes],go(chartres,lemans,go(lemans,angers,go(angers,nantes,goal)))),true).
step((true:+path([A,nantes],B)),path([lemans,nantes],go(lemans,angers,go(angers,nantes,goal))),true).
