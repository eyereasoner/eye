path([angers,nantes],go(angers,nantes,goal)).
path([paris,nantes],go(paris,chartres,go(chartres,lemans,go(lemans,angers,go(angers,nantes,goal))))).
path([chartres,nantes],go(chartres,lemans,go(lemans,angers,go(angers,nantes,goal)))).
path([lemans,nantes],go(lemans,angers,go(angers,nantes,goal))).
