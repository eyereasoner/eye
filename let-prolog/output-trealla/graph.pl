:- op(1200, xfx, :+).

answer(path(angers,nantes)).
answer(path(lemans,nantes)).
answer(path(chartres,nantes)).
answer(path(paris,nantes)).

step((path(A,B):+oneway(A,B)),oneway(paris,orleans),path(paris,orleans)).
step((path(A,B):+oneway(A,B)),oneway(paris,chartres),path(paris,chartres)).
step((path(A,B):+oneway(A,B)),oneway(paris,amiens),path(paris,amiens)).
step((path(A,B):+oneway(A,B)),oneway(orleans,blois),path(orleans,blois)).
step((path(A,B):+oneway(A,B)),oneway(orleans,bourges),path(orleans,bourges)).
step((path(A,B):+oneway(A,B)),oneway(blois,tours),path(blois,tours)).
step((path(A,B):+oneway(A,B)),oneway(chartres,lemans),path(chartres,lemans)).
step((path(A,B):+oneway(A,B)),oneway(lemans,angers),path(lemans,angers)).
step((path(A,B):+oneway(A,B)),oneway(lemans,tours),path(lemans,tours)).
step((path(A,B):+oneway(A,B)),oneway(angers,nantes),path(angers,nantes)).
step((path(A,B):+path(A,C),path(C,B)),(path(paris,orleans),path(orleans,blois)),path(paris,blois)).
step((path(A,B):+path(A,C),path(C,B)),(path(paris,orleans),path(orleans,bourges)),path(paris,bourges)).
step((path(A,B):+path(A,C),path(C,B)),(path(paris,chartres),path(chartres,lemans)),path(paris,lemans)).
step((path(A,B):+path(A,C),path(C,B)),(path(orleans,blois),path(blois,tours)),path(orleans,tours)).
step((path(A,B):+path(A,C),path(C,B)),(path(chartres,lemans),path(lemans,angers)),path(chartres,angers)).
step((path(A,B):+path(A,C),path(C,B)),(path(chartres,lemans),path(lemans,tours)),path(chartres,tours)).
step((path(A,B):+path(A,C),path(C,B)),(path(lemans,angers),path(angers,nantes)),path(lemans,nantes)).
step((true:+path(A,nantes)),path(angers,nantes),true).
step((true:+path(A,nantes)),path(lemans,nantes),true).
step((path(A,B):+path(A,C),path(C,B)),(path(paris,orleans),path(orleans,tours)),path(paris,tours)).
step((path(A,B):+path(A,C),path(C,B)),(path(paris,chartres),path(chartres,angers)),path(paris,angers)).
step((path(A,B):+path(A,C),path(C,B)),(path(chartres,lemans),path(lemans,nantes)),path(chartres,nantes)).
step((path(A,B):+path(A,C),path(C,B)),(path(paris,lemans),path(lemans,nantes)),path(paris,nantes)).
step((true:+path(A,nantes)),path(chartres,nantes),true).
step((true:+path(A,nantes)),path(paris,nantes),true).
