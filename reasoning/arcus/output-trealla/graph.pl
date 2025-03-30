:- op(1200, xfx, :+).

answer('urn:example:path'('urn:example:angers','urn:example:nantes')).
answer('urn:example:path'('urn:example:lemans','urn:example:nantes')).
answer('urn:example:path'('urn:example:chartres','urn:example:nantes')).
answer('urn:example:path'('urn:example:paris','urn:example:nantes')).

step(('urn:example:path'(A,B):+'urn:example:oneway'(A,B)),'urn:example:oneway'('urn:example:paris','urn:example:orleans'),'urn:example:path'('urn:example:paris','urn:example:orleans')).
step(('urn:example:path'(A,B):+'urn:example:oneway'(A,B)),'urn:example:oneway'('urn:example:paris','urn:example:chartres'),'urn:example:path'('urn:example:paris','urn:example:chartres')).
step(('urn:example:path'(A,B):+'urn:example:oneway'(A,B)),'urn:example:oneway'('urn:example:paris','urn:example:amiens'),'urn:example:path'('urn:example:paris','urn:example:amiens')).
step(('urn:example:path'(A,B):+'urn:example:oneway'(A,B)),'urn:example:oneway'('urn:example:orleans','urn:example:blois'),'urn:example:path'('urn:example:orleans','urn:example:blois')).
step(('urn:example:path'(A,B):+'urn:example:oneway'(A,B)),'urn:example:oneway'('urn:example:orleans','urn:example:bourges'),'urn:example:path'('urn:example:orleans','urn:example:bourges')).
step(('urn:example:path'(A,B):+'urn:example:oneway'(A,B)),'urn:example:oneway'('urn:example:blois','urn:example:tours'),'urn:example:path'('urn:example:blois','urn:example:tours')).
step(('urn:example:path'(A,B):+'urn:example:oneway'(A,B)),'urn:example:oneway'('urn:example:chartres','urn:example:lemans'),'urn:example:path'('urn:example:chartres','urn:example:lemans')).
step(('urn:example:path'(A,B):+'urn:example:oneway'(A,B)),'urn:example:oneway'('urn:example:lemans','urn:example:angers'),'urn:example:path'('urn:example:lemans','urn:example:angers')).
step(('urn:example:path'(A,B):+'urn:example:oneway'(A,B)),'urn:example:oneway'('urn:example:lemans','urn:example:tours'),'urn:example:path'('urn:example:lemans','urn:example:tours')).
step(('urn:example:path'(A,B):+'urn:example:oneway'(A,B)),'urn:example:oneway'('urn:example:angers','urn:example:nantes'),'urn:example:path'('urn:example:angers','urn:example:nantes')).
step(('urn:example:path'(A,B):+'urn:example:path'(A,C),'urn:example:path'(C,B)),('urn:example:path'('urn:example:paris','urn:example:orleans'),'urn:example:path'('urn:example:orleans','urn:example:blois')),'urn:example:path'('urn:example:paris','urn:example:blois')).
step(('urn:example:path'(A,B):+'urn:example:path'(A,C),'urn:example:path'(C,B)),('urn:example:path'('urn:example:paris','urn:example:orleans'),'urn:example:path'('urn:example:orleans','urn:example:bourges')),'urn:example:path'('urn:example:paris','urn:example:bourges')).
step(('urn:example:path'(A,B):+'urn:example:path'(A,C),'urn:example:path'(C,B)),('urn:example:path'('urn:example:paris','urn:example:chartres'),'urn:example:path'('urn:example:chartres','urn:example:lemans')),'urn:example:path'('urn:example:paris','urn:example:lemans')).
step(('urn:example:path'(A,B):+'urn:example:path'(A,C),'urn:example:path'(C,B)),('urn:example:path'('urn:example:orleans','urn:example:blois'),'urn:example:path'('urn:example:blois','urn:example:tours')),'urn:example:path'('urn:example:orleans','urn:example:tours')).
step(('urn:example:path'(A,B):+'urn:example:path'(A,C),'urn:example:path'(C,B)),('urn:example:path'('urn:example:chartres','urn:example:lemans'),'urn:example:path'('urn:example:lemans','urn:example:angers')),'urn:example:path'('urn:example:chartres','urn:example:angers')).
step(('urn:example:path'(A,B):+'urn:example:path'(A,C),'urn:example:path'(C,B)),('urn:example:path'('urn:example:chartres','urn:example:lemans'),'urn:example:path'('urn:example:lemans','urn:example:tours')),'urn:example:path'('urn:example:chartres','urn:example:tours')).
step(('urn:example:path'(A,B):+'urn:example:path'(A,C),'urn:example:path'(C,B)),('urn:example:path'('urn:example:lemans','urn:example:angers'),'urn:example:path'('urn:example:angers','urn:example:nantes')),'urn:example:path'('urn:example:lemans','urn:example:nantes')).
step((true:+'urn:example:path'(A,'urn:example:nantes')),'urn:example:path'('urn:example:angers','urn:example:nantes'),true).
step((true:+'urn:example:path'(A,'urn:example:nantes')),'urn:example:path'('urn:example:lemans','urn:example:nantes'),true).
step(('urn:example:path'(A,B):+'urn:example:path'(A,C),'urn:example:path'(C,B)),('urn:example:path'('urn:example:paris','urn:example:orleans'),'urn:example:path'('urn:example:orleans','urn:example:tours')),'urn:example:path'('urn:example:paris','urn:example:tours')).
step(('urn:example:path'(A,B):+'urn:example:path'(A,C),'urn:example:path'(C,B)),('urn:example:path'('urn:example:paris','urn:example:chartres'),'urn:example:path'('urn:example:chartres','urn:example:angers')),'urn:example:path'('urn:example:paris','urn:example:angers')).
step(('urn:example:path'(A,B):+'urn:example:path'(A,C),'urn:example:path'(C,B)),('urn:example:path'('urn:example:chartres','urn:example:lemans'),'urn:example:path'('urn:example:lemans','urn:example:nantes')),'urn:example:path'('urn:example:chartres','urn:example:nantes')).
step(('urn:example:path'(A,B):+'urn:example:path'(A,C),'urn:example:path'(C,B)),('urn:example:path'('urn:example:paris','urn:example:lemans'),'urn:example:path'('urn:example:lemans','urn:example:nantes')),'urn:example:path'('urn:example:paris','urn:example:nantes')).
step((true:+'urn:example:path'(A,'urn:example:nantes')),'urn:example:path'('urn:example:chartres','urn:example:nantes'),true).
step((true:+'urn:example:path'(A,'urn:example:nantes')),'urn:example:path'('urn:example:paris','urn:example:nantes'),true).
