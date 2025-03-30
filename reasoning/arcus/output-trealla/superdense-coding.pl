:- op(1200, xfx, :+).

answer('urn:example:sdcoding'(1,1)).
answer('urn:example:sdcoding'(3,3)).
answer('urn:example:sdcoding'(0,0)).
answer('urn:example:sdcoding'(2,2)).

step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(0,1),'urn:example:sdconot'(0,1)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(0,3),'urn:example:sdconot'(0,3)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(1,0),'urn:example:sdconot'(1,0)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(1,1),'urn:example:sdconot'(1,1)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(1,2),'urn:example:sdconot'(1,2)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(2,1),'urn:example:sdconot'(2,1)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(2,3),'urn:example:sdconot'(2,3)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(3,1),'urn:example:sdconot'(3,1)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(3,3),'urn:example:sdconot'(3,3)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(3,0),'urn:example:sdconot'(3,0)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(3,2),'urn:example:sdconot'(3,2)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(0,0),'urn:example:sdconot'(0,0)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(2,0),'urn:example:sdconot'(2,0)).
step(('urn:example:sdconot'(A,B):+'urn:example:sdc'(A,B)),'urn:example:sdc'(2,2),'urn:example:sdconot'(2,2)).
step((true:+'urn:example:sdcoding'(A,B)),'urn:example:sdcoding'(1,1),true).
step((true:+'urn:example:sdcoding'(A,B)),'urn:example:sdcoding'(3,3),true).
step((true:+'urn:example:sdcoding'(A,B)),'urn:example:sdcoding'(0,0),true).
step((true:+'urn:example:sdcoding'(A,B)),'urn:example:sdcoding'(2,2),true).
