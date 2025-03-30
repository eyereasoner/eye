:- op(1200, xfx, :+).

answer('urn:example:path'(['urn:example:angers','urn:example:nantes'],go('urn:example:angers','urn:example:nantes',goal))).
answer('urn:example:path'(['urn:example:paris','urn:example:nantes'],go('urn:example:paris','urn:example:chartres',go('urn:example:chartres','urn:example:lemans',go('urn:example:lemans','urn:example:angers',go('urn:example:angers','urn:example:nantes',goal)))))).
answer('urn:example:path'(['urn:example:chartres','urn:example:nantes'],go('urn:example:chartres','urn:example:lemans',go('urn:example:lemans','urn:example:angers',go('urn:example:angers','urn:example:nantes',goal))))).
answer('urn:example:path'(['urn:example:lemans','urn:example:nantes'],go('urn:example:lemans','urn:example:angers',go('urn:example:angers','urn:example:nantes',goal)))).

step((true:+'urn:example:path'([A,'urn:example:nantes'],B)),'urn:example:path'(['urn:example:angers','urn:example:nantes'],go('urn:example:angers','urn:example:nantes',goal)),true).
step((true:+'urn:example:path'([A,'urn:example:nantes'],B)),'urn:example:path'(['urn:example:paris','urn:example:nantes'],go('urn:example:paris','urn:example:chartres',go('urn:example:chartres','urn:example:lemans',go('urn:example:lemans','urn:example:angers',go('urn:example:angers','urn:example:nantes',goal))))),true).
step((true:+'urn:example:path'([A,'urn:example:nantes'],B)),'urn:example:path'(['urn:example:chartres','urn:example:nantes'],go('urn:example:chartres','urn:example:lemans',go('urn:example:lemans','urn:example:angers',go('urn:example:angers','urn:example:nantes',goal)))),true).
step((true:+'urn:example:path'([A,'urn:example:nantes'],B)),'urn:example:path'(['urn:example:lemans','urn:example:nantes'],go('urn:example:lemans','urn:example:angers',go('urn:example:angers','urn:example:nantes',goal))),true).
