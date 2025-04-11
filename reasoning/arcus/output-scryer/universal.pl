:- op(1200, xfx, :+).

answer('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:Pat','http://www.w3.org/2000/01/rdf-schema#Resource')).
answer('urn:example:loves'('urn:example:Bob','urn:example:Lonely'(skolem('urn:example:Bob')))).

step((true:+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:Pat','http://www.w3.org/2000/01/rdf-schema#Resource')),'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:Pat','http://www.w3.org/2000/01/rdf-schema#Resource'),true).
step((true:+'urn:example:loves'('urn:example:Bob',A)),'urn:example:loves'('urn:example:Bob','urn:example:Lonely'(skolem('urn:example:Bob'))),true).
