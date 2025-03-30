:- op(1200, xfx, :+).

answer('urn:example:is'('urn:example:test', true)).

step((('urn:example:is'('urn:example:test', true):+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, 'urn:example:Dog')):+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, 'urn:example:Cat')), 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:Minka', 'urn:example:Cat'), ('urn:example:is'('urn:example:test', true):+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, 'urn:example:Dog'))).
step(('urn:example:is'('urn:example:test', true):+'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, 'urn:example:Dog')), 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:Charly', 'urn:example:Dog'), 'urn:example:is'('urn:example:test', true)).
step((true:+'urn:example:is'('urn:example:test', true)), 'urn:example:is'('urn:example:test', true), true).
