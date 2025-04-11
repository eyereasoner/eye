:- op(1200, xfx, :+).

answer(('urn:example:Policy'('urn:example:PolicyX'), 'urn:example:pass'('urn:example:PolicyX', 'urn:example:allOfTest'), 'urn:example:pass'('urn:example:PolicyX', 'urn:example:anyOfTest'), 'urn:example:pass'('urn:example:PolicyX', 'urn:example:noneOfTest'))).

step((true:+'urn:example:Policy'(A), 'urn:example:pass'(A, 'urn:example:allOfTest'), 'urn:example:pass'(A, 'urn:example:anyOfTest'), 'urn:example:pass'(A, 'urn:example:noneOfTest')), ('urn:example:Policy'('urn:example:PolicyX'), 'urn:example:pass'('urn:example:PolicyX', 'urn:example:allOfTest'), 'urn:example:pass'('urn:example:PolicyX', 'urn:example:anyOfTest'), 'urn:example:pass'('urn:example:PolicyX', 'urn:example:noneOfTest')), true).
