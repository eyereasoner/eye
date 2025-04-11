:- op(1200, xfx, :+).

fuse('urn:example:Mortal'('urn:example:Socrates')).

step(('urn:example:Mortal'(A):+'urn:example:Human'(A)), 'urn:example:Human'('urn:example:Socrates'), 'urn:example:Mortal'('urn:example:Socrates')).
