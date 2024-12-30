:- op(1200, xfx, :+).

% inference fuse, return code 2
  fuse('<urn:example:Mortal>'('<urn:example:Socrates>')).

% proof steps
  step(('<urn:example:Mortal>'(A):+'<urn:example:Human>'(A)),
       '<urn:example:Human>'('<urn:example:Socrates>'),
       '<urn:example:Mortal>'('<urn:example:Socrates>')).
  step((false:+'<urn:example:Mortal>'(_)),
       '<urn:example:Mortal>'('<urn:example:Socrates>'),
       false).
