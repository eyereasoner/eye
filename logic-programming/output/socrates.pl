:- op(1200, xfx, :+).

% answers
  answer('<urn:example:Mortal>'('<urn:example:Socrates>')).

% proof steps
  step(('<urn:example:Mortal>'(A):+'<urn:example:Human>'(A)),
       '<urn:example:Human>'('<urn:example:Socrates>'),
       '<urn:example:Mortal>'('<urn:example:Socrates>')).
