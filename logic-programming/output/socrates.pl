:- op(1200, xfx, :+).

answer('<urn:example:Mortal>'('<urn:example:Socrates>')).

%
% Proof steps
%

step(('<urn:example:Mortal>'(A):+'<urn:example:Human>'(A)), '<urn:example:Human>'('<urn:example:Socrates>'), '<urn:example:Mortal>'('<urn:example:Socrates>')).
