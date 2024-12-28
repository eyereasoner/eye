% Socrates is a mortal

'<urn:example:Human>'('<urn:example:Socrates>').

'<urn:example:Mortal>'(X) :+ '<urn:example:Human>'(X).

% query
true :+ '<urn:example:Mortal>'(_).
