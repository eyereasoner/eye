% Socrates is a mortal

:- op(1200, xfx, :+).

'<urn:example:Human>'('<urn:example:Socrates>').

'<urn:example:Mortal>'(X) :+ '<urn:example:Human>'(X).

% query
true :+ '<urn:example:Mortal>'(_).
