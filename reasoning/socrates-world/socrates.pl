% fact
'<http://example.org/#Human>'('Socrates').

% subclass logic
'<http://example.org/#Mortal>'(X) :-
    '<http://example.org/#Human>'(X).

% query
query('<http://example.org/#Mortal>'(_X)).
