% Good Cobbler

:- op(1200, xfx, :+).

% some x is a good cobbler
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, good(cobbler)) :+ true.

% is there some x which is good at some y
true :+ ('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, good(_)) :+ true).
