% Good Cobbler

:- op(1200, xfx, :+).

% some x is a good cobbler
'<urn:example:is>'(_, '<urn:example:good>'('<urn:example:Cobbler>')) :+ true.

% is there some x which is good at some y
true :+ ('<urn:example:is>'(_, '<urn:example:good>'(_)) :+ true).
