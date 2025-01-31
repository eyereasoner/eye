% If you have more than 4 dogs you need a license.

:- op(1200, xfx, :+).

'<urn:example:hasDog>'('<urn:example:alice>', '<urn:example:dog1>').
'<urn:example:hasDog>'('<urn:example:alice>', '<urn:example:dog2>').
'<urn:example:hasDog>'('<urn:example:alice>', '<urn:example:dog3>').
'<urn:example:hasDog>'('<urn:example:alice>', '<urn:example:dog4>').
'<urn:example:hasDog>'('<urn:example:alice>', '<urn:example:dog5>').
'<urn:example:hasDog>'('<urn:example:bob>', '<urn:example:dog6>').
'<urn:example:hasDog>'('<urn:example:bob>', '<urn:example:dog7>').

'<urn:example:mustHave>'(Subject, '<urn:example:dogLicense>') :-
    '<urn:example:hasDog>'(Subject, _),
    findall(Dog, '<urn:example:hasDog>'(Subject, Dog), List),
    length(List, Count),
    Count > 4.

% query
true :+ '<urn:example:mustHave>'(_, _).
