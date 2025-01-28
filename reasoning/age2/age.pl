:- op(1200, xfx, :+).

% person data
'<urn:example:birthDay>'('<urn:example:patH>', literal('1944-08-21', type('<http://www.w3.org/2001/XMLSchema#date>'))).

% is the age of a person above some duration?
'<urn:example:ageAbove>'(A, B) :-
    '<urn:example:birthDay>'(A, C),
    '<http://www.w3.org/2000/10/swap/time#localTime>'(literal('', type('<http://www.w3.org/2001/XMLSchema#string>')), D),
    '<http://www.w3.org/2000/10/swap/math#difference>'([D, C], E),
    '<http://www.w3.org/2000/10/swap/math#greaterThan>'(E, B).

% query
true :+ '<urn:example:ageAbove>'(_, literal('P80Y', type('<http://www.w3.org/2001/XMLSchema#duration>'))).
