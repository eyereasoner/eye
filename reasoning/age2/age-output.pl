:- op(1200, xfx, :+).

answer('<urn:example:ageAbove>'('<urn:example:patH>', literal('P80Y', type('<http://www.w3.org/2001/XMLSchema#duration>')))).

% proof steps
step((true:+'<urn:example:ageAbove>'(_, literal('P80Y', type('<http://www.w3.org/2001/XMLSchema#duration>')))), '<urn:example:ageAbove>'('<urn:example:patH>', literal('P80Y', type('<http://www.w3.org/2001/XMLSchema#duration>'))), true).
