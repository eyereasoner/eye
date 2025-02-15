:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#ageAbove>'(patH, literal('P80Y', type('<http://www.w3.org/2001/XMLSchema#duration>')))).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#ageAbove>'(_, literal('P80Y', type('<http://www.w3.org/2001/XMLSchema#duration>')))), '<https://eyereasoner.github.io/ns#ageAbove>'(patH, literal('P80Y', type('<http://www.w3.org/2001/XMLSchema#duration>'))), true).
