% proof by cases
% See https://en.wikipedia.org/wiki/Disjunction_elimination

% water is an inorganic compound
'<urn:example:InorganicCompound>'('<urn:example:water>').

% water is solid or liquid or gas
'<urn:example:allPossibleCases>'([A],
        [
            '<urn:example:is>'(A, '<urn:example:solid>'),
            '<urn:example:is>'(A, '<urn:example:liquid>'),
            '<urn:example:is>'(A, '<urn:example:gas>')
        ]
    ) :+ '<urn:example:InorganicCompound>'(A).

% solid, liquid and gas things are observable
'<urn:example:is>'(A, '<urn:example:observable>') :+
    '<urn:example:is>'(A, '<urn:example:solid>').

'<urn:example:is>'(A, '<urn:example:observable>') :+
    '<urn:example:is>'(A, '<urn:example:liquid>').

'<urn:example:is>'(A, '<urn:example:observable>') :+
    '<urn:example:is>'(A, '<urn:example:gas>').

% proof by cases
'<urn:example:is>'(A, '<urn:example:observable>') :+
    '<urn:example:allPossibleCases>'([A], B),
    forall(
        member('<urn:example:is>'(A, C), B),
        '<urn:example:is>'(A, '<urn:example:observable>') :+ '<urn:example:is>'(A, C)
    ).

% query
true :+ '<urn:example:is>'(_, _).
