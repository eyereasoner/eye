% Proof by cases

:- op(1200, xfx, :+).

:- dynamic('<urn:example:allPossibleCases>'/2).

% context
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, B) :- type(A, B).

% water is an inorganic compound
type('<urn:example:water>', '<urn:example:InorganicCompound>').

% proof by cases
type(A, '<urn:example:Observablee>') :-
    '<urn:example:allPossibleCases>'(A, B),
    forall(
        member(type(A, C), B),
        (type(A, '<urn:example:Observablee>') :+ type(A, C))
    ).

% water is solid or liquid or gas
'<urn:example:allPossibleCases>'(A,
        [
            type(A, '<urn:example:Solid>'),
            type(A, '<urn:example:Liquid>'),
            type(A, '<urn:example:Gas>')
        ]
    ) :+ type(A, '<urn:example:InorganicCompound>').

% solid, liquid and gas things are Observablee
type(A, '<urn:example:Observablee>') :+
    type(A, '<urn:example:Solid>').

type(A, '<urn:example:Observablee>') :+
    type(A, '<urn:example:Liquid>').

type(A, '<urn:example:Observablee>') :+
    type(A, '<urn:example:Gas>').

% query
true :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, _).
