% Socrates is a mortal

isDefinedBy(subClassOf/2,'http://www.w3.org/2000/01/rdf-schema#').
isDefinedBy(type/2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

subClassOf(man,mortal).

type(socrates,man).
type(S,B) :-
    subClassOf(A,B),
    type(S,A).

% test cases
case(type(socrates,mortal)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
