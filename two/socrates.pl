% Socrates is a mortal

context(subClassOf,'http://www.w3.org/2000/01/rdf-schema#subClassOf').
context(type,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type').

subClassOf(man,mortal).

type(socrates,man).
type(S,B) :-
    subClassOf(A,B),
    type(S,A).

% test cases
case(context(_PRED,_URI)).
case(subClassOf(_SUBCLASS,_CLASS)).
case(type(_IND,_CLASS)).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
