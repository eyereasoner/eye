% Socrates is a mortal

% subClassOf/2 comes from http://www.w3.org/2000/01/rdf-schema#subClassOf
subClassOf(man,mortal).

% type/2 comes from http://www.w3.org/1999/02/22-rdf-syntax-ns#type
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
    write('.'),
    nl,
    fail.
test :-
    halt.
