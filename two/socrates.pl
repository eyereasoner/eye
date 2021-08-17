% Socrates is a mortal

namespace(rdfs,'http://www.w3.org/2000/01/rdf-schema#').
namespace(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
namespace(that,'http://josd.github.io/eye/two/sample-ns#').

rdfs-subClassOf(that-man,that-mortal).

rdf-type(that-socrates,that-man).
rdf-type(S,B) :-
    rdfs-subClassOf(A,B),
    rdf-type(S,A).

% test cases
case(rdf-type(that-socrates,that-mortal)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
