% Socrates is a mortal

namespace(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
namespace(rdfs,'http://www.w3.org/2000/01/rdf-schema#').

rdf_type(socrates,man).
rdfs_subClassOf(man,mortal).

rdf_type(S,B) :-
    rdfs_subClassOf(A,B),
    rdf_type(S,A).

% test cases
case(rdf_type(socrates,mortal)).

test :-
    case(A),
    A,
    write(A),
    write('.'),
    nl,
    fail.
test :-
    halt.
