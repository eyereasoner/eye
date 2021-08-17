% Socrates is a mortal

web_nsp(rdfs_,'http://www.w3.org/2000/01/rdf_schema#').
web_nsp(rdf_,'http://www.w3.org/1999/02/22-rdf_syntax-ns#').
web_nsp(etc_,'http://josd.github.io/eye/two/cases#').

rdfs_subClassOf(etc_man,etc_mortal).

rdf_type(etc_socrates,etc_man).
rdf_type(S,B) :-
    rdfs_subClassOf(A,B),
    rdf_type(S,A).

% test cases
case(rdf_type(etc_socrates,etc_mortal)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
