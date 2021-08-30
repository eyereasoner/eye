% Socrates is a mortal

told(rdf/1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
told(el/1,'https://josd.github.io/eye/lateral/ns#').

rdf(type(el(socrates),el(human))).

rdf(type(S,el(mortal))) :-
    rdf(type(S,el(human))).

% test cases
case(told(_NS,_P)).
case(rdf(type(_IND,el(mortal)))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
