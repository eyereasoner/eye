% Socrates is a mortal

webize(rdf/1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
webize(eye/1,'https://josd.github.io/eye/thinking/ns#').

rdf(type(eye(socrates),eye(human))).

rdf(type(S,eye(mortal))) :-
    rdf(type(S,eye(human))).

% test cases
case(webize(_NS,_P)).
case(rdf(type(_IND,eye(mortal)))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
