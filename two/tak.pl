% Takeuchi function

web_nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_tak(X,Y,Z,Z) :-
    X =< Y,
    !.

etc_tak(X,Y,Z,A) :-
    X1 is X-1,
    etc_tak(X1,Y,Z,A1),
    Y1 is Y-1,
    etc_tak(Y1,Z,X,A2),
    Z1 is Z-1,
    etc_tak(Z1,X,Y,A3),
    etc_tak(A1,A2,A3,A).

% test cases
case(etc_tak(34,13,8,_A)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
