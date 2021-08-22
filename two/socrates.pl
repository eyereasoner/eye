% Socrates is a mortal

human(socrates).

mortal(S) :-
    human(S).

% test cases
case(mortal(_IND)).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
