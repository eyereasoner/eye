% Socrates is a mortal

man(socrates).

mortal(S) :-
    man(S).

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
