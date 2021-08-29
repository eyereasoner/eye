% Socrates is a mortal

wrapper(el,'https://josd.github.io/eye/lateral/ns#').

man(socrates).

mortal(S) :-
    man(S).

% test cases
case(wrapper(_NS,_P)).
case(mortal(_IND)).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
