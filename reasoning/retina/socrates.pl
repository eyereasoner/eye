% Socrates is a mortal

:- initialization(test).

:- op(1150,xfx,-:).

:- dynamic((-:)/2).
:- dynamic(type/2).

test :-
    % query implies goal
    assertz((type(socrates,mortal) -: goal)),
    consult(retina),
    retina,
    write('true.'),
    nl.

type(socrates,man).
subclass_of(man,mortal).

subclass_of(A,B),type(Subject,A) -: type(Subject,B).
