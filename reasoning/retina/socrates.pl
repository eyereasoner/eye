% Socrates is a mortal

:- initialization(test).

:- op(1150,xfx,'-:').

:- dynamic(type/2).

test :-
    [retina],
    retina,
    type(socrates,mortal),
    write('true.'),
    nl.

type(socrates,man).
subclass_of(man,mortal).

subclass_of(A,B),type(Subject,A) -: type(Subject,B).
