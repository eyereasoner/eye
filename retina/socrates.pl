% Socrates is a mortal

:- initialization(main).

:- op(1150,xfx,'=>').

:- dynamic(type/2).

main :-
    [retina],
    retina,
    findall(type(Subject,Class),type(Subject,Class),All),
    sort(All,AllSorted),
    AllSorted = [type(socrates,man),type(socrates,mortal)],
    write('true.'),
    nl.

type(socrates,man).
subclass_of(man,mortal).

subclass_of(A,B),type(Subject,A) => type(Subject,B).
