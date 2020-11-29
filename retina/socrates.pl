% Socrates Inference

:- initialization(main).

:- dynamic(type/2).

main :-
    [retina],
    retina,
    findall(type(Subject,Class),type(Subject,Class),All),
    All = [type(socrates,man),type(socrates,mortal)],
    write('true.'),
    nl.

type(socrates,man).
subclass_of(man,mortal).

implies((subclass_of(A,B),type(Subject,A)),type(Subject,B)).
