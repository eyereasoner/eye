% Socrates is a mortal

:- initialization(test).

:- op(1150,xfx,-:).

:- dynamic((-:)/2).
:- dynamic(type/2).
:- dynamic(goal/0).
:- dynamic(label/1).
:- dynamic(lus/1).

test :-
    retina,
    type(socrates,mortal),
    write('true.'),
    nl.

type(socrates,man).
subclass_of(man,mortal).

subclass_of(A,B),type(Subject,A) -: type(Subject,B).

% retina to support forward chaining
retina :-
    (Prem -: Conc),
    call(Prem),
    \+call(Conc),
    (   Conc = goal
    ->  !
    ;   labelvars(Conc),
        assertz(lus(Conc)),
        retract(goal),
        fail
    ).
retina :-
    (   goal
    ->  !
    ;   assertz(goal),
        forall(retract(lus(Conc)),astep(Conc)),
        retina
    ).

labelvars(Term) :-
    (   label(Current)
    ->  true
    ;   Current = 0
    ),
    numbervars(Term,Current,Next),
    retractall(label(_)),
    assertz(label(Next)).

astep((A,B)) :-
    !,
    astep(A),
    astep(B).
astep(A) :-
    (   \+call(A)
    ->  assertz(A)
    ;   true
    ).
