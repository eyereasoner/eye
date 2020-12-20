% Socrates is a mortal

:- initialization(test).

:- op(1150,xfx,-:).

:- dynamic((-:)/2).
:- dynamic(type/2).
:- dynamic(goal/0).
:- dynamic(label/1).

test :-
    retina,
    type(socrates,mortal),
    write('true.'),
    nl.

type(socrates,man).
subclass_of(man,mortal).

subclass_of(A,B),type(Subject,A) -: type(Subject,B).

% retina to support controlled chaining
retina :-
    (Prem -: Conc),
    call(Prem),
    \+call(Conc),
    (   Conc = goal
    ->  !
    ;   labelvars(Conc),
        astep(Conc),
        retract(goal),
        fail
    ).
retina :-
    (   goal
    ->  !
    ;   assertz(goal),
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
