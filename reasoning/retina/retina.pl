% Retina to support controlled chaining

:- initialization(test).

:- op(1150,xfx,'-:').   % logical implication

:- dynamic('-:'/2).
:- dynamic(goal/0).
:- dynamic(label/1).

test :-
    \+(_ -: _),
    write('true.'),
    nl.
test.

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
    assertz(A),
    !,
    astep(B).
astep(A) :-
    asserta(A).
