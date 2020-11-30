% Retina to support controlled chaining

:- initialization(test).

:- op(1150,xfx,'-:').

:- dynamic('-:'/2).
:- dynamic(brake/0).
:- dynamic(label/1).
:- dynamic(goal/0).

test :-
    \+(_ -: _),
    write('true.'),
    nl.
test.

retina :-
    (   (Prem -: Conc),
        call(Prem),
        \+call(Conc),
        labelvars(Conc),
        (   Conc = goal
        ->  true
        ;   astep(Conc),
            retract(brake),
            fail
        )
    ;   brake,
        !
    ;   assertz(brake),
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
    asserta(A),
    !,
    astep(B).
astep(A) :-
    asserta(A).
