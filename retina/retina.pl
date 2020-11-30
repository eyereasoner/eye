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

label(0).

retina :-
    (   (Prem -: Conc),
        call(Prem),
        \+call(Conc),
        label(Current),
        numbervars(Conc,Current,Next),
        retractall(label(_)),
        assertz(label(Next)),
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

astep((A,B)) :-
    asserta(A),
    !,
    astep(B).
astep(A) :-
    asserta(A).
