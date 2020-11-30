% Retina to support controlled chaining

:- dynamic(implies/2).
:- dynamic(brake/0).
:- dynamic(label/1).
:- dynamic(goal/0).

:- initialization(test).

test :-
    \+ implies(_,_),
    write('true.'),
    nl.
test.

label(0).

retina :-
    (   implies(Prem,Conc),
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
    assertz(A),
    !,
    astep(B).
astep(A) :-
    assertz(A).
