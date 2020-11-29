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

retina :-
    (   implies(Prem,Conc),
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

label(0).

labelvars(A) :-
    label(B),
    labelvars(A,B,C),
    retractall(label(_)),
    assertz(label(C)).

labelvars(A,B,C) :-
    var(A),
    !,
    number_chars(B,D),
    atom_chars(E,D),
    atom_concat('sk_',E,A),
    C is B+1.
labelvars(A,B,B) :-
    atomic(A),
    !.
labelvars((A,B),C,D) :-
    !,
    labelvars(A,C,E),
    labelvars(B,E,D).
labelvars([A|B],C,D) :-
    !,
    labelvars(A,C,E),
    labelvars(B,E,D).
labelvars(A,B,C) :-
    nonvar(A),
    functor(A,_,D),
    labelvars(0,D,A,B,C).

labelvars(A,A,_,B,B) :-
    !.
labelvars(A,B,C,D,E) :-
    F is A+1,
    arg(F,C,G),
    labelvars(G,D,H),
    labelvars(F,B,C,H,E).

astep((A,B)) :-
    assertz(A),
    !,
    astep(B).
astep(A) :-
    assertz(A).
