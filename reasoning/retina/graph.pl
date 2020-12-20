% Traversing graph paths

:- initialization(test).

:- op(1150,xfx,-:).

:- dynamic((-:)/2).
:- dynamic(path/2).
:- dynamic(goal/0).
:- dynamic(label/1).

test :-
    retina,
    path(paris,nantes),
    write('true.'),
    nl.

oneway(paris,orleans).
oneway(paris,chartres).
oneway(paris,amiens).
oneway(orleans,blois).
oneway(orleans,bourges).
oneway(blois,tours).
oneway(chartres,lemans).
oneway(lemans,angers).
oneway(lemans,tours).
oneway(angers,nantes).

oneway(A,B) -: path(A,B).
path(A,B),path(B,C) -: path(A,C).

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
