% --------------------------------------------------------------------------
% Retina performs controlled chaining and Euler path detection -- Jos De Roo
% --------------------------------------------------------------------------

% Controlled chaining is backward chaining for rules using HEAD :- BODY and
% forward chaining for rules using BODY -: HEAD.
%
% Euler paths are roughly "don't step in your own steps" which is inspired by
% what Leonhard Euler discovered in 1736 for the Königsberg Bridge Problem.
% Retina sees the rule BODY -: HEAD as HEAD & NOT(BODY) => HEAD.


:- op(1150,xfx,-:).

:- dynamic((-:)/2).
:- dynamic(goal/0).
:- dynamic(label/1).
:- dynamic(lus/1).

retina :-
    (Prem -: Conc),
    Prem,
    \+ Conc,
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
    (   \+ A
    ->  assertz(A)
    ;   true
    ).
