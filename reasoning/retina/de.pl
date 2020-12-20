% Disjunction elimination test case using negation predicates

:- initialization(test).

:- op(1150,xfx,-:).

:- dynamic((-:)/2).
:- dynamic(saying/2).
:- dynamic(not_saying/2).
:- dynamic(goal/0).
:- dynamic(label/1).

test :-
    % query implies goal
    assertz((saying(_,'C') -: goal)),
    % assuming the negation of the query so that it can be discharged when the query succeeds
    assertz(not_saying(sk_0,'C')),
    retina,
    write('true.'),
    nl.

% saying A implies saying C
saying(S,'A') -: saying(S,'C').
not_saying(S,'C') -: not_saying(S,'A').

% saying B implies saying C
saying(S,'B') -: saying(S,'C').
not_saying(S,'C') -: not_saying(S,'B').

% saying A or saying B
not_saying(S,'A') -: saying(S,'B').
not_saying(S,'B') -: saying(S,'A').

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
    ->  asserta(A)
    ;   true
    ).
