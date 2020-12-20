% Diamond Property Equality
% DP(r) -: DP(re), i.e. the diamond property is preserved under reflexive closure
% Original version at http://www.ii.uib.no/~bezem/GL/dpe.in

:- initialization(test).

:- op(1150,xfx,-:).

:- dynamic((-:)/2).
:- dynamic(dom/1).
:- dynamic(e/2).
:- dynamic(r/2).
:- dynamic(re/2).
:- dynamic(not_e/2).
:- dynamic(not_r/2).
:- dynamic(not_re/2).
:- dynamic(goal/0).
:- dynamic(label/1).

test :-
    % query implies goal
    assertz((re(b,X),re(c,X) -: goal)),
    % assuming the negation of the query so that it can be discharged when the query succeeds
    assertz((re(b,X) -: not_re(c,X))),
    assertz((re(c,X) -: not_re(b,X))),
    retina,
    write('true.'),
    nl.

dom(a).
dom(b).
dom(c).

re(a,b).
re(a,c).

% equality axioms
%dom(X) -: e(X,X).

e(X,Y) -: e(Y,X).
not_e(Y,X) -: not_e(X,Y).

e(X,Y),re(Y,Z) -: re(X,Z).
not_re(X,Z),re(Y,Z) -: not_e(X,Y).
e(X,Y),not_re(X,Z) -: not_e(Y,Z).

% basic facts on re
e(X,Y) -: re(X,Y).
not_re(X,Y) -: not_e(X,Y).

r(X,Y) -: re(X,Y).
not_re(X,Y) -: not_r(X,Y).

re(X,Y),not_e(X,Y) -: r(X,Y).
not_r(X,Y),not_e(X,Y) -: not_re(X,Y).
re(X,Y),not_r(X,Y) -: e(X,Y).

% DP
r(X,Y),r(X,Z) -: dom(U),r(Y,U),r(Z,U).

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
