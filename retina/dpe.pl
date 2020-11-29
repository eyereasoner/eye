% Diamond Property Equality
% original version at http://www.ii.uib.no/~bezem/GL/dpe.in
% DP(r) => DP(re), i.e. the diamond property is preserved under reflexive closure

:- initialization(main).

:- dynamic(e/2).
:- dynamic(r/2).
:- dynamic(re/2).
:- dynamic(not_e/2).
:- dynamic(not_r/2).
:- dynamic(not_re/2).

main :-
    [retina],
    % assuming the negation of the query so that it can be discharged when the query succeeds
    assertz(implies(re(b,U),not_re(c,U))),
    assertz(implies(re(c,V),not_re(b,V))),
    % query
    assertz(implies((re(b,X),re(c,X)),goal)),
    retina,
    retract(implies(re(b,U),not_re(c,U))),
    retract(implies(re(c,V),not_re(b,V))),
    write('true.'),
    nl.

re(a,b).
re(a,c).

% equality axioms
implies(re(X,_),e(X,X)).
implies(re(_,X),e(X,X)).


implies(e(X,Y),e(Y,X)).
implies(not_e(Y,X),not_e(X,Y)).

implies((e(X,Y),re(Y,Z)),re(X,Z)).
implies((not_re(X,Z),re(Y,Z)),not_e(X,Y)).
implies((e(X,Y),not_re(X,Z)),not_e(Y,Z)).

% basic facts on re
implies(e(X,Y),re(X,Y)).
implies(not_re(X,Y),not_e(X,Y)).

implies(r(X,Y),re(X,Y)).
implies(not_re(X,Y),not_r(X,Y)).

implies((re(X,Y),not_e(X,Y)),r(X,Y)).
implies((not_r(X,Y),not_e(X,Y)),not_re(X,Y)).
implies((re(X,Y),not_r(X,Y)),e(X,Y)).

% DP
implies((r(X,Y),r(X,Z),not_e(Y,Z)),(r(Y,U),r(Z,U))).
