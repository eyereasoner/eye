% Diamond Property Equality
% DP(r) -: DP(re), i.e. the diamond property is preserved under reflexive closure
% Original code from http://www.ii.uib.no/~bezem/GL/dpe.in

:- dynamic(dom/1).
:- dynamic(e/2).
:- dynamic(r/2).
:- dynamic(re/2).

% facts
true -: dom(a),dom(b),dom(c).
true -: re(a,b),re(a,c).

% equality axioms
dom(X) -: e(X,X).

e(X,Y) -: e(Y,X).
neg(e(Y,X)) -: neg(e(X,Y)).

e(X,Y),re(Y,Z) -: re(X,Z).
neg(re(X,Z)),re(Y,Z) -: neg(e(X,Y)).
e(X,Y),neg(re(X,Z)) -: neg(e(Y,Z)).

% basic facts on re
e(X,Y) -: re(X,Y).
neg(re(X,Y)) -: neg(e(X,Y)).

r(X,Y) -: re(X,Y).
neg(re(X,Y)) -: neg(r(X,Y)).

re(X,Y),neg(e(X,Y)) -: r(X,Y).
neg(r(X,Y)),neg(e(X,Y)) -: neg(re(X,Y)).
re(X,Y),neg(r(X,Y)) -: e(X,Y).

% DP
r(X,Y),r(X,Z) -: dom(U),r(Y,U),r(Z,U).

% query implies goal
re(b,X),re(c,X) -: goal.

% assuming the negation of the query so that it can be discharged when the query succeeds
re(b,X) -: neg(re(c,X)).
re(c,X) -: neg(re(b,X)).

% a single answer is fine
limited_answer(1).
