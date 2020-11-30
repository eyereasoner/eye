% Diamond Property Equality
% DP(r) -: DP(re), i.e. the diamond property is preserved under reflexive closure
% original version at http://www.ii.uib.no/~bezem/GL/dpe.in

:- initialization(main).

:- op(1150,xfx,'-:').

:- dynamic(dom/1).
:- dynamic(e/2).
:- dynamic(r/2).
:- dynamic(re/2).
:- dynamic(not_e/2).
:- dynamic(not_r/2).
:- dynamic(not_re/2).

main :-
    [retina],
    % assuming the negation of the query so that it can be discharged when the query succeeds
    assertz(re(b,X) -: not_re(c,X)),
    assertz(re(c,X) -: not_re(b,X)),
    % query
    assertz((re(b,X),re(c,X) -: goal)),
    retina,
    retract(re(b,X) -: not_re(c,X)),
    retract(re(c,X) -: not_re(b,X)),
    write('true.'),
    nl.

dom(a).
dom(b).
dom(c).

re(a,b).
re(a,c).

% equality axioms
dom(X) -: e(X,X).

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
r(X,Y),r(X,Z),not_e(Y,Z) -: dom(U),r(Y,U),r(Z,U).
%r(X,Y),r(X,Z) -: dom(U),r(Y,U),r(Z,U).
