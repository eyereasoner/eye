% Diamond Property Equality
% original version at http://www.ii.uib.no/~bezem/GL/dpe.in
% DP(r) = DP(re), i.e. the diamond property is preserved under reflexive closure

:- op(1200, xfx, :+).

re(a, b), re(a, c) :+ true.

% equality axioms
e(A, A) :+ re(A, _).
e(A, A) :+ re(_, A).

e(B, A) :+ e(A, B).
not_e(B, A) :+ not_e(A, B).

not_e(A, B) :+ not_re(A, C), re(B, C).
not_e(B, C) :+ e(A, B), not_re(A, C).

% basic facts on re
re(A, B) :+ e(A, B).
re(A, B) :+ r(A, B).

r(A, B) :+ re(A, B), not_e(A, B).
e(A, B) :+ re(A, B), not_r(A, B).

% DP
r(B, D), r(C, D) :+ r(A, B), r(A, C).

% assuming the negation of the query so that it can be discharged when the query succeeds
not_re(c, A) :+ re(b, A).
not_re(b, A) :+ re(c, A).

% query
true :+ re(b, A), re(c, A).
