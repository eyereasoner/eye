% Diamond Property Equality
% original version at http://www.ii.uib.no/~bezem/GL/dpe.in
% DP(r) = DP(re), i.e. the diamond property is preserved under reflexive closure

:- op(1200, xfx, :+).

'urn:example:re'(a, b), 'urn:example:re'(a, c) :+ true.

% equality axioms
'urn:example:e'(A, A) :+ 'urn:example:re'(A, _).
'urn:example:e'(A, A) :+ 'urn:example:re'(_, A).

'urn:example:e'(B, A) :+ 'urn:example:e'(A, B).
'urn:example:not_e'(B, A) :+ 'urn:example:not_e'(A, B).

'urn:example:not_e'(A, B) :+ 'urn:example:not_re'(A, C), 'urn:example:re'(B, C).
'urn:example:not_e'(B, C) :+ 'urn:example:e'(A, B), 'urn:example:not_re'(A, C).

% basic facts on re
'urn:example:re'(A, B) :+ 'urn:example:e'(A, B).
'urn:example:re'(A, B) :+ 'urn:example:r'(A, B).

'urn:example:r'(A, B) :+ 'urn:example:re'(A, B), 'urn:example:not_e'(A, B).
'urn:example:e'(A, B) :+ 'urn:example:re'(A, B), 'urn:example:not_r'(A, B).

% DP
'urn:example:r'(B, D), 'urn:example:r'(C, D) :+ 'urn:example:r'(A, B), 'urn:example:r'(A, C).

% assuming the negation of the query so that it can be discharged when the query succeeds
'urn:example:not_re'(c, A) :+ 'urn:example:re'(b, A).
'urn:example:not_re'(b, A) :+ 'urn:example:re'(c, A).

% query
true :+ 'urn:example:re'(b, A), 'urn:example:re'(c, A).
