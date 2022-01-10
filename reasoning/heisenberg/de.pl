% Disjunction elimination

:- dynamic(saying/2).

% saying A implies saying C
saying(S,"A") => saying(S,"C").
neg(saying(S,"C")) => neg(saying(S,"A")).

% saying B implies saying C
saying(S,"B") => saying(S,"C").
neg(saying(S,"C")) => neg(saying(S,"B")).

% saying A or saying B
neg(saying(S,"A")) => saying(S,"B").
neg(saying(S,"B")) => saying(S,"A").

% query implies goal
saying(_WHO,"C") => goal.

% assuming the negation of the query so that it can be discharged when the query succeeds
neg(saying(someone,"C")).
