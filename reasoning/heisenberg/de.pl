% Disjunction elimination

:- dynamic(saying/2).
:- dynamic(not_saying/2).

% saying A implies saying C
saying(S,'A') => saying(S,'C').
not_saying(S,'C') => not_saying(S,'A').

% saying B implies saying C
saying(S,'B') => saying(S,'C').
not_saying(S,'C') => not_saying(S,'B').

% saying A or saying B
not_saying(S,'A') => saying(S,'B').
not_saying(S,'B') => saying(S,'A').

% query
saying(_WHO,'C') => goal.

% assuming the negation of the query so that it can be discharged when the query succeeds
not_saying(someone,'C').
