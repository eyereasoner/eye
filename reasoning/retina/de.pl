% Disjunction elimination test case using negation predicates

:- initialization(test).

:- op(1150,xfx,'-:').

:- dynamic(saying/2).
:- dynamic(not_saying/2).

test :-
    [retina],
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
