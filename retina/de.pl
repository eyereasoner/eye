% Disjunction elimination test case using negation predicates

:- initialization(main).

:- dynamic(saying/2).
:- dynamic(not_saying/2).

main :-
    % assuming the negation of the query so that it can be discharged when the query succeeds
    assertz(not_saying(sk_0,ccc)),
    saying(S,ccc),
    S = sk_0,
    retract(not_saying(sk_0,ccc)),
    write('true.'),
    nl.

% saying ccc if saying aaa or saying bbb
saying(S,ccc) :-
    saying(S,aaa).
saying(S,ccc) :-
    saying(S,bbb).
% saying aaa or saying bbb
saying(S,bbb) :-
    not_saying(S,aaa).
saying(S,aaa) :-
    not_saying(S,bbb).

% not saying aaa if not saying ccc
not_saying(S,aaa) :-
    not_saying(S,ccc).
% not saying bbb if not saying ccc
not_saying(S,bbb) :-
    not_saying(S,ccc).
