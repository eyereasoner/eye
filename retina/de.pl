% disjunction elimination test case using negation predicates

:- initialization(main).

main :-
    saying(S,c),
    S = anon,
    write('true.'),
    nl.

% assuming the negation of the query so that it can be discharged when the query succeeds
not_saying(anon,c).

% saying A means saying C
not_saying(S,a) :-
    not_saying(S,c).
% saying B means saying C
not_saying(S,b) :-
    not_saying(S,c).

% saying A means saying C
saying(S,c) :-
    saying(S,a).
% saying B means saying C
saying(S,c) :-
    saying(S,b).
% saying A or saying B
saying(S,b) :-
    not_saying(S,a).
saying(S,a) :-
    not_saying(S,b).
