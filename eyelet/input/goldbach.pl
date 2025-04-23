% Goldbach's_conjecture
% See https://en.wikipedia.org/wiki/Goldbach%27s_conjecture:
% every positive even number greater than 2 is the sum of two prime numbers

:- op(1200, xfx, :+).

'urn:example:goldbach'(4, [2, 2]).
'urn:example:goldbach'(N, L) :-
    0 =:= N rem 2,
    N > 4,
    'urn:example:goldb'(N, L, 3).

'urn:example:goldb'(N, [P, Q], P) :-
    Q is N-P,
    'urn:example:is_prime'(Q),
    !.
'urn:example:goldb'(N, L, P) :-
    P < N,
    'urn:example:next_prime'(P, P1),
    'urn:example:goldb'(N, L, P1).

'urn:example:next_prime'(P, P1) :-
    P1 is P+2,
    'urn:example:is_prime'(P1),
    !.
'urn:example:next_prime'(P, P1) :-
    P2 is P+2,
    'urn:example:next_prime'(P2, P1).

'urn:example:is_prime'(2).
'urn:example:is_prime'(3).
'urn:example:is_prime'(P) :-
    P > 3,
    1 =:= P rem 2,
    \+'urn:example:has_factor'(P, 3).

'urn:example:has_factor'(N, L) :-
    0 =:= N rem L,
    !.
'urn:example:has_factor'(N, L) :-
    L*L <  N,
    L2 is L+2,
    'urn:example:has_factor'(N, L2).

% query
(true :+ 'urn:example:goldbach'(N, [_, _])) :-
    between(2, 36, I),
    N is 2^I.
