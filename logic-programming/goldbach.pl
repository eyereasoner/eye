% See https://en.wikipedia.org/wiki/Goldbach%27s_conjecture
% every positive even number greater than 2 is the sum of two prime numbers
% Code taken from https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

'<urn:example:goldbach>'(4, [2, 2]).
'<urn:example:goldbach>'(N, L) :-
    N mod 2 =:= 0,
    N > 4,
    goldbach(N, L, 3).

goldbach(N, [P, Q], P) :-
    Q is N-P,
    is_prime(Q),
    !.
goldbach(N, L, P) :-
    P < N,
    next_prime(P, P1),
    goldbach(N, L, P1).

next_prime(P, P1) :-
    P1 is P+2,
    is_prime(P1),
    !.
next_prime(P, P1) :-
    P2 is P+2,
    next_prime(P2, P1).

is_prime(2).
is_prime(3).
is_prime(P) :-
    integer(P),
    P > 3,
    P mod 2 =\= 0,
    \+has_factor(P, 3).

has_factor(N, L) :-
    N mod L =:= 0.
has_factor(N, L) :-
    L*L < N,
    L2 is L+2,
    has_factor(N, L2).

% query
(true :+ '<urn:example:goldbach>'(N, [_, _])) :-
    between(1234567890, 1234567990, N),
    N mod 2 =:= 0.
