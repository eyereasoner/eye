% Goldbach's_conjecture
% See https://en.wikipedia.org/wiki/Goldbach%27s_conjecture:
% every positive even number greater than 2 is the sum of two prime numbers

goldbach(4, [2, 2]).
goldbach(N, L) :-
    0 =:= N rem 2,
    N > 4,
    goldb(N, L, 3).

goldb(N, [P, Q], P) :-
    Q is N-P,
    is_prime(Q),
    !.
goldb(N, L, P) :-
    P < N,
    next_prime(P, P1),
    goldb(N, L, P1).

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
    P > 3,
    1 =:= P rem 2,
    \+has_factor(P, 3).

has_factor(N, L) :-
    0 =:= N rem L,
    !.
has_factor(N, L) :-
    L*L <  N,
    L2 is L+2,
    has_factor(N, L2).

% goldbach iterations
goldbach(A, B, C, D, E) :-
    between(A, B, C),
    D is 2^C,
    goldbach(D, E).

% query
true :+ goldbach(2, 35, _, _, _).
