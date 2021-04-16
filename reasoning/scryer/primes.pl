% See https://en.wikipedia.org/wiki/Prime_number

:- use_module(library(between)).

prime(2).
prime(3).
prime(P) :-
    P > 3,
    P mod 2 =\= 0,
    \+factor(P, 3).

factor(N, L) :-
    N mod L =:= 0.
factor(N, L) :-
    L*L < N,
    L2 is L+2,
    factor(N, L2).

% test cases
case(prime(I)) :-
    between(0, 100, I).
case(prime(I)) :-
    between(1000, 1100, I).
case(prime(I)) :-
    between(1000000, 1000100, I).
case(prime(I)) :-
    between(1000000000, 100000100, I).

test :-
    case(A),
    A,
    write('[ :scryer-statement "'),
    write(A),
    write('"].'),
    nl,
    fail.
test :-
    halt.
