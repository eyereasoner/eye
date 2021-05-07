% See https://en.wikipedia.org/wiki/Prime_number

:- use_module(library(between)).

primerange(A, B, L) :-
    findall(I, (between(A, B, I), prime(I)), L).

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
case(primerange(0, 100, _)).
case(primerange(1000000, 1000100, _)).

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
