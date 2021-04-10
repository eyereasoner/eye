% See https://en.wikipedia.org/wiki/Prime_number

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
    writeln('# running primes.pl'),
    case(A),
    A,
    writeln(A),
    fail.
test :-
    nl,
    halt.
