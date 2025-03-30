% See https://en.wikipedia.org/wiki/Prime_number

:- op(1200, xfx, :+).

'urn:example:primerange'(A, B, L) :-
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

% Eulers totient function
% See https://en.wikipedia.org/wiki/eye%27s_totient_function
% Original code from https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

% Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime factors (and their
% multiplicities) of a given number m. Then phi(m) can be calculated
% with the following formula:
%   phi(m) = (p1-1)*p1**(m1-1)*(p2-1)*p2**(m2-1)*(p3-1)*p3**(m3-1)*...

% totient(N, Phi) :- Phi is the value of Eulers totient function
% for the argument N.
%   (integer, integer) (+, ?)

'urn:example:totient'(N, Phi) :-
    prime_factors_mult(N, L),
    to_phi(L, Phi).

to_phi([], 1).
to_phi([[F, 1]|L], Phi) :-
    !,
    to_phi(L, Phi1),
    Phi is Phi1*(F-1).
to_phi([[F, M]|L], Phi) :-
    M > 1,
    M1 is M-1,
    to_phi([[F, M1]|L], Phi1),
    Phi is Phi1*F.

% prime_factors_mult(N, L) :- L is the list of prime factors of N. It is
% composed of terms [F, M] where F is a prime factor and M its multiplicity.
%   (integer, list) (+, ?)

prime_factors_mult(N, L) :-
    N > 0,
    prime_factors_mult(N, L, 2).

% prime_factors_mult(N, L, K) :- L is the list of prime factors of N. It is
% known that N does not have any prime factors less than K.

prime_factors_mult(1, [], _) :-
    !.
prime_factors_mult(N, [[F, M]|L], F) :-
    divide(N, F, M, R),
    !,
    next_factor(R, F, NF),
    prime_factors_mult(R, L, NF).
prime_factors_mult(N, L, F) :-
    !,
    next_factor(N, F, NF),
    prime_factors_mult(N, L, NF).

% prime_factors(N, L) :- N is the list of prime factors of N.
%   (integer, list) (+, ?)

prime_factors(N, L) :-
    N > 0,
    prime_factors(N, L, 2).

% prime_factors(N, L, K) :- L is the list of prime factors of N. It is
% known that N does not have any prime factors less than K.

prime_factors(1, [], _) :-
    !.
prime_factors(N, [F|L], F) :-
   R is N//F,
   N =:= R*F,
   !,
   prime_factors(R, L, F).
prime_factors(N, L, F) :-
   next_factor(N, F, NF),
   prime_factors(N, L, NF).

% next_factor(N, F, NF) :- when calculating the prime factors of N
% and if F does not divide N then NF is the next larger candidate to
% be a factor of N.

next_factor(_, 2, 3) :-
    !.
next_factor(N, F, NF) :-
    F*F < N,
    !,
    NF is F+2.
next_factor(N, _, N).

% divide(N, F, M, R) :- N = R * F**M, M >= 1, and F is not a factor of R.
%   (integer, integer, integer, integer) (+, +, -, -)

divide(N, F, M, R) :-
    divi(N, F, M, R, 0),
    M > 0.

divi(N, F, M, R, K) :-
    S is N//F,
    N =:= S*F,
    !,
    K1 is K+1,
    divi(S, F, M, R, K1).
divi(N, _, M, N, M).

% query
true :+ 'urn:example:primerange'(0, 100, _).
true :+ 'urn:example:primerange'(1000000, 1000100, _).
true :+ 'urn:example:totient'(271, _).
true :+ 'urn:example:totient'(2718281, _).
true :+ 'urn:example:totient'(27182818284, _).
true :+ 'urn:example:totient'(271828182845904, _).
