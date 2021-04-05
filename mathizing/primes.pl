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
    between(1900, 2050, I).

test :-
    findall(_, (case(A), A, writeln(A)), _),
    halt.
