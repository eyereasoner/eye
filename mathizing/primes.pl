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
case(findall(I, (between(2021, 2050, I), prime(I)), _)).

test :-
    forall(case(A), (A, writeln(A))),
    halt.
