% Goldbach's_conjecture

:- op(1200, xfx, :+).

'<https://eyereasoner.github.io/ns#goldbach>'(4, [2, 2]).
'<https://eyereasoner.github.io/ns#goldbach>'(N, L) :-
    0 =:= N rem 2,
    N > 4,
    '<https://eyereasoner.github.io/ns#goldb>'(N, L, 3).

'<https://eyereasoner.github.io/ns#goldb>'(N, [P, Q], P) :-
    Q is N-P,
    '<https://eyereasoner.github.io/ns#is_prime>'(Q),
    !.
'<https://eyereasoner.github.io/ns#goldb>'(N, L, P) :-
    P < N,
    '<https://eyereasoner.github.io/ns#next_prime>'(P, P1),
    '<https://eyereasoner.github.io/ns#goldb>'(N, L, P1).

'<https://eyereasoner.github.io/ns#next_prime>'(P, P1) :-
    P1 is P+2,
    '<https://eyereasoner.github.io/ns#is_prime>'(P1),
    !.
'<https://eyereasoner.github.io/ns#next_prime>'(P, P1) :-
    P2 is P+2,
    '<https://eyereasoner.github.io/ns#next_prime>'(P2, P1).

'<https://eyereasoner.github.io/ns#is_prime>'(2).
'<https://eyereasoner.github.io/ns#is_prime>'(3).
'<https://eyereasoner.github.io/ns#is_prime>'(P) :-
    P > 3,
    1 =:= P rem 2,
    \+'<https://eyereasoner.github.io/ns#has_factor>'(P, 3).

'<https://eyereasoner.github.io/ns#has_factor>'(N, L) :-
    0 =:= N rem L,
    !.
'<https://eyereasoner.github.io/ns#has_factor>'(N, L) :-
    L*L <  N,
    L2 is L+2,
    '<https://eyereasoner.github.io/ns#has_factor>'(N, L2).

% query
(true :+ \+'<https://eyereasoner.github.io/ns#goldbach>'(N, [_, _])) :-
    between(2, 2, N).
