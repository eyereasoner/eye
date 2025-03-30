% Collatz conjecture

'<https://eyereasoner.github.io/ns#collatz>'(N, L) :-
    collatz(N, 1, L).

collatz(N, N, [N]) :-
    !.
collatz(N0, N, [N0|J]) :-
    (   0 =:= N0 rem 2
    ->  N1 is N0 // 2
    ;   N1 is 3 * N0 + 1
    ),
    collatz(N1, N, J).
