% Collatz conjecture
% See https://en.wikipedia.org/wiki/Collatz_conjecture

collatz(N, L) :-
    collatz(N, 1, L).

collatz(N, N, [N]) :-
    !.
collatz(N0, N, [N0|J]) :-
    (   0 =:= N0 rem 2
    ->  N1 is N0 // 2
    ;   N1 is 3 * N0 + 1
    ),
    collatz(N1, N, J).

collatz(A, B, C, D) :-
    between(A, B, C),
    collatz(C, D).

% query
true :+ collatz(1, 1000, _, _).
