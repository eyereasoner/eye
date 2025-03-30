% Collatz conjecture
% See https://en.wikipedia.org/wiki/Collatz_conjecture

:- op(1200, xfx, :+).

'urn:example:collatz'(N, L) :-
    collatz(N, 1, L).

collatz(N, N, [N]) :-
    !.
collatz(N0, N, [N0|J]) :-
    (   0 =:= N0 rem 2
    ->  N1 is N0 // 2
    ;   N1 is 3 * N0 + 1
    ),
    collatz(N1, N, J).

% query
(true :+ 'urn:example:collatz'(N, _)) :-
    between(1, 1000, N).
