% Collatz conjecture
% See https://en.wikipedia.org/wiki/Collatz_conjecture

'<urn:example:collatz>'(N, N, 1) :-
    !.
'<urn:example:collatz>'(N0, N, M1) :-
    (   N0 mod 2 =:= 0
    ->  N1 is N0//2
    ;   N1 is 3*N0+1
    ),
    '<urn:example:collatz>'(N1, N, M),
    M1 is M+1.

% query
(true :+ '<urn:example:collatz>'(N0, 1, _)) :- between(1, 256, N0).
