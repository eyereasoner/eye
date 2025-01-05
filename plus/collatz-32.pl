% Collatz conjecture
% See https://en.wikipedia.org/wiki/Collatz_conjecture

'<urn:example:collatz>'(N, N) :-
    !.
'<urn:example:collatz>'(N0, N) :-
    (   N0 mod 2 =:= 0
    ->  N1 is N0//2
    ;   N1 is 3*N0+1
    ),
    '<urn:example:collatz>'(N1, N).

% query
(true :+ '<urn:example:collatz>'(N0, 1)) :- between(1, 32, N0).
