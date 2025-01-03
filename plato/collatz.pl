% Collatz conjecture
% See https://en.wikipedia.org/wiki/Collatz_conjecture

'<urn:example:collatz>'(N, N, []) :-
    !.
'<urn:example:collatz>'(N0, N, [N1|L]) :-
    (   N0 mod 2 =:= 0
    ->  N1 is N0//2
    ;   N1 is 3*N0+1
    ),
    '<urn:example:collatz>'(N1, N, L).

% query
(true :+ '<urn:example:collatz>'(N0, 1, _)) :- between(1, 3000, N0).
