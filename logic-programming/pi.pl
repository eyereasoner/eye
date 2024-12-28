% Calculate pi using Nilakantha series
% See http://www.wikihow.com/Calculate-Pi

:- op(1200, xfx, :+).

'<urn:example:pi>'(A, B) :-
    pi(1, A, 0, C, 1),
    B is 3+4*C.

pi(A, A, B, B, _) :-
    !.
pi(A, B, C, D, E) :-
    F is A+1,
    L is C+E/(2*A*(2*A+1)*(2*A+2)),
    M is -E,
    pi(F, B, L, D, M).

% query
true :+ '<urn:example:pi>'(100000, _).
