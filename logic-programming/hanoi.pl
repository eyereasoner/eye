% Towers of Hanoi
% See https://en.wikipedia.org/wiki/Tower_of_Hanoi

:- op(1200, xfx, :+).

'<urn:example:move>'(0, [_, _, _]) :-
    !.
'<urn:example:move>'(N, [A, B, C]) :-
    M is N-1,
    '<urn:example:move>'(M, [A, C, B]),
    '<urn:example:move>'(M, [C, B, A]).

% query
true :+ '<urn:example:move>'(14, [left, centre, right]).
