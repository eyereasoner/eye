% Calculating the area of a polygon

:- op(1200, xfx, :+).

'<urn:example:area>'([_], 0).
'<urn:example:area>'([[A, B], [C, D]|E], F) :-
    '<urn:example:area>'([[C, D]|E], G),
    F is (A*D-B*C)/2+G.

% query
true :+ '<urn:example:area>'([[3, 2], [6, 2], [7, 6], [4, 6], [5, 5], [5, 3], [3, 2]], _).
