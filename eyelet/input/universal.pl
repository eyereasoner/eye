% Examples of universal statements

:- op(1200, xfx, :+).

% Every x: type(x, Resource)
type(_, 'Resource').

% Everybody loves somebody who is lonely
loves(A, lonely(skolem(A))).

% query
true :+ type('Pat', 'Resource').
true :+ loves('Bob', _).
