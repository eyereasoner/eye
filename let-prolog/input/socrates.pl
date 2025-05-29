% Socrates is a mortal

:- op(1200, xfx, :+).

:- dynamic(type/2).

type('Socrates', 'Man').

type(X, 'Mortal') :+
    type(X, 'Man').

% query
true :+ type(_, _).
