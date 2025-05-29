% Qiana examples
% See https://github.com/dig-team/Qiana

:- op(1200, xfx, :+).

% says(Einstein, not (forall x: glitters(x) => gold(x)))
says('Einstein', (notNecessarilyA(X, gold) :+ glitter(X))).

% example
glitter(northStar).

% forall phi, x: says(Einstein, phi) => believes(x, phi)
believes(_, Phi) :- says('Einstein', Phi).

% forall phi: says(Einstein, phi) => phi
Phi :+ says('Einstein', Phi).

% queries
true :+ believes('Fabian', _).
true :+ notNecessarilyA(_, _).
