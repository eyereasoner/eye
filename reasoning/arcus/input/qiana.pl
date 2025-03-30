% Qiana examples
% See https://github.com/dig-team/Qiana

:- op(1200, xfx, :+).

% says(Einstein, not (forall x: glitters(x) => gold(x)))
'urn:example:says'('urn:example:Einstein', ('urn:example:notNecessarilyA'(X, 'urn:example:gold') :+ 'urn:example:glitter'(X))).

% example
'urn:example:glitter'('urn:example:northStar').

% forall phi, x: says(Einstein, phi) => believes(x, phi)
'urn:example:believes'(_, Phi) :- 'urn:example:says'('urn:example:Einstein', Phi).

% forall phi: says(Einstein, phi) => phi
Phi :+ 'urn:example:says'('urn:example:Einstein', Phi).

% queries
true :+ 'urn:example:believes'('urn:example:Fabian', _).
true :+ 'urn:example:notNecessarilyA'(_, _).
