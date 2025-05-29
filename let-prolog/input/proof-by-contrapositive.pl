% Proof by contrapositive
% See https://en.wikipedia.org/wiki/Contraposition#Proof_by_contrapositive

:- op(1200, xfx, :+).

% the ground is not wet
false :+ ascribed(ground, wet).

% if it is raining, then the ground is wet
ascribed(ground, wet) :+
    ascribed(it, raining).

% proof by contrapositive
(false :+ P) :+ (C :+ P), (false :+ C).

% query
true :+ (false :+ _).
