% Proof by contrapositive

:- op(1200, xfx, :+).

% the ground is not wet
false :+ '<urn:example:is>'('<urn:example:ground>', '<urn:example:wet>').

% if it is raining, then the ground is wet
'<urn:example:is>'('<urn:example:ground>', '<urn:example:wet>') :+
    '<urn:example:is>'('<urn:example:it>', '<urn:example:raining>').

% proof by contrapositive
(false :+ P) :+ (C :+ P), (false :+ C).

% query
true :+ (false :+ _).
