:- op(1200, xfx, :+).

% answers
  answer((false:+'<urn:example:is>'('<urn:example:ground>', '<urn:example:wet>'))).
  answer((false:+'<urn:example:is>'('<urn:example:it>', '<urn:example:raining>'))).

% proof steps
  step(((false:+A):+(B:+A), (false:+B)),
       (('<urn:example:is>'('<urn:example:ground>', '<urn:example:wet>'):+'<urn:example:is>'('<urn:example:it>', '<urn:example:raining>')), (false:+'<urn:example:is>'('<urn:example:ground>', '<urn:example:wet>'))),
       (false:+'<urn:example:is>'('<urn:example:it>', '<urn:example:raining>'))).
