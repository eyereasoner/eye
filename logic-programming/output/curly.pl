:- op(1200, xfx, :+).

% answers
  answer('<urn:example:test>'('Philip K Dick')).

% proof steps
  step((true:+'<urn:example:test>'(_)),
       '<urn:example:test>'('Philip K Dick'),
       true).
