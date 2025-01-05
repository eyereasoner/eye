:- op(1200, xfx, :+).

% answers
  answer('<urn:example:got>'('<urn:example:all>', '<urn:example:dinner>')).

% proof steps
  step((true:+'<urn:example:got>'('<urn:example:all>', '<urn:example:dinner>')),
       '<urn:example:got>'('<urn:example:all>', '<urn:example:dinner>'),
       true).
