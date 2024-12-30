:- op(1200, xfx, :+).

% answers
  answer('<urn:example:ageAbove>'('<urn:example:patH>', 80)).

% proof steps
  step((true:+'<urn:example:ageAbove>'(_, 80)),
       '<urn:example:ageAbove>'('<urn:example:patH>', 80),
       true).
