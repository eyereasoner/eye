:- op(1200, xfx, :+).

% answers
  answer('<urn:example:prepare>'(1, 2)).

% proof steps
  step((true:+'<urn:example:prepare>'(1, 2)), '<urn:example:prepare>'(1, 2), true).
