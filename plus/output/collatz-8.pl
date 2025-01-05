:- op(1200, xfx, :+).

% answers
  answer('<urn:example:collatz>'(1, 1)).
  answer('<urn:example:collatz>'(2, 1)).
  answer('<urn:example:collatz>'(3, 1)).
  answer('<urn:example:collatz>'(4, 1)).
  answer('<urn:example:collatz>'(5, 1)).
  answer('<urn:example:collatz>'(6, 1)).
  answer('<urn:example:collatz>'(7, 1)).
  answer('<urn:example:collatz>'(8, 1)).

% proof steps
  step((true:+'<urn:example:collatz>'(1, 1)), '<urn:example:collatz>'(1, 1), true).
  step((true:+'<urn:example:collatz>'(2, 1)), '<urn:example:collatz>'(2, 1), true).
  step((true:+'<urn:example:collatz>'(3, 1)), '<urn:example:collatz>'(3, 1), true).
  step((true:+'<urn:example:collatz>'(4, 1)), '<urn:example:collatz>'(4, 1), true).
  step((true:+'<urn:example:collatz>'(5, 1)), '<urn:example:collatz>'(5, 1), true).
  step((true:+'<urn:example:collatz>'(6, 1)), '<urn:example:collatz>'(6, 1), true).
  step((true:+'<urn:example:collatz>'(7, 1)), '<urn:example:collatz>'(7, 1), true).
  step((true:+'<urn:example:collatz>'(8, 1)), '<urn:example:collatz>'(8, 1), true).
