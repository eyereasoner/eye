:- op(1200, xfx, :+).

% answers
  answer('<urn:example:collatz>'(1, 1, 1)).
  answer('<urn:example:collatz>'(2, 1, 2)).
  answer('<urn:example:collatz>'(3, 1, 8)).
  answer('<urn:example:collatz>'(4, 1, 3)).
  answer('<urn:example:collatz>'(5, 1, 6)).
  answer('<urn:example:collatz>'(6, 1, 9)).
  answer('<urn:example:collatz>'(7, 1, 17)).
  answer('<urn:example:collatz>'(8, 1, 4)).
  answer('<urn:example:collatz>'(9, 1, 20)).
  answer('<urn:example:collatz>'(10, 1, 7)).
  answer('<urn:example:collatz>'(11, 1, 15)).
  answer('<urn:example:collatz>'(12, 1, 10)).
  answer('<urn:example:collatz>'(13, 1, 10)).
  answer('<urn:example:collatz>'(14, 1, 18)).
  answer('<urn:example:collatz>'(15, 1, 18)).
  answer('<urn:example:collatz>'(16, 1, 5)).

% proof steps
  step((true:+'<urn:example:collatz>'(1, 1, _)),
       '<urn:example:collatz>'(1, 1, 1),
       true).
  step((true:+'<urn:example:collatz>'(2, 1, _)),
       '<urn:example:collatz>'(2, 1, 2),
       true).
  step((true:+'<urn:example:collatz>'(3, 1, _)),
       '<urn:example:collatz>'(3, 1, 8),
       true).
  step((true:+'<urn:example:collatz>'(4, 1, _)),
       '<urn:example:collatz>'(4, 1, 3),
       true).
  step((true:+'<urn:example:collatz>'(5, 1, _)),
       '<urn:example:collatz>'(5, 1, 6),
       true).
  step((true:+'<urn:example:collatz>'(6, 1, _)),
       '<urn:example:collatz>'(6, 1, 9),
       true).
  step((true:+'<urn:example:collatz>'(7, 1, _)),
       '<urn:example:collatz>'(7, 1, 17),
       true).
  step((true:+'<urn:example:collatz>'(8, 1, _)),
       '<urn:example:collatz>'(8, 1, 4),
       true).
  step((true:+'<urn:example:collatz>'(9, 1, _)),
       '<urn:example:collatz>'(9, 1, 20),
       true).
  step((true:+'<urn:example:collatz>'(10, 1, _)),
       '<urn:example:collatz>'(10, 1, 7),
       true).
  step((true:+'<urn:example:collatz>'(11, 1, _)),
       '<urn:example:collatz>'(11, 1, 15),
       true).
  step((true:+'<urn:example:collatz>'(12, 1, _)),
       '<urn:example:collatz>'(12, 1, 10),
       true).
  step((true:+'<urn:example:collatz>'(13, 1, _)),
       '<urn:example:collatz>'(13, 1, 10),
       true).
  step((true:+'<urn:example:collatz>'(14, 1, _)),
       '<urn:example:collatz>'(14, 1, 18),
       true).
  step((true:+'<urn:example:collatz>'(15, 1, _)),
       '<urn:example:collatz>'(15, 1, 18),
       true).
  step((true:+'<urn:example:collatz>'(16, 1, _)),
       '<urn:example:collatz>'(16, 1, 5),
       true).
