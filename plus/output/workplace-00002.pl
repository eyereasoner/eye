:- op(1200, xfx, :+).

% answers
  answer('<urn:example:prepare>'(1, 2)).
  answer('<urn:example:complies>'('<urn:example:alice2>', true)).

% proof steps
  step((true:+'<urn:example:prepare>'(1, 2)), '<urn:example:prepare>'(1, 2), true).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice2>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice2>', true)).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice2>', true),
       true).
