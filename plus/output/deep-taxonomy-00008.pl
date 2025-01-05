:- op(1200, xfx, :+).

% answers
  answer('<urn:example:prepare>'(1, 8)).

% proof steps
  step((true:+'<urn:example:prepare>'(1, 8)), '<urn:example:prepare>'(1, 8), true).
  step(('<urn:example:N1>'(A):+'<urn:example:N0>'(A)),
       '<urn:example:N0>'('<urn:example:z>'),
       '<urn:example:N1>'('<urn:example:z>')).
  step(('<urn:example:I1>'(A):+'<urn:example:N0>'(A)),
       '<urn:example:N0>'('<urn:example:z>'),
       '<urn:example:I1>'('<urn:example:z>')).
  step(('<urn:example:J1>'(A):+'<urn:example:N0>'(A)),
       '<urn:example:N0>'('<urn:example:z>'),
       '<urn:example:J1>'('<urn:example:z>')).
  step(('<urn:example:N2>'(A):+'<urn:example:N1>'(A)),
       '<urn:example:N1>'('<urn:example:z>'),
       '<urn:example:N2>'('<urn:example:z>')).
  step(('<urn:example:I2>'(A):+'<urn:example:N1>'(A)),
       '<urn:example:N1>'('<urn:example:z>'),
       '<urn:example:I2>'('<urn:example:z>')).
  step(('<urn:example:J2>'(A):+'<urn:example:N1>'(A)),
       '<urn:example:N1>'('<urn:example:z>'),
       '<urn:example:J2>'('<urn:example:z>')).
