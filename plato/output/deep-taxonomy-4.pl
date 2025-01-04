:- op(1200, xfx, :+).

% answers
  answer('<urn:example:prepare>'(1, 4)).

% proof steps
  step((true:+'<urn:example:prepare>'(1, 4)), '<urn:example:prepare>'(1, 4), true).
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
  step(('<urn:example:N3>'(A):+'<urn:example:N2>'(A)),
       '<urn:example:N2>'('<urn:example:z>'),
       '<urn:example:N3>'('<urn:example:z>')).
  step(('<urn:example:I3>'(A):+'<urn:example:N2>'(A)),
       '<urn:example:N2>'('<urn:example:z>'),
       '<urn:example:I3>'('<urn:example:z>')).
  step(('<urn:example:J3>'(A):+'<urn:example:N2>'(A)),
       '<urn:example:N2>'('<urn:example:z>'),
       '<urn:example:J3>'('<urn:example:z>')).
  step(('<urn:example:N4>'(A):+'<urn:example:N3>'(A)),
       '<urn:example:N3>'('<urn:example:z>'),
       '<urn:example:N4>'('<urn:example:z>')).
  step(('<urn:example:I4>'(A):+'<urn:example:N3>'(A)),
       '<urn:example:N3>'('<urn:example:z>'),
       '<urn:example:I4>'('<urn:example:z>')).
  step(('<urn:example:J4>'(A):+'<urn:example:N3>'(A)),
       '<urn:example:N3>'('<urn:example:z>'),
       '<urn:example:J4>'('<urn:example:z>')).
