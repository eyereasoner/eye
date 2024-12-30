:- op(1200, xfx, :+).

% answers
  answer('<urn:example:goldbach>'(123456789012, [61, 123456788951])).
  answer('<urn:example:goldbach>'(123456789014, [3, 123456789011])).
  answer('<urn:example:goldbach>'(123456789016, [5, 123456789011])).
  answer('<urn:example:goldbach>'(123456789018, [7, 123456789011])).
  answer('<urn:example:goldbach>'(123456789020, [109, 123456788911])).
  answer('<urn:example:goldbach>'(123456789022, [11, 123456789011])).
  answer('<urn:example:goldbach>'(123456789024, [13, 123456789011])).
  answer('<urn:example:goldbach>'(123456789026, [127, 123456788899])).
  answer('<urn:example:goldbach>'(123456789028, [17, 123456789011])).
  answer('<urn:example:goldbach>'(123456789030, [19, 123456789011])).

% proof steps
  step((true:+'<urn:example:goldbach>'(123456789012, [_, _])),
       '<urn:example:goldbach>'(123456789012, [61, 123456788951]),
       true).
  step((true:+'<urn:example:goldbach>'(123456789014, [_, _])),
       '<urn:example:goldbach>'(123456789014, [3, 123456789011]),
       true).
  step((true:+'<urn:example:goldbach>'(123456789016, [_, _])),
       '<urn:example:goldbach>'(123456789016, [5, 123456789011]),
       true).
  step((true:+'<urn:example:goldbach>'(123456789018, [_, _])),
       '<urn:example:goldbach>'(123456789018, [7, 123456789011]),
       true).
  step((true:+'<urn:example:goldbach>'(123456789020, [_, _])),
       '<urn:example:goldbach>'(123456789020, [109, 123456788911]),
       true).
  step((true:+'<urn:example:goldbach>'(123456789022, [_, _])),
       '<urn:example:goldbach>'(123456789022, [11, 123456789011]),
       true).
  step((true:+'<urn:example:goldbach>'(123456789024, [_, _])),
       '<urn:example:goldbach>'(123456789024, [13, 123456789011]),
       true).
  step((true:+'<urn:example:goldbach>'(123456789026, [_, _])),
       '<urn:example:goldbach>'(123456789026, [127, 123456788899]),
       true).
  step((true:+'<urn:example:goldbach>'(123456789028, [_, _])),
       '<urn:example:goldbach>'(123456789028, [17, 123456789011]),
       true).
  step((true:+'<urn:example:goldbach>'(123456789030, [_, _])),
       '<urn:example:goldbach>'(123456789030, [19, 123456789011]),
       true).
