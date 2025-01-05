:- op(1200, xfx, :+).

% answers
  answer('<urn:example:prepare>'(1, 1)).
  answer('<urn:example:complies>'(bob1, true)).
  answer('<urn:example:complies>'(alice1, true)).
  answer('<urn:example:complies>'(carol1, false)).

% proof steps
  step((true:+'<urn:example:prepare>'(1, 1)), '<urn:example:prepare>'(1, 1), true).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob1, '<urn:example:work_related_task>'), '<urn:example:does>'(bob1, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob1, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice1, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice1, true)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol1, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol1, false)).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob1, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice1, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol1, false),
       true).
