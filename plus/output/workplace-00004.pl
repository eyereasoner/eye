:- op(1200, xfx, :+).

% answers
  answer('<urn:example:prepare>'(1, 4)).
  answer('<urn:example:complies>'(bob1, true)).
  answer('<urn:example:complies>'(bob2, true)).
  answer('<urn:example:complies>'(bob3, true)).
  answer('<urn:example:complies>'(bob4, true)).
  answer('<urn:example:complies>'(alice1, true)).
  answer('<urn:example:complies>'(alice2, true)).
  answer('<urn:example:complies>'(alice3, true)).
  answer('<urn:example:complies>'(alice4, true)).
  answer('<urn:example:complies>'(carol1, false)).
  answer('<urn:example:complies>'(carol2, false)).
  answer('<urn:example:complies>'(carol3, false)).
  answer('<urn:example:complies>'(carol4, false)).

% proof steps
  step((true:+'<urn:example:prepare>'(1, 4)), '<urn:example:prepare>'(1, 4), true).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob1, '<urn:example:work_related_task>'), '<urn:example:does>'(bob1, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob1, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob2, '<urn:example:work_related_task>'), '<urn:example:does>'(bob2, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob2, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob3, '<urn:example:work_related_task>'), '<urn:example:does>'(bob3, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob3, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob4, '<urn:example:work_related_task>'), '<urn:example:does>'(bob4, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob4, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice1, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice1, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice2, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice2, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice3, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice3, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice4, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice4, true)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol1, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol1, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol2, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol2, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol3, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol3, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol4, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol4, false)).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob1, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob2, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob3, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob4, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice1, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice2, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice3, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice4, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol1, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol2, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol3, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol4, false),
       true).
