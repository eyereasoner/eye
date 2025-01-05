:- op(1200, xfx, :+).

% answers
  answer('<urn:example:prepare>'(1, 8)).
  answer('<urn:example:complies>'(bob1, true)).
  answer('<urn:example:complies>'(bob2, true)).
  answer('<urn:example:complies>'(bob3, true)).
  answer('<urn:example:complies>'(bob4, true)).
  answer('<urn:example:complies>'(bob5, true)).
  answer('<urn:example:complies>'(bob6, true)).
  answer('<urn:example:complies>'(bob7, true)).
  answer('<urn:example:complies>'(bob8, true)).
  answer('<urn:example:complies>'(alice1, true)).
  answer('<urn:example:complies>'(alice2, true)).
  answer('<urn:example:complies>'(alice3, true)).
  answer('<urn:example:complies>'(alice4, true)).
  answer('<urn:example:complies>'(alice5, true)).
  answer('<urn:example:complies>'(alice6, true)).
  answer('<urn:example:complies>'(alice7, true)).
  answer('<urn:example:complies>'(alice8, true)).
  answer('<urn:example:complies>'(carol1, false)).
  answer('<urn:example:complies>'(carol2, false)).
  answer('<urn:example:complies>'(carol3, false)).
  answer('<urn:example:complies>'(carol4, false)).
  answer('<urn:example:complies>'(carol5, false)).
  answer('<urn:example:complies>'(carol6, false)).
  answer('<urn:example:complies>'(carol7, false)).
  answer('<urn:example:complies>'(carol8, false)).

% proof steps
  step((true:+'<urn:example:prepare>'(1, 8)), '<urn:example:prepare>'(1, 8), true).
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
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob5, '<urn:example:work_related_task>'), '<urn:example:does>'(bob5, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob5, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob6, '<urn:example:work_related_task>'), '<urn:example:does>'(bob6, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob6, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob7, '<urn:example:work_related_task>'), '<urn:example:does>'(bob7, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob7, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob8, '<urn:example:work_related_task>'), '<urn:example:does>'(bob8, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob8, true)).
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
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice5, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice5, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice6, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice6, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice7, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice7, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice8, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice8, true)).
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
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol5, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol5, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol6, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol6, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol7, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol7, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol8, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol8, false)).
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
       '<urn:example:complies>'(bob5, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob6, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob7, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob8, true),
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
       '<urn:example:complies>'(alice5, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice6, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice7, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice8, true),
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
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol5, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol6, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol7, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol8, false),
       true).
