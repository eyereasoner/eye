:- op(1200, xfx, :+).

% answers
  answer('<urn:example:prepare>'(1, 16)).
  answer('<urn:example:complies>'(bob1, true)).
  answer('<urn:example:complies>'(bob2, true)).
  answer('<urn:example:complies>'(bob3, true)).
  answer('<urn:example:complies>'(bob4, true)).
  answer('<urn:example:complies>'(bob5, true)).
  answer('<urn:example:complies>'(bob6, true)).
  answer('<urn:example:complies>'(bob7, true)).
  answer('<urn:example:complies>'(bob8, true)).
  answer('<urn:example:complies>'(bob9, true)).
  answer('<urn:example:complies>'(bob10, true)).
  answer('<urn:example:complies>'(bob11, true)).
  answer('<urn:example:complies>'(bob12, true)).
  answer('<urn:example:complies>'(bob13, true)).
  answer('<urn:example:complies>'(bob14, true)).
  answer('<urn:example:complies>'(bob15, true)).
  answer('<urn:example:complies>'(bob16, true)).
  answer('<urn:example:complies>'(alice1, true)).
  answer('<urn:example:complies>'(alice2, true)).
  answer('<urn:example:complies>'(alice3, true)).
  answer('<urn:example:complies>'(alice4, true)).
  answer('<urn:example:complies>'(alice5, true)).
  answer('<urn:example:complies>'(alice6, true)).
  answer('<urn:example:complies>'(alice7, true)).
  answer('<urn:example:complies>'(alice8, true)).
  answer('<urn:example:complies>'(alice9, true)).
  answer('<urn:example:complies>'(alice10, true)).
  answer('<urn:example:complies>'(alice11, true)).
  answer('<urn:example:complies>'(alice12, true)).
  answer('<urn:example:complies>'(alice13, true)).
  answer('<urn:example:complies>'(alice14, true)).
  answer('<urn:example:complies>'(alice15, true)).
  answer('<urn:example:complies>'(alice16, true)).
  answer('<urn:example:complies>'(carol1, false)).
  answer('<urn:example:complies>'(carol2, false)).
  answer('<urn:example:complies>'(carol3, false)).
  answer('<urn:example:complies>'(carol4, false)).
  answer('<urn:example:complies>'(carol5, false)).
  answer('<urn:example:complies>'(carol6, false)).
  answer('<urn:example:complies>'(carol7, false)).
  answer('<urn:example:complies>'(carol8, false)).
  answer('<urn:example:complies>'(carol9, false)).
  answer('<urn:example:complies>'(carol10, false)).
  answer('<urn:example:complies>'(carol11, false)).
  answer('<urn:example:complies>'(carol12, false)).
  answer('<urn:example:complies>'(carol13, false)).
  answer('<urn:example:complies>'(carol14, false)).
  answer('<urn:example:complies>'(carol15, false)).
  answer('<urn:example:complies>'(carol16, false)).

% proof steps
  step((true:+'<urn:example:prepare>'(1, 16)), '<urn:example:prepare>'(1, 16), true).
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
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob9, '<urn:example:work_related_task>'), '<urn:example:does>'(bob9, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob9, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob10, '<urn:example:work_related_task>'), '<urn:example:does>'(bob10, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob10, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob11, '<urn:example:work_related_task>'), '<urn:example:does>'(bob11, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob11, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob12, '<urn:example:work_related_task>'), '<urn:example:does>'(bob12, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob12, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob13, '<urn:example:work_related_task>'), '<urn:example:does>'(bob13, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob13, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob14, '<urn:example:work_related_task>'), '<urn:example:does>'(bob14, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob14, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob15, '<urn:example:work_related_task>'), '<urn:example:does>'(bob15, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob15, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob16, '<urn:example:work_related_task>'), '<urn:example:does>'(bob16, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob16, true)).
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
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice9, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice9, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice10, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice10, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice11, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice11, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice12, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice12, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice13, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice13, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice14, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice14, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice15, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice15, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice16, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice16, true)).
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
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol9, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol9, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol10, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol10, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol11, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol11, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol12, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol12, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol13, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol13, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol14, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol14, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol15, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol15, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol16, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol16, false)).
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
       '<urn:example:complies>'(bob9, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob10, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob11, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob12, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob13, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob14, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob15, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob16, true),
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
       '<urn:example:complies>'(alice9, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice10, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice11, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice12, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice13, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice14, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice15, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice16, true),
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
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol9, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol10, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol11, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol12, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol13, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol14, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol15, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol16, false),
       true).
