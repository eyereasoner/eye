:- op(1200, xfx, :+).

% answers
  answer('<urn:example:prepare>'(1, 32)).
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
  answer('<urn:example:complies>'(bob17, true)).
  answer('<urn:example:complies>'(bob18, true)).
  answer('<urn:example:complies>'(bob19, true)).
  answer('<urn:example:complies>'(bob20, true)).
  answer('<urn:example:complies>'(bob21, true)).
  answer('<urn:example:complies>'(bob22, true)).
  answer('<urn:example:complies>'(bob23, true)).
  answer('<urn:example:complies>'(bob24, true)).
  answer('<urn:example:complies>'(bob25, true)).
  answer('<urn:example:complies>'(bob26, true)).
  answer('<urn:example:complies>'(bob27, true)).
  answer('<urn:example:complies>'(bob28, true)).
  answer('<urn:example:complies>'(bob29, true)).
  answer('<urn:example:complies>'(bob30, true)).
  answer('<urn:example:complies>'(bob31, true)).
  answer('<urn:example:complies>'(bob32, true)).
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
  answer('<urn:example:complies>'(alice17, true)).
  answer('<urn:example:complies>'(alice18, true)).
  answer('<urn:example:complies>'(alice19, true)).
  answer('<urn:example:complies>'(alice20, true)).
  answer('<urn:example:complies>'(alice21, true)).
  answer('<urn:example:complies>'(alice22, true)).
  answer('<urn:example:complies>'(alice23, true)).
  answer('<urn:example:complies>'(alice24, true)).
  answer('<urn:example:complies>'(alice25, true)).
  answer('<urn:example:complies>'(alice26, true)).
  answer('<urn:example:complies>'(alice27, true)).
  answer('<urn:example:complies>'(alice28, true)).
  answer('<urn:example:complies>'(alice29, true)).
  answer('<urn:example:complies>'(alice30, true)).
  answer('<urn:example:complies>'(alice31, true)).
  answer('<urn:example:complies>'(alice32, true)).
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
  answer('<urn:example:complies>'(carol17, false)).
  answer('<urn:example:complies>'(carol18, false)).
  answer('<urn:example:complies>'(carol19, false)).
  answer('<urn:example:complies>'(carol20, false)).
  answer('<urn:example:complies>'(carol21, false)).
  answer('<urn:example:complies>'(carol22, false)).
  answer('<urn:example:complies>'(carol23, false)).
  answer('<urn:example:complies>'(carol24, false)).
  answer('<urn:example:complies>'(carol25, false)).
  answer('<urn:example:complies>'(carol26, false)).
  answer('<urn:example:complies>'(carol27, false)).
  answer('<urn:example:complies>'(carol28, false)).
  answer('<urn:example:complies>'(carol29, false)).
  answer('<urn:example:complies>'(carol30, false)).
  answer('<urn:example:complies>'(carol31, false)).
  answer('<urn:example:complies>'(carol32, false)).

% proof steps
  step((true:+'<urn:example:prepare>'(1, 32)), '<urn:example:prepare>'(1, 32), true).
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
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob17, '<urn:example:work_related_task>'), '<urn:example:does>'(bob17, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob17, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob18, '<urn:example:work_related_task>'), '<urn:example:does>'(bob18, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob18, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob19, '<urn:example:work_related_task>'), '<urn:example:does>'(bob19, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob19, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob20, '<urn:example:work_related_task>'), '<urn:example:does>'(bob20, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob20, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob21, '<urn:example:work_related_task>'), '<urn:example:does>'(bob21, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob21, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob22, '<urn:example:work_related_task>'), '<urn:example:does>'(bob22, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob22, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob23, '<urn:example:work_related_task>'), '<urn:example:does>'(bob23, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob23, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob24, '<urn:example:work_related_task>'), '<urn:example:does>'(bob24, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob24, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob25, '<urn:example:work_related_task>'), '<urn:example:does>'(bob25, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob25, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob26, '<urn:example:work_related_task>'), '<urn:example:does>'(bob26, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob26, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob27, '<urn:example:work_related_task>'), '<urn:example:does>'(bob27, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob27, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob28, '<urn:example:work_related_task>'), '<urn:example:does>'(bob28, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob28, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob29, '<urn:example:work_related_task>'), '<urn:example:does>'(bob29, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob29, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob30, '<urn:example:work_related_task>'), '<urn:example:does>'(bob30, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob30, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob31, '<urn:example:work_related_task>'), '<urn:example:does>'(bob31, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob31, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'(bob32, '<urn:example:work_related_task>'), '<urn:example:does>'(bob32, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'(bob32, true)).
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
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice17, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice17, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice18, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice18, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice19, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice19, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice20, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice20, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice21, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice21, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice22, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice22, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice23, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice23, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice24, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice24, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice25, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice25, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice26, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice26, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice27, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice27, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice28, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice28, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice29, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice29, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice30, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice30, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice31, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice31, true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'(alice32, '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'(alice32, true)).
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
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol17, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol17, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol18, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol18, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol19, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol19, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol20, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol20, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol21, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol21, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol22, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol22, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol23, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol23, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol24, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol24, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol25, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol25, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol26, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol26, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol27, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol27, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol28, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol28, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol29, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol29, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol30, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol30, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol31, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol31, false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'(carol32, '<urn:example:access_social_media>'),
       '<urn:example:complies>'(carol32, false)).
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
       '<urn:example:complies>'(bob17, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob18, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob19, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob20, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob21, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob22, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob23, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob24, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob25, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob26, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob27, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob28, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob29, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob30, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob31, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(bob32, true),
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
       '<urn:example:complies>'(alice17, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice18, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice19, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice20, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice21, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice22, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice23, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice24, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice25, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice26, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice27, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice28, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice29, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice30, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice31, true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(alice32, true),
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
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol17, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol18, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol19, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol20, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol21, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol22, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol23, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol24, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol25, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol26, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol27, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol28, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol29, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol30, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol31, false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'(carol32, false),
       true).
