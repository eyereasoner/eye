:- op(1200, xfx, :+).

% answers
  answer('<urn:example:prepare>'(1, 32)).
  answer('<urn:example:complies>'('<urn:example:bob8>', true)).
  answer('<urn:example:complies>'('<urn:example:bob16>', true)).
  answer('<urn:example:complies>'('<urn:example:bob24>', true)).
  answer('<urn:example:complies>'('<urn:example:bob32>', true)).
  answer('<urn:example:complies>'('<urn:example:alice2>', true)).
  answer('<urn:example:complies>'('<urn:example:alice4>', true)).
  answer('<urn:example:complies>'('<urn:example:alice6>', true)).
  answer('<urn:example:complies>'('<urn:example:alice8>', true)).
  answer('<urn:example:complies>'('<urn:example:alice10>', true)).
  answer('<urn:example:complies>'('<urn:example:alice12>', true)).
  answer('<urn:example:complies>'('<urn:example:alice14>', true)).
  answer('<urn:example:complies>'('<urn:example:alice16>', true)).
  answer('<urn:example:complies>'('<urn:example:alice18>', true)).
  answer('<urn:example:complies>'('<urn:example:alice20>', true)).
  answer('<urn:example:complies>'('<urn:example:alice22>', true)).
  answer('<urn:example:complies>'('<urn:example:alice24>', true)).
  answer('<urn:example:complies>'('<urn:example:alice26>', true)).
  answer('<urn:example:complies>'('<urn:example:alice28>', true)).
  answer('<urn:example:complies>'('<urn:example:alice30>', true)).
  answer('<urn:example:complies>'('<urn:example:alice32>', true)).
  answer('<urn:example:complies>'('<urn:example:carol444>', false)).
  answer('<urn:example:complies>'('<urn:example:carol888>', false)).
  answer('<urn:example:complies>'('<urn:example:carol121212>', false)).
  answer('<urn:example:complies>'('<urn:example:carol161616>', false)).
  answer('<urn:example:complies>'('<urn:example:carol202020>', false)).
  answer('<urn:example:complies>'('<urn:example:carol242424>', false)).
  answer('<urn:example:complies>'('<urn:example:carol282828>', false)).
  answer('<urn:example:complies>'('<urn:example:carol323232>', false)).
  answer('<urn:example:complies>'('<urn:example:bob4>', false)).
  answer('<urn:example:complies>'('<urn:example:bob12>', false)).
  answer('<urn:example:complies>'('<urn:example:bob20>', false)).
  answer('<urn:example:complies>'('<urn:example:bob28>', false)).

% proof steps
  step((true:+'<urn:example:prepare>'(1, 32)), '<urn:example:prepare>'(1, 32), true).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob8>', '<urn:example:work_related_task>'), '<urn:example:does>'('<urn:example:bob8>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob8>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob16>', '<urn:example:work_related_task>'), '<urn:example:does>'('<urn:example:bob16>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob16>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob24>', '<urn:example:work_related_task>'), '<urn:example:does>'('<urn:example:bob24>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob24>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob32>', '<urn:example:work_related_task>'), '<urn:example:does>'('<urn:example:bob32>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob32>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice2>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice2>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice4>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice4>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice6>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice6>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice8>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice8>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice10>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice10>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice12>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice12>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice14>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice14>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice16>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice16>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice18>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice18>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice20>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice20>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice22>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice22>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice24>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice24>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice26>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice26>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice28>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice28>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice30>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice30>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice32>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice32>', true)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol444>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol444>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol888>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol888>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol121212>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol121212>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol161616>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol161616>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol202020>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol202020>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol242424>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol242424>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol282828>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol282828>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol323232>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol323232>', false)).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob8>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob16>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob24>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob32>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice2>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice4>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice6>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice8>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice10>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice12>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice14>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice16>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice18>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice20>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice22>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice24>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice26>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice28>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice30>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice32>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol444>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol888>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol121212>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol161616>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol202020>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol242424>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol282828>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol323232>', false),
       true).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob4>', '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'('<urn:example:bob4>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob4>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob12>', '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'('<urn:example:bob12>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob12>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob20>', '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'('<urn:example:bob20>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob20>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob28>', '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'('<urn:example:bob28>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob28>', false)).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob4>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob12>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob20>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob28>', false),
       true).
