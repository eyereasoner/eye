:- op(1200, xfx, :+).

% answers
  answer('<urn:example:prepare>'(1, 64)).
  answer('<urn:example:complies>'('<urn:example:bob8>', true)).
  answer('<urn:example:complies>'('<urn:example:bob16>', true)).
  answer('<urn:example:complies>'('<urn:example:bob24>', true)).
  answer('<urn:example:complies>'('<urn:example:bob32>', true)).
  answer('<urn:example:complies>'('<urn:example:bob40>', true)).
  answer('<urn:example:complies>'('<urn:example:bob48>', true)).
  answer('<urn:example:complies>'('<urn:example:bob56>', true)).
  answer('<urn:example:complies>'('<urn:example:bob64>', true)).
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
  answer('<urn:example:complies>'('<urn:example:alice34>', true)).
  answer('<urn:example:complies>'('<urn:example:alice36>', true)).
  answer('<urn:example:complies>'('<urn:example:alice38>', true)).
  answer('<urn:example:complies>'('<urn:example:alice40>', true)).
  answer('<urn:example:complies>'('<urn:example:alice42>', true)).
  answer('<urn:example:complies>'('<urn:example:alice44>', true)).
  answer('<urn:example:complies>'('<urn:example:alice46>', true)).
  answer('<urn:example:complies>'('<urn:example:alice48>', true)).
  answer('<urn:example:complies>'('<urn:example:alice50>', true)).
  answer('<urn:example:complies>'('<urn:example:alice52>', true)).
  answer('<urn:example:complies>'('<urn:example:alice54>', true)).
  answer('<urn:example:complies>'('<urn:example:alice56>', true)).
  answer('<urn:example:complies>'('<urn:example:alice58>', true)).
  answer('<urn:example:complies>'('<urn:example:alice60>', true)).
  answer('<urn:example:complies>'('<urn:example:alice62>', true)).
  answer('<urn:example:complies>'('<urn:example:alice64>', true)).
  answer('<urn:example:complies>'('<urn:example:carol444>', false)).
  answer('<urn:example:complies>'('<urn:example:carol888>', false)).
  answer('<urn:example:complies>'('<urn:example:carol121212>', false)).
  answer('<urn:example:complies>'('<urn:example:carol161616>', false)).
  answer('<urn:example:complies>'('<urn:example:carol202020>', false)).
  answer('<urn:example:complies>'('<urn:example:carol242424>', false)).
  answer('<urn:example:complies>'('<urn:example:carol282828>', false)).
  answer('<urn:example:complies>'('<urn:example:carol323232>', false)).
  answer('<urn:example:complies>'('<urn:example:carol363636>', false)).
  answer('<urn:example:complies>'('<urn:example:carol404040>', false)).
  answer('<urn:example:complies>'('<urn:example:carol444444>', false)).
  answer('<urn:example:complies>'('<urn:example:carol484848>', false)).
  answer('<urn:example:complies>'('<urn:example:carol525252>', false)).
  answer('<urn:example:complies>'('<urn:example:carol565656>', false)).
  answer('<urn:example:complies>'('<urn:example:carol606060>', false)).
  answer('<urn:example:complies>'('<urn:example:carol646464>', false)).
  answer('<urn:example:complies>'('<urn:example:bob4>', false)).
  answer('<urn:example:complies>'('<urn:example:bob12>', false)).
  answer('<urn:example:complies>'('<urn:example:bob20>', false)).
  answer('<urn:example:complies>'('<urn:example:bob28>', false)).
  answer('<urn:example:complies>'('<urn:example:bob36>', false)).
  answer('<urn:example:complies>'('<urn:example:bob44>', false)).
  answer('<urn:example:complies>'('<urn:example:bob52>', false)).
  answer('<urn:example:complies>'('<urn:example:bob60>', false)).

% proof steps
  step((true:+'<urn:example:prepare>'(1, 64)), '<urn:example:prepare>'(1, 64), true).
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
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob40>', '<urn:example:work_related_task>'), '<urn:example:does>'('<urn:example:bob40>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob40>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob48>', '<urn:example:work_related_task>'), '<urn:example:does>'('<urn:example:bob48>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob48>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob56>', '<urn:example:work_related_task>'), '<urn:example:does>'('<urn:example:bob56>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob56>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), '<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob64>', '<urn:example:work_related_task>'), '<urn:example:does>'('<urn:example:bob64>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob64>', true)).
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
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice34>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice34>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice36>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice36>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice38>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice38>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice40>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice40>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice42>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice42>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice44>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice44>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice46>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice46>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice48>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice48>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice50>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice50>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice52>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice52>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice54>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice54>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice56>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice56>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice58>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice58>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice60>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice60>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice62>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice62>', true)).
  step(('<urn:example:complies>'(A, true):+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:does>'('<urn:example:alice64>',
                            '<urn:example:log_off_at_end_of_shift>'),
       '<urn:example:complies>'('<urn:example:alice64>', true)).
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
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol363636>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol363636>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol404040>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol404040>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol444444>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol444444>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol484848>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol484848>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol525252>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol525252>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol565656>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol565656>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol606060>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol606060>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:access_social_media>')),
       '<urn:example:does>'('<urn:example:carol646464>',
                            '<urn:example:access_social_media>'),
       '<urn:example:complies>'('<urn:example:carol646464>', false)).
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
       '<urn:example:complies>'('<urn:example:bob40>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob48>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob56>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob64>', true),
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
       '<urn:example:complies>'('<urn:example:alice34>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice36>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice38>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice40>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice42>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice44>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice46>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice48>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice50>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice52>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice54>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice56>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice58>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice60>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice62>', true),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:alice64>', true),
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
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol363636>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol404040>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol444444>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol484848>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol525252>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol565656>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol606060>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:carol646464>', false),
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
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob36>', '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'('<urn:example:bob36>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob36>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob44>', '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'('<urn:example:bob44>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob44>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob52>', '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'('<urn:example:bob52>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob52>', false)).
  step(('<urn:example:complies>'(A, false):+'<urn:example:does>'(A, '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')),
       ('<urn:example:does>'('<urn:example:bob60>', '<urn:example:work_related_task>'), stable(1), \+'<urn:example:does>'('<urn:example:bob60>', '<urn:example:log_off_at_end_of_shift>')),
       '<urn:example:complies>'('<urn:example:bob60>', false)).
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
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob36>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob44>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob52>', false),
       true).
  step((true:+'<urn:example:complies>'(_, _)),
       '<urn:example:complies>'('<urn:example:bob60>', false),
       true).
