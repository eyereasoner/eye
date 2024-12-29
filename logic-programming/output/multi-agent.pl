:- op(1200, xfx, :+).

% answers
  answer('<urn:example:obligation>'('<urn:example:complete:task>'('<urn:example:agent2>',
                                                                  '<urn:example:task1>'))).
  answer('<urn:example:obligation>'('<urn:example:escalate:task>'('<urn:example:agent1>',
                                                                  '<urn:example:task1>'))).
  answer('<urn:example:permission>'('<urn:example:execute:task>'('<urn:example:agent2>',
                                                                 '<urn:example:task1>'))).
  answer('<urn:example:violation>'('<urn:example:task1>')).
  answer('<urn:example:sanction>'('<urn:example:agent2>')).

% proof steps
  step(('<urn:example:obligation>'('<urn:example:complete:task>'(A, B)):+'<urn:example:assigned>'(B, A)),
       '<urn:example:assigned>'('<urn:example:task1>', '<urn:example:agent2>'),
       '<urn:example:obligation>'('<urn:example:complete:task>'('<urn:example:agent2>',
                                                                '<urn:example:task1>'))).
  step(('<urn:example:obligation>'('<urn:example:escalate:task>'(A, B)):+'<urn:example:role>'(A, '<urn:example:manager>'), '<urn:example:assigned>'(B, _), '<urn:example:deadline>'(B, C), '<urn:example:time:current>'(D), D>=C, \+'<urn:example:completed>'(B)),
       ('<urn:example:role>'('<urn:example:agent1>', '<urn:example:manager>'), '<urn:example:assigned>'('<urn:example:task1>', '<urn:example:agent2>'), '<urn:example:deadline>'('<urn:example:task1>', 10), '<urn:example:time:current>'(15), 15>=10, \+'<urn:example:completed>'('<urn:example:task1>')),
       '<urn:example:obligation>'('<urn:example:escalate:task>'('<urn:example:agent1>',
                                                                '<urn:example:task1>'))).
  step(('<urn:example:permission>'('<urn:example:execute:task>'(A, B)):+'<urn:example:role>'(A, '<urn:example:employee>'), '<urn:example:assigned>'(B, A)),
       ('<urn:example:role>'('<urn:example:agent2>', '<urn:example:employee>'), '<urn:example:assigned>'('<urn:example:task1>', '<urn:example:agent2>')),
       '<urn:example:permission>'('<urn:example:execute:task>'('<urn:example:agent2>',
                                                               '<urn:example:task1>'))).
  step(('<urn:example:violation>'(A):+'<urn:example:obligation>'('<urn:example:complete:task>'(_, A)), '<urn:example:time:current>'(B), '<urn:example:deadline>'(A, C), B>C, \+'<urn:example:completed>'(A)),
       ('<urn:example:obligation>'('<urn:example:complete:task>'('<urn:example:agent2>', '<urn:example:task1>')), '<urn:example:time:current>'(15), '<urn:example:deadline>'('<urn:example:task1>', 10), 15>10, \+'<urn:example:completed>'('<urn:example:task1>')),
       '<urn:example:violation>'('<urn:example:task1>')).
  step(('<urn:example:sanction>'(A):+'<urn:example:violation>'(B), '<urn:example:assigned>'(B, A)),
       ('<urn:example:violation>'('<urn:example:task1>'), '<urn:example:assigned>'('<urn:example:task1>', '<urn:example:agent2>')),
       '<urn:example:sanction>'('<urn:example:agent2>')).
