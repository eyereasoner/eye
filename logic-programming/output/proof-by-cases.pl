:- op(1200, xfx, :+).

% answers
  answer('<urn:example:is>'('<urn:example:water>', '<urn:example:observable>')).

% proof steps
  step(('<urn:example:allPossibleCases>'([A], ['<urn:example:is>'(A, '<urn:example:solid>'), '<urn:example:is>'(A, '<urn:example:liquid>'), '<urn:example:is>'(A, '<urn:example:gas>')]):+'<urn:example:InorganicCompound>'(A)),
       '<urn:example:InorganicCompound>'('<urn:example:water>'),
       '<urn:example:allPossibleCases>'(['<urn:example:water>'],
                                        [ '<urn:example:is>'('<urn:example:water>',
                                                             '<urn:example:solid>'),
                                          '<urn:example:is>'('<urn:example:water>',
                                                             '<urn:example:liquid>'),
                                          '<urn:example:is>'('<urn:example:water>',
                                                             '<urn:example:gas>')
                                        ])).
  step(('<urn:example:is>'(A, '<urn:example:observable>'):+'<urn:example:allPossibleCases>'([A], B), forall(member('<urn:example:is>'(A, C), B), ('<urn:example:is>'(A, '<urn:example:observable>'):+'<urn:example:is>'(A, C)))),
       ('<urn:example:allPossibleCases>'(['<urn:example:water>'], ['<urn:example:is>'('<urn:example:water>', '<urn:example:solid>'), '<urn:example:is>'('<urn:example:water>', '<urn:example:liquid>'), '<urn:example:is>'('<urn:example:water>', '<urn:example:gas>')]), forall(member('<urn:example:is>'('<urn:example:water>', D), ['<urn:example:is>'('<urn:example:water>', '<urn:example:solid>'), '<urn:example:is>'('<urn:example:water>', '<urn:example:liquid>'), '<urn:example:is>'('<urn:example:water>', '<urn:example:gas>')]), ('<urn:example:is>'('<urn:example:water>', '<urn:example:observable>'):+'<urn:example:is>'('<urn:example:water>', D)))),
       '<urn:example:is>'('<urn:example:water>', '<urn:example:observable>')).
  step((true:+'<urn:example:is>'(_, _)),
       '<urn:example:is>'('<urn:example:water>', '<urn:example:observable>'),
       true).
