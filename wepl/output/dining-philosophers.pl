:- op(1200, xfx, :+).

% <urn:example:person1> thinking for 0.1 seconds
% <urn:example:person3> thinking for 0.15 seconds
% <urn:example:person2> thinking for 0.2 seconds
% <urn:example:person1> eating for 0.1 seconds
% <urn:example:person4> thinking for 0.25 seconds
% <urn:example:person5> thinking for 0.25 seconds
% <urn:example:person3> eating for 0.1 seconds
% <urn:example:person2> eating for 0.2 seconds
% <urn:example:person4> eating for 0.2 seconds
% <urn:example:person5> eating for 0.1 seconds
% <urn:example:person1> thinking for 0.1 seconds
% <urn:example:person3> thinking for 0.15 seconds
% <urn:example:person2> thinking for 0.2 seconds
% <urn:example:person1> eating for 0.1 seconds
% <urn:example:person4> thinking for 0.25 seconds
% <urn:example:person3> eating for 0.1 seconds
% <urn:example:person5> thinking for 0.25 seconds
% <urn:example:person4> eating for 0.2 seconds
% <urn:example:person2> eating for 0.2 seconds
% <urn:example:person5> eating for 0.1 seconds
% answers
  answer('<urn:example:got>'('<urn:example:all>', '<urn:example:dinner>')).

% proof steps
  step(('<urn:example:got>'('<urn:example:all>', '<urn:example:dinner>'):+thread_create('<urn:example:run>'('<urn:example:person1>', 0.1, 0.1), A, []), thread_create('<urn:example:run>'('<urn:example:person2>', 0.2, 0.2), B, []), thread_create('<urn:example:run>'('<urn:example:person3>', 0.15, 0.1), C, []), thread_create('<urn:example:run>'('<urn:example:person4>', 0.25, 0.2), D, []), thread_create('<urn:example:run>'('<urn:example:person5>', 0.25, 0.1), E, []), thread_join(A, _), thread_join(B, _), thread_join(C, _), thread_join(D, _), thread_join(E, _)),
       (thread_create('<urn:example:run>'('<urn:example:person1>', 0.1, 0.1), '$BLOB'("<thread>(3,0x564742e252c0)"), []), thread_create('<urn:example:run>'('<urn:example:person2>', 0.2, 0.2), '$BLOB'("<thread>(4,0x564742e25310)"), []), thread_create('<urn:example:run>'('<urn:example:person3>', 0.15, 0.1), '$BLOB'("<thread>(5,0x564742e25360)"), []), thread_create('<urn:example:run>'('<urn:example:person4>', 0.25, 0.2), '$BLOB'("<thread>(6,0x564742e253b0)"), []), thread_create('<urn:example:run>'('<urn:example:person5>', 0.25, 0.1), '$BLOB'("<thread>(7,0x564742e24d70)"), []), thread_join('$BLOB'("<thread>(3,0x564742e252c0)"), true), thread_join('$BLOB'("<thread>(4,0x564742e25310)"), true), thread_join('$BLOB'("<thread>(5,0x564742e25360)"), true), thread_join('$BLOB'("<thread>(6,0x564742e253b0)"), true), thread_join('$BLOB'("<thread>(7,0x564742e24d70)"), true)),
       '<urn:example:got>'('<urn:example:all>', '<urn:example:dinner>')).
  step((true:+'<urn:example:got>'('<urn:example:all>', '<urn:example:dinner>')),
       '<urn:example:got>'('<urn:example:all>', '<urn:example:dinner>'),
       true).
