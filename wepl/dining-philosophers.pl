% Dining Philosophers
% See http://www.cs.utk.edu/~plank/plank/classes/cs560/560/notes/Dphil/lecture.html

'<urn:example:chopstick>'('<urn:example:person1>', '<urn:example:chopstick1>', '<urn:example:chopstick5>').
'<urn:example:chopstick>'('<urn:example:person2>', '<urn:example:chopstick2>', '<urn:example:chopstick1>').
'<urn:example:chopstick>'('<urn:example:person3>', '<urn:example:chopstick3>', '<urn:example:chopstick2>').
'<urn:example:chopstick>'('<urn:example:person4>', '<urn:example:chopstick4>', '<urn:example:chopstick3>').
'<urn:example:chopstick>'('<urn:example:person5>', '<urn:example:chopstick5>', '<urn:example:chopstick4>').

'<urn:example:pickup>'(A) :-
    '<urn:example:chopstick>'(A, B, C),
    mutex_lock(B),
    mutex_lock(C).

'<urn:example:putdown>'(A) :-
    '<urn:example:chopstick>'(A, B, C),
    mutex_unlock(C),
    mutex_unlock(B).

'<urn:example:run>'(A, B, C) :-
    sleep(B),
    format('% ~w thinking for ~w seconds~n', [A, B]),
    '<urn:example:pickup>'(A),
    sleep(C),
    format('% ~w eating for ~w seconds~n', [A, C]),
    '<urn:example:putdown>'(A).

'<urn:example:got>'('<urn:example:all>', '<urn:example:dinner>') :+
    thread_create('<urn:example:run>'('<urn:example:person1>', 0.1, 0.1), A, []),
    thread_create('<urn:example:run>'('<urn:example:person2>', 0.2, 0.2), B, []),
    thread_create('<urn:example:run>'('<urn:example:person3>', 0.15, 0.1), C, []),
    thread_create('<urn:example:run>'('<urn:example:person4>', 0.25, 0.2), D, []),
    thread_create('<urn:example:run>'('<urn:example:person5>', 0.25, 0.1), E, []),
    thread_join(A, _),
    thread_join(B, _),
    thread_join(C, _),
    thread_join(D, _),
    thread_join(E, _).

% query
true :+ '<urn:example:got>'('<urn:example:all>', '<urn:example:dinner>').
