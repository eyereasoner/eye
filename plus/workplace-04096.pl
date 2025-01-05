% Deontic Logic example benchmark

% facts about what employees are doing
'<urn:example:prepare>'(X, Y) :-
    between(X, Y, N),
    atom_number(I, N),
    atom_concat(alice, I, A),
    (   \+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')
    ->  assertz('<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>'))
    ;   true
    ),
    atom_concat(bob, I, B),
    (   \+'<urn:example:does>'(B, '<urn:example:work_related_task>')
    ->  assertz('<urn:example:does>'(B, '<urn:example:work_related_task>'))
    ;   true
    ),
    (   \+'<urn:example:does>'(B, '<urn:example:log_off_at_end_of_shift>')
    ->   assertz('<urn:example:does>'(B, '<urn:example:log_off_at_end_of_shift>'))
    ;   true
    ),
    atom_concat(carol, I, C),
    (   \+'<urn:example:does>'(C, '<urn:example:access_social_media>')
    ->  assertz('<urn:example:does>'(C, '<urn:example:access_social_media>'))
    ;   true
    ),
    fail;
    true.

% rules to check if an action complies with deontic logic
'<urn:example:complies>'(Person, true) :+
    '<urn:example:does>'(Person, '<urn:example:work_related_task>'),
    '<urn:example:does>'(Person, '<urn:example:log_off_at_end_of_shift>').

'<urn:example:complies>'(Person, true) :+
    '<urn:example:does>'(Person, '<urn:example:log_off_at_end_of_shift>').

'<urn:example:complies>'(Person, false) :+
    '<urn:example:does>'(Person, '<urn:example:work_related_task>'),
    stable(1),
    \+'<urn:example:does>'(Person, '<urn:example:log_off_at_end_of_shift>').

'<urn:example:complies>'(Person, false) :+
    '<urn:example:does>'(Person, '<urn:example:access_social_media>').

% prepare employee data
true :+ '<urn:example:prepare>'(1, 4096).

% query
true :+ '<urn:example:complies>'(_, _).
