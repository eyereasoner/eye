% Deontic Logic example benchmark

% facts about what employees are doing
'<urn:example:prepare>'(X, Y) :-
    between(X, Y, N),
    atom_number(I, N),
    atomic_list_concat(['<urn:example:alice', I, '>'], A),
    (   N mod 2 =:= 0,
        \+'<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>')
    ->  assertz('<urn:example:does>'(A, '<urn:example:log_off_at_end_of_shift>'))
    ;   true
    ),
    atomic_list_concat(['<urn:example:bob', I, '>'], B),
    (   N mod 4 =:= 0,
        \+'<urn:example:does>'(B, '<urn:example:work_related_task>')
    ->  assertz('<urn:example:does>'(B, '<urn:example:work_related_task>'))
    ;   true
    ),
    (   N mod 8 =:= 0,
        \+'<urn:example:does>'(B, '<urn:example:log_off_at_end_of_shift>')
    ->   assertz('<urn:example:does>'(B, '<urn:example:log_off_at_end_of_shift>'))
    ;   true
    ),
    atomic_list_concat(['<urn:example:carol', I, '>'], I, C),
    (   N mod 4 =:= 0,
        \+'<urn:example:does>'(C, '<urn:example:access_social_media>')
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
true :+ '<urn:example:prepare>'(1, 16).

% query
true :+ '<urn:example:complies>'(_, _).
