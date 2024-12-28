% Deontic Logic example

% facts about what an employee is doing
'<urn:example:does>'('<urn:example:alice>', '<urn:example:log_off_at_end_of_shift>').
'<urn:example:does>'('<urn:example:bob>', '<urn:example:work_related_task>').
%'<urn:example:does>'('<urn:example:bob>', '<urn:example:log_off_at_end_of_shift>').
'<urn:example:does>'('<urn:example:carol>', '<urn:example:access_social_media>').

% rules to check if an action complies with deontic logic
'<urn:example:complies>'(Person, true) :+
    '<urn:example:does>'(Person, '<urn:example:work_related_task>'),
    '<urn:example:does>'(Person, '<urn:example:log_off_at_end_of_shift>').

'<urn:example:complies>'(Person, false) :+
    '<urn:example:does>'(Person, '<urn:example:work_related_task>'),
    stable(1),
    \+'<urn:example:does>'(Person, '<urn:example:log_off_at_end_of_shift>').

'<urn:example:complies>'(Person, true) :+
    '<urn:example:does>'(Person, '<urn:example:log_off_at_end_of_shift>').

'<urn:example:complies>'(Person, false) :+
    '<urn:example:does>'(Person, '<urn:example:access_social_media>').

% query
true :+ '<urn:example:complies>'(_, _).
