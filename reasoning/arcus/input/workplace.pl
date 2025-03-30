% Deontic Logic example

:- op(1200, xfx, :+).

:- dynamic('urn:example:does'/2).
:- dynamic('urn:example:complies'/2).

% Facts about what employees are doing
'urn:example:prepare'(X, Y) :-
    between(X, Y, N),
    number_codes(N, J),
    atom_codes(I, J),
    atomic_list_concat(['urn:example:alice', I, ''], A),
    assertz('urn:example:does'(A, 'urn:example:log_off_at_end_of_shift')),
    atomic_list_concat(['urn:example:bob', I, ''], B),
    assertz('urn:example:does'(B, 'urn:example:work_related_task')),
    assertz('urn:example:does'(B, 'urn:example:log_off_at_end_of_shift')),
    atomic_list_concat(['urn:example:carol', I, ''], C),
    assertz('urn:example:does'(C, 'urn:example:access_social_media')),
    fail;
    true.

% Rules to check if an action complies with deontic logic
'urn:example:complies'(Person, true) :+
    'urn:example:does'(Person, 'urn:example:work_related_task'),
    'urn:example:does'(Person, 'urn:example:log_off_at_end_of_shift').

'urn:example:complies'(Person, false) :+
    'urn:example:does'(Person, 'urn:example:work_related_task'),
    stable(1),
    \+'urn:example:does'(Person, 'urn:example:log_off_at_end_of_shift').

'urn:example:complies'(Person, true) :+
    'urn:example:does'(Person, 'urn:example:log_off_at_end_of_shift').

'urn:example:complies'(Person, false) :+
    'urn:example:does'(Person, 'urn:example:access_social_media').


% prepare employee data
true :+
    'urn:example:prepare'(1, 1000).

% Query to test if everyone complies with deontic logic
true :+
    'urn:example:complies'(_, _).
