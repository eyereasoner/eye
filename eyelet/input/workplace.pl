% Deontic Logic example

:- op(1200, xfx, :+).

:- dynamic(does/2).
:- dynamic(complies/2).

% Facts about what employees are doing
prepare(X, Y) :-
    between(X, Y, N),
    number_codes(N, J),
    atom_codes(I, J),
    atom_concat(alice, I, A),
    assertz(does(A, log_off_at_end_of_shift)),
    atom_concat(bob, I, B),
    assertz(does(B, work_related_task)),
    assertz(does(B, log_off_at_end_of_shift)),
    atom_concat(carol, I, C),
    assertz(does(C, access_social_media)),
    fail;
    true.

% Rules to check if an action complies with deontic logic
complies(Person, true) :+
    does(Person, work_related_task),
    does(Person, log_off_at_end_of_shift).

complies(Person, false) :+
    does(Person, work_related_task),
    stable(1),
    \+does(Person, log_off_at_end_of_shift).

complies(Person, true) :+
    does(Person, log_off_at_end_of_shift).

complies(Person, false) :+
    does(Person, access_social_media).


% prepare employee data
true :+
    prepare(1, 1000).

% Query to test if everyone complies with deontic logic
true :+
    complies(_, _).
