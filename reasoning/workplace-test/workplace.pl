% Deontic Logic example

:- op(1200, xfx, :+).

% Facts about what employees are doing
'<https://eyereasoner.github.io/ns#prepare>'(X, Y) :-
    between(X, Y, N),
    number_codes(N, J),
    atom_codes(I, J),
    atom_concat('<https://eyereasoner.github.io/ns#alice>', I, A),
    assertz('<https://eyereasoner.github.io/ns#does>'(A, '<https://eyereasoner.github.io/ns#log_off_at_end_of_shift>')),
    atom_concat('<https://eyereasoner.github.io/ns#bob>', I, B),
    assertz('<https://eyereasoner.github.io/ns#does>'(B, '<https://eyereasoner.github.io/ns#work_related_task>')),
    assertz('<https://eyereasoner.github.io/ns#does>'(B, '<https://eyereasoner.github.io/ns#log_off_at_end_of_shift>')),
    atom_concat('<https://eyereasoner.github.io/ns#carol>', I, C),
    assertz('<https://eyereasoner.github.io/ns#does>'(C, '<https://eyereasoner.github.io/ns#access_social_media>')),
    fail;
    true.

% Rules to check if an action complies with deontic logic
'<https://eyereasoner.github.io/ns#complies>'(Person, true) :+
    '<https://eyereasoner.github.io/ns#does>'(Person, '<https://eyereasoner.github.io/ns#work_related_task>'),
    '<https://eyereasoner.github.io/ns#does>'(Person, '<https://eyereasoner.github.io/ns#log_off_at_end_of_shift>').

'<https://eyereasoner.github.io/ns#complies>'(Person, false) :+
    '<https://eyereasoner.github.io/ns#does>'(Person, '<https://eyereasoner.github.io/ns#work_related_task>'),
    stable(1),
    \+'<https://eyereasoner.github.io/ns#does>'(Person, '<https://eyereasoner.github.io/ns#log_off_at_end_of_shift>').

'<https://eyereasoner.github.io/ns#complies>'(Person, true) :+
    '<https://eyereasoner.github.io/ns#does>'(Person, '<https://eyereasoner.github.io/ns#log_off_at_end_of_shift>').

'<https://eyereasoner.github.io/ns#complies>'(Person, false) :+
    '<https://eyereasoner.github.io/ns#does>'(Person, '<https://eyereasoner.github.io/ns#access_social_media>').


% prepare employee data
true :+ '<https://eyereasoner.github.io/ns#prepare>'(1, 5000).

% Query to test if everyone complies with deontic logic
true :+ '<https://eyereasoner.github.io/ns#complies>'(_, _).
