% Dihedral group of order 6

% the elements of the Dihedral Group D3 (Symmetries of an Equilateral Triangle)
'<https://eyereasoner.github.io/ns#symmetry>'(identity).
'<https://eyereasoner.github.io/ns#symmetry>'(rotation_120).
'<https://eyereasoner.github.io/ns#symmetry>'(rotation_240).
'<https://eyereasoner.github.io/ns#symmetry>'(reflection_a).
'<https://eyereasoner.github.io/ns#symmetry>'(reflection_b).
'<https://eyereasoner.github.io/ns#symmetry>'(reflection_c).

% composition table for D3
'<https://eyereasoner.github.io/ns#compose>'(identity, X, X) :-
    '<https://eyereasoner.github.io/ns#symmetry>'(X).
'<https://eyereasoner.github.io/ns#compose>'(X, identity, X) :-
    '<https://eyereasoner.github.io/ns#symmetry>'(X).

'<https://eyereasoner.github.io/ns#compose>'(rotation_120, rotation_120, rotation_240).
'<https://eyereasoner.github.io/ns#compose>'(rotation_120, rotation_240, identity).
'<https://eyereasoner.github.io/ns#compose>'(rotation_240, rotation_120, identity).
'<https://eyereasoner.github.io/ns#compose>'(rotation_240, rotation_240, rotation_120).

'<https://eyereasoner.github.io/ns#compose>'(reflection_a, reflection_a, identity).
'<https://eyereasoner.github.io/ns#compose>'(reflection_b, reflection_b, identity).
'<https://eyereasoner.github.io/ns#compose>'(reflection_c, reflection_c, identity).

'<https://eyereasoner.github.io/ns#compose>'(reflection_a, reflection_b, rotation_120).
'<https://eyereasoner.github.io/ns#compose>'(reflection_a, reflection_c, rotation_240).
'<https://eyereasoner.github.io/ns#compose>'(reflection_b, reflection_a, rotation_240).
'<https://eyereasoner.github.io/ns#compose>'(reflection_b, reflection_c, rotation_120).
'<https://eyereasoner.github.io/ns#compose>'(reflection_c, reflection_a, rotation_120).
'<https://eyereasoner.github.io/ns#compose>'(reflection_c, reflection_b, rotation_240).

% inverse elements
'<https://eyereasoner.github.io/ns#inverse>'(identity, identity).
'<https://eyereasoner.github.io/ns#inverse>'(rotation_120, rotation_240).
'<https://eyereasoner.github.io/ns#inverse>'(rotation_240, rotation_120).
'<https://eyereasoner.github.io/ns#inverse>'(reflection_a, reflection_a).
'<https://eyereasoner.github.io/ns#inverse>'(reflection_b, reflection_b).
'<https://eyereasoner.github.io/ns#inverse>'(reflection_c, reflection_c).

% check if a set is a valid group
'<https://eyereasoner.github.io/ns#validGroup>'(Group, true) :-
    findall(X, '<https://eyereasoner.github.io/ns#symmetry>'(X), AllElements),
    subgroup(AllElements, Group),
    memberchk(identity, Group),
    forall(
        (   member(X, Group),
            member(Y, Group),
            '<https://eyereasoner.github.io/ns#compose>'(X, Y, Z)
        ),
        member(Z, Group)
    ),
    forall(
        (   member(X, Group),
            '<https://eyereasoner.github.io/ns#inverse>'(X, Y)
        ),
        member(Y, Group)
    ).

% find subgroups
subgroup([], []).
subgroup([H|T], [H|Sub]) :-
    subgroup(T, Sub).
subgroup([_|T], Sub) :-
    subgroup(T, Sub).
