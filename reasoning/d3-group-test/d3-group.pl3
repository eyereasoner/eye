% Dihedral group of order 6

:- op(1200, xfx, :+).

% the elements of the Dihedral Group D3 (Symmetries of an Equilateral Triangle)
'<urn:example:symmetry>'(identity).
'<urn:example:symmetry>'(rotation_120).
'<urn:example:symmetry>'(rotation_240).
'<urn:example:symmetry>'(reflection_a).
'<urn:example:symmetry>'(reflection_b).
'<urn:example:symmetry>'(reflection_c).

% composition table for D3
'<urn:example:compose>'(identity, X, X) :-
    '<urn:example:symmetry>'(X).
'<urn:example:compose>'(X, identity, X) :-
    '<urn:example:symmetry>'(X).

'<urn:example:compose>'(rotation_120, rotation_120, rotation_240).
'<urn:example:compose>'(rotation_120, rotation_240, identity).
'<urn:example:compose>'(rotation_240, rotation_120, identity).
'<urn:example:compose>'(rotation_240, rotation_240, rotation_120).

'<urn:example:compose>'(reflection_a, reflection_a, identity).
'<urn:example:compose>'(reflection_b, reflection_b, identity).
'<urn:example:compose>'(reflection_c, reflection_c, identity).

'<urn:example:compose>'(reflection_a, reflection_b, rotation_120).
'<urn:example:compose>'(reflection_a, reflection_c, rotation_240).
'<urn:example:compose>'(reflection_b, reflection_a, rotation_240).
'<urn:example:compose>'(reflection_b, reflection_c, rotation_120).
'<urn:example:compose>'(reflection_c, reflection_a, rotation_120).
'<urn:example:compose>'(reflection_c, reflection_b, rotation_240).

% inverse elements
'<urn:example:inverse>'(identity, identity).
'<urn:example:inverse>'(rotation_120, rotation_240).
'<urn:example:inverse>'(rotation_240, rotation_120).
'<urn:example:inverse>'(reflection_a, reflection_a).
'<urn:example:inverse>'(reflection_b, reflection_b).
'<urn:example:inverse>'(reflection_c, reflection_c).

% check if a set is a valid group
'<urn:example:validGroup>'(Group) :-
    findall(X, '<urn:example:symmetry>'(X), AllElements),
    subgroup(AllElements, Group),
    memberchk(identity, Group),
    forall(
        (   member(X, Group),
            member(Y, Group),
            '<urn:example:compose>'(X, Y, Z)
        ),
        member(Z, Group)
    ),
    forall(
        (   member(X, Group),
            '<urn:example:inverse>'(X, Y)
        ),
        member(Y, Group)
    ).

% find subgroups
subgroup([], []).
subgroup([H|T], [H|Sub]) :-
    subgroup(T, Sub).
subgroup([_|T], Sub) :-
    subgroup(T, Sub).

% query
true :+ '<urn:example:validGroup>'(_).
