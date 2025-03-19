% Dihedral group of order 6
% See https://en.wikipedia.org/wiki/Dihedral_group_of_order_6

:- op(1200, xfx, :+).

% the elements of the Dihedral Group D3 (Symmetries of an Equilateral Triangle)
symmetry(identity).
symmetry(rotation_120).
symmetry(rotation_240).
symmetry(reflection_a).
symmetry(reflection_b).
symmetry(reflection_c).

% composition table for D3
compose(identity, X, X) :-
    symmetry(X).
compose(X, identity, X) :-
    symmetry(X).

compose(rotation_120, rotation_120, rotation_240).
compose(rotation_120, rotation_240, identity).
compose(rotation_240, rotation_120, identity).
compose(rotation_240, rotation_240, rotation_120).

compose(reflection_a, reflection_a, identity).
compose(reflection_b, reflection_b, identity).
compose(reflection_c, reflection_c, identity).

compose(reflection_a, reflection_b, rotation_120).
compose(reflection_a, reflection_c, rotation_240).
compose(reflection_b, reflection_a, rotation_240).
compose(reflection_b, reflection_c, rotation_120).
compose(reflection_c, reflection_a, rotation_120).
compose(reflection_c, reflection_b, rotation_240).

% inverse elements
inversion(identity, identity).
inversion(rotation_120, rotation_240).
inversion(rotation_240, rotation_120).
inversion(reflection_a, reflection_a).
inversion(reflection_b, reflection_b).
inversion(reflection_c, reflection_c).

% check if a set is a valid group
'urn:example:validGroup'(Group) :-
    findall(X, symmetry(X), AllElements),
    subgroup(AllElements, Group),
    memberchk(identity, Group),
    forall(
        (   member(X, Group),
            member(Y, Group),
            compose(X, Y, Z)
        ),
        member(Z, Group)
    ),
    forall(
        (   member(X, Group),
            inversion(X, Y)
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
true :+ 'urn:example:validGroup'(_).
