:- op(1200, xfx, :+).

answer(validGroup([identity,rotation_120,rotation_240,reflection_a,reflection_b,reflection_c])).
answer(validGroup([identity,rotation_120,rotation_240,reflection_a,reflection_b])).
answer(validGroup([identity,rotation_120,rotation_240,reflection_a,reflection_c])).
answer(validGroup([identity,rotation_120,rotation_240,reflection_a])).
answer(validGroup([identity,rotation_120,rotation_240,reflection_b,reflection_c])).
answer(validGroup([identity,rotation_120,rotation_240,reflection_b])).
answer(validGroup([identity,rotation_120,rotation_240,reflection_c])).
answer(validGroup([identity,rotation_120,rotation_240])).
answer(validGroup([identity,reflection_a])).
answer(validGroup([identity,reflection_b])).
answer(validGroup([identity,reflection_c])).
answer(validGroup([identity])).

step((true:+validGroup(A)),validGroup([identity,rotation_120,rotation_240,reflection_a,reflection_b,reflection_c]),true).
step((true:+validGroup(A)),validGroup([identity,rotation_120,rotation_240,reflection_a,reflection_b]),true).
step((true:+validGroup(A)),validGroup([identity,rotation_120,rotation_240,reflection_a,reflection_c]),true).
step((true:+validGroup(A)),validGroup([identity,rotation_120,rotation_240,reflection_a]),true).
step((true:+validGroup(A)),validGroup([identity,rotation_120,rotation_240,reflection_b,reflection_c]),true).
step((true:+validGroup(A)),validGroup([identity,rotation_120,rotation_240,reflection_b]),true).
step((true:+validGroup(A)),validGroup([identity,rotation_120,rotation_240,reflection_c]),true).
step((true:+validGroup(A)),validGroup([identity,rotation_120,rotation_240]),true).
step((true:+validGroup(A)),validGroup([identity,reflection_a]),true).
step((true:+validGroup(A)),validGroup([identity,reflection_b]),true).
step((true:+validGroup(A)),validGroup([identity,reflection_c]),true).
step((true:+validGroup(A)),validGroup([identity]),true).
