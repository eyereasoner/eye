:- op(1200, xfx, :+).

answer(control1(actuator1, 39.27346198678276)).
answer(control1(actuator2, 26.08)).

step((true:+control1(_, _)), control1(actuator1, 39.27346198678276), true).
step((true:+control1(_, _)), control1(actuator2, 26.08), true).
