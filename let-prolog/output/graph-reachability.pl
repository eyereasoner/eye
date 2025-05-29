:- op(1200, xfx, :+).

answer(isReachable(a, f)).
answer(\+isReachable(b, e)).
answer(isReachable(c, g)).

step((true:+isReachable(a, f)), isReachable(a, f), true).
step((true:+ \+isReachable(b, e)), \+isReachable(b, e), true).
step((true:+isReachable(c, g)), isReachable(c, g), true).
