:- op(1200, xfx, :+).

answer((ascribed(_, good(cobbler)):+true)).

step((ascribed(_, good(cobbler)):+true), true, ascribed(sk_0, good(cobbler))).
step((true:+(ascribed(_, good(_)):+true)), (ascribed(_, good(cobbler)):+true), true).
