:- op(1200, xfx, :+).

answer(sum(4096, 8390656)).

step((true:+sum(4096, _)), sum(4096, 8390656), true).
