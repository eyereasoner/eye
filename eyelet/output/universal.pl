:- op(1200, xfx, :+).

answer(type('Pat','Resource')).
answer(loves('Bob',lonely(skolem('Bob')))).

step((true:+type('Pat','Resource')),type('Pat','Resource'),true).
step((true:+loves('Bob',A)),loves('Bob',lonely(skolem('Bob'))),true).
