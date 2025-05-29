:- op(1200, xfx, :+).

answer(ascribed(test,true)).

step(((ascribed(test,true):+type(A,'Dog')):+type(B,'Cat')),type('Minka','Cat'),(ascribed(test,true):+type(C,'Dog'))).
step((ascribed(test,true):+type(A,'Dog')),type('Charly','Dog'),ascribed(test,true)).
step((true:+ascribed(test,true)),ascribed(test,true),true).
