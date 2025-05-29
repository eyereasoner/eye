:- op(1200, xfx, :+).

answer(sdcoding(1, 1)).
answer(sdcoding(3, 3)).
answer(sdcoding(0, 0)).
answer(sdcoding(2, 2)).

step((sdconot(A, B):+sdc(A, B)), sdc(0, 1), sdconot(0, 1)).
step((sdconot(A, B):+sdc(A, B)), sdc(0, 3), sdconot(0, 3)).
step((sdconot(A, B):+sdc(A, B)), sdc(1, 0), sdconot(1, 0)).
step((sdconot(A, B):+sdc(A, B)), sdc(1, 1), sdconot(1, 1)).
step((sdconot(A, B):+sdc(A, B)), sdc(1, 2), sdconot(1, 2)).
step((sdconot(A, B):+sdc(A, B)), sdc(2, 1), sdconot(2, 1)).
step((sdconot(A, B):+sdc(A, B)), sdc(2, 3), sdconot(2, 3)).
step((sdconot(A, B):+sdc(A, B)), sdc(3, 1), sdconot(3, 1)).
step((sdconot(A, B):+sdc(A, B)), sdc(3, 3), sdconot(3, 3)).
step((sdconot(A, B):+sdc(A, B)), sdc(3, 0), sdconot(3, 0)).
step((sdconot(A, B):+sdc(A, B)), sdc(3, 2), sdconot(3, 2)).
step((sdconot(A, B):+sdc(A, B)), sdc(0, 0), sdconot(0, 0)).
step((sdconot(A, B):+sdc(A, B)), sdc(2, 0), sdconot(2, 0)).
step((sdconot(A, B):+sdc(A, B)), sdc(2, 2), sdconot(2, 2)).
step((true:+sdcoding(_, _)), sdcoding(1, 1), true).
step((true:+sdcoding(_, _)), sdcoding(3, 3), true).
step((true:+sdcoding(_, _)), sdcoding(0, 0), true).
step((true:+sdcoding(_, _)), sdcoding(2, 2), true).
