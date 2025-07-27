sdcoding(1,1) .
sdcoding(3,3) .
sdcoding(0,0) .
sdcoding(2,2) .

step(rule(sdc(A, B), sdconot(A, B)), sdc(0, 1), sdconot(0, 1)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(0, 3), sdconot(0, 3)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(1, 0), sdconot(1, 0)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(1, 1), sdconot(1, 1)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(1, 2), sdconot(1, 2)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(2, 1), sdconot(2, 1)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(2, 3), sdconot(2, 3)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(3, 1), sdconot(3, 1)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(3, 3), sdconot(3, 3)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(3, 0), sdconot(3, 0)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(3, 2), sdconot(3, 2)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(0, 0), sdconot(0, 0)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(2, 0), sdconot(2, 0)).
step(rule(sdc(A, B), sdconot(A, B)), sdc(2, 2), sdconot(2, 2)).
step(rule(sdcoding(A, B), answer(sdcoding(A, B))), sdcoding(1, 1), answer(sdcoding(1, 1))).
step(rule(sdcoding(A, B), answer(sdcoding(A, B))), sdcoding(3, 3), answer(sdcoding(3, 3))).
step(rule(sdcoding(A, B), answer(sdcoding(A, B))), sdcoding(0, 0), answer(sdcoding(0, 0))).
step(rule(sdcoding(A, B), answer(sdcoding(A, B))), sdcoding(2, 2), answer(sdcoding(2, 2))).
