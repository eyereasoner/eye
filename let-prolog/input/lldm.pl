% ----------------------------------
% Leg Length Discrepancy Measurement
% ----------------------------------
%
% See http://www.agfa.com/w3c/2002/10/medicad/op/
%

:- op(1200, xfx, :+).

/* ----------  raw data ---------- */

%  measurement(ID,
%              P1x , P1y , P2x , P2y , P3x , P3y , P4x , P4y).
measurement(meas47,
            10.1 ,  7.8 , 45.1 ,  5.6 ,  3.6 , 29.8 , 54.7 , 28.5).

/* ----------  helper predicates ---------- */

dx12(M,DX) :- measurement(M,P1x,_,P2x,_,_,_,_,_), DX is P1x - P2x.
dy12(M,DY) :- measurement(M,_,P1y,_,P2y,_,_,_,_), DY is P1y - P2y.
dy13(M,DY) :- measurement(M,_,P1y,_,_,_,P3y,_,_), DY is P1y - P3y.
dy24(M,DY) :- measurement(M,_,_,_,P2y,_,_,_,P4y), DY is P2y - P4y.

cL1(M,C)   :- dy12(M,DY), dx12(M,DX), C  is DY / DX.
dL3m(M,D)  :- cL1(M,C),  D  is 1 / C.
cL3(M,C)   :- dL3m(M,D), C  is -D.

pL1x1(M,V) :- cL1(M,C), measurement(M,P1x,_,_,_,_,_,_,_), V is C * P1x.
pL1x2(M,V) :- cL1(M,C), measurement(M,_,_,P2x,_,_,_,_,_), V is C * P2x.
pL3x3(M,V) :- cL3(M,C), measurement(M,_,_,_,_,P3x,_,_,_), V is C * P3x.
pL3x4(M,V) :- cL3(M,C), measurement(M,_,_,_,_,_,_,P4x,_), V is C * P4x.

dd13(M,V)  :- pL1x1(M,A), pL3x3(M,B), V is A - B.
ddy13(M,V) :- dd13(M,A), dy13(M,B), V is A - B.

dd24(M,V)  :- pL1x2(M,A), pL3x4(M,B), V is A - B.
ddy24(M,V) :- dd24(M,A), dy24(M,B), V is A - B.

ddL13(M,V) :- cL1(M,A), cL3(M,B), V is A - B.

/* ----------  intersection points p5 & p6 ---------- */

p5x(M,V)   :- ddy13(M,A), ddL13(M,B), V is A / B.
dx51(M,V)  :- p5x(M,P5x), measurement(M,P1x,_,_,_,_,_,_,_), V is P5x - P1x.
pL1dx51(M,V):- cL1(M,C), dx51(M,D), V is C * D.
p5y(M,V)   :- pL1dx51(M,A), measurement(M,_,P1y,_,_,_,_,_,_), V is A + P1y.

p6x(M,V)   :- ddy24(M,A), ddL13(M,B), V is A / B.
dx62(M,V)  :- p6x(M,P6x), measurement(M,_,_,P2x,_,_,_,_,_), V is P6x - P2x.
pL1dx62(M,V):- cL1(M,C), dx62(M,D), V is C * D.
p6y(M,V)   :- pL1dx62(M,A), measurement(M,_,_,_,P2y,_,_,_,_), V is A + P2y.

/* ----------  deltas to build the two long diagonals ---------- */

dx53(M,V)  :- p5x(M,P5x), measurement(M,_,_,_,_,P3x,_,_,_), V is P5x - P3x.
dx64(M,V)  :- p6x(M,P6x), measurement(M,_,_,_,_,_,_,P4x,_), V is P6x - P4x.
dy53(M,V)  :- p5y(M,P5y), measurement(M,_,_,_,_,_,P3y,_,_), V is P5y - P3y.
dy64(M,V)  :- p6y(M,P6y), measurement(M,_,_,_,_,_,_,_,P4y), V is P6y - P4y.

/* ----------  Euclidean lengths of those diagonals ---------- */

d53(M,D)   :- dx53(M,DX), dy53(M,DY), D is sqrt(DX*DX + DY*DY).
d64(M,D)   :- dx64(M,DX), dy64(M,DY), D is sqrt(DX*DX + DY*DY).

/* ----------  final diagnostic delta & alarm ---------- */

d(M,Delta) :- d53(M,A), d64(M,B), Delta is A - B.

lld_alarm(M) :+
    d(M,Delta),
    ( Delta < -1.25 ; Delta >  1.25 ).

/*  ----------  query ---------- */
true :+ lld_alarm(meas47).
