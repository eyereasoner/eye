% Control Systems

:- op(1200, xfx, :+).

% measurements
'<urn:example:measurement1>'('<urn:example:input1>', [6, 11]).
'<urn:example:measurement1>'('<urn:example:disturbance2>', [45, 39]).
'<urn:example:measurement2>'('<urn:example:input2>', true).
'<urn:example:measurement3>'('<urn:example:input3>', 56967).
'<urn:example:measurement3>'('<urn:example:disturbance1>', 35766).
'<urn:example:measurement4>'('<urn:example:output2>', 24).

% observations
'<urn:example:observation1>'('<urn:example:state1>', 80).
'<urn:example:observation2>'('<urn:example:state2>', false).
'<urn:example:observation3>'('<urn:example:state3>', 22).

% targets
'<urn:example:target2>'('<urn:example:output2>', 29).

% rules
'<urn:example:control1>'('<urn:example:actuator1>', C) :-
    '<urn:example:measurement10>'('<urn:example:input1>', M1),
    '<urn:example:measurement2>'('<urn:example:input2>', true),
    '<urn:example:measurement3>'('<urn:example:disturbance1>', D1),
    C1 is M1*19.6,          % proportial part
    C2 is log(D1)/log(10),  % compensation part
    C is C1-C2.             % simple feedforward control

'<urn:example:control1>'('<urn:example:actuator2>', C) :-
    '<urn:example:observation3>'('<urn:example:state3>', P3),
    '<urn:example:measurement4>'('<urn:example:output2>', M4),
    '<urn:example:target2>'('<urn:example:output2>', T2),
    E is T2-M4,             % error
    D is P3-M4,             % differential error
    C1 is 5.8*E,            % proportial part
    N is 7.3/E,             % nonlinear factor
    C2 is N*D,              % nonlinear differential part
    C is C1+C2.             % PND feedback control

'<urn:example:measurement10>'(I, M) :-
    '<urn:example:measurement1>'(I, [M1, M2]),
    M1 < M2,
    M3 is M2-M1,
    M is sqrt(M3).

'<urn:example:measurement10>'(I, M1) :-
    '<urn:example:measurement1>'(I, [M1, M2]),
    M1 >= M2.

% query
true :+ '<urn:example:control1>'(_, _).
