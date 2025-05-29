% Control Systems

:- op(1200, xfx, :+).

% measurements
measurement1(input1, [6, 11]).
measurement1(disturbance2, [45, 39]).
measurement2(input2, true).
measurement3(input3, 56967).
measurement3(disturbance1, 35766).
measurement4(output2, 24).

% observations
observation1(state1, 80).
observation2(state2, false).
observation3(state3, 22).

% targets
target2(output2, 29).

% rules
control1(actuator1, C) :-
    measurement10(input1, M1),
    measurement2(input2, true),
    measurement3(disturbance1, D1),
    C1 is M1*19.6,          % proportial part
    C2 is log(D1)/log(10),  % compensation part
    C is C1-C2.             % simple feedforward control

control1(actuator2, C) :-
    observation3(state3, P3),
    measurement4(output2, M4),
    target2(output2, T2),
    E is T2-M4,             % error
    D is P3-M4,             % differential error
    C1 is 5.8*E,            % proportial part
    N is 7.3/E,             % nonlinear factor
    C2 is N*D,              % nonlinear differential part
    C is C1+C2.             % PND feedback control

measurement10(I, M) :-
    measurement1(I, [M1, M2]),
    M1 < M2,
    M3 is M2-M1,
    M is sqrt(M3).

measurement10(I, M1) :-
    measurement1(I, [M1, M2]),
    M1 >= M2.

% query
true :+ control1(_, _).
