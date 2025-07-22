:- op(1200, xfx, :+).

answer(needs_maintenance(t456,time_based_maintenance)).
answer(needs_maintenance(t456,age_based_maintenance)).
answer(needs_maintenance(t456,problem_based_maintenance)).
answer(needs_maintenance(t789,problem_based_maintenance)).
answer(needs_maintenance(t456,performance_based_maintenance)).
answer(needs_maintenance(t101,performance_based_maintenance)).

step((needs_maintenance(A,time_based_maintenance):+turbine(A,B,C,D,E),C>=12),(turbine(t456,12,15,[vibration],78),15>=12),needs_maintenance(t456,time_based_maintenance)).
step((needs_maintenance(A,age_based_maintenance):+turbine(A,B,C,D,E),B>10,C>=6),(turbine(t456,12,15,[vibration],78),12>10,15>=6),needs_maintenance(t456,age_based_maintenance)).
step((needs_maintenance(A,problem_based_maintenance):+turbine(A,B,C,D,E),D\=[]),(turbine(t456,12,15,[vibration],78),[vibration]\=[]),needs_maintenance(t456,problem_based_maintenance)).
step((needs_maintenance(A,problem_based_maintenance):+turbine(A,B,C,D,E),D\=[]),(turbine(t789,8,7,[noise],85),[noise]\=[]),needs_maintenance(t789,problem_based_maintenance)).
step((needs_maintenance(A,performance_based_maintenance):+turbine(A,B,C,D,E),E<80),(turbine(t456,12,15,[vibration],78),78<80),needs_maintenance(t456,performance_based_maintenance)).
step((needs_maintenance(A,performance_based_maintenance):+turbine(A,B,C,D,E),E<80),(turbine(t101,15,5,[],65),65<80),needs_maintenance(t101,performance_based_maintenance)).
step((true:+needs_maintenance(A,B)),needs_maintenance(t456,time_based_maintenance),true).
step((true:+needs_maintenance(A,B)),needs_maintenance(t456,age_based_maintenance),true).
step((true:+needs_maintenance(A,B)),needs_maintenance(t456,problem_based_maintenance),true).
step((true:+needs_maintenance(A,B)),needs_maintenance(t789,problem_based_maintenance),true).
step((true:+needs_maintenance(A,B)),needs_maintenance(t456,performance_based_maintenance),true).
step((true:+needs_maintenance(A,B)),needs_maintenance(t101,performance_based_maintenance),true).
