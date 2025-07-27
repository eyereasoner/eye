needs_maintenance(t456,time_based_maintenance).
needs_maintenance(t456,age_based_maintenance).
needs_maintenance(t456,problem_based_maintenance).
needs_maintenance(t789,problem_based_maintenance).
needs_maintenance(t456,performance_based_maintenance).
needs_maintenance(t101,performance_based_maintenance).

step(rule((turbine(A, _, B, _, _), B>=12), needs_maintenance(A, time_based_maintenance)), (turbine(t456, 12, 15, [vibration], 78), 15>=12), needs_maintenance(t456, time_based_maintenance)).
step(rule((turbine(A, B, C, _, _), B>10, C>=6), needs_maintenance(A, age_based_maintenance)), (turbine(t456, 12, 15, [vibration], 78), 12>10, 15>=6), needs_maintenance(t456, age_based_maintenance)).
step(rule((turbine(A, _, _, B, _), B\=[]), needs_maintenance(A, problem_based_maintenance)), (turbine(t456, 12, 15, [vibration], 78), [vibration]\=[]), needs_maintenance(t456, problem_based_maintenance)).
step(rule((turbine(A, _, _, B, _), B\=[]), needs_maintenance(A, problem_based_maintenance)), (turbine(t789, 8, 7, [noise], 85), [noise]\=[]), needs_maintenance(t789, problem_based_maintenance)).
step(rule((turbine(A, _, _, _, B), B<80), needs_maintenance(A, performance_based_maintenance)), (turbine(t456, 12, 15, [vibration], 78), 78<80), needs_maintenance(t456, performance_based_maintenance)).
step(rule((turbine(A, _, _, _, B), B<80), needs_maintenance(A, performance_based_maintenance)), (turbine(t101, 15, 5, [], 65), 65<80), needs_maintenance(t101, performance_based_maintenance)).
step(rule(needs_maintenance(A, B), answer(needs_maintenance(A, B))), needs_maintenance(t456, time_based_maintenance), answer(needs_maintenance(t456, time_based_maintenance))).
step(rule(needs_maintenance(A, B), answer(needs_maintenance(A, B))), needs_maintenance(t456, age_based_maintenance), answer(needs_maintenance(t456, age_based_maintenance))).
step(rule(needs_maintenance(A, B), answer(needs_maintenance(A, B))), needs_maintenance(t456, problem_based_maintenance), answer(needs_maintenance(t456, problem_based_maintenance))).
step(rule(needs_maintenance(A, B), answer(needs_maintenance(A, B))), needs_maintenance(t789, problem_based_maintenance), answer(needs_maintenance(t789, problem_based_maintenance))).
step(rule(needs_maintenance(A, B), answer(needs_maintenance(A, B))), needs_maintenance(t456, performance_based_maintenance), answer(needs_maintenance(t456, performance_based_maintenance))).
step(rule(needs_maintenance(A, B), answer(needs_maintenance(A, B))), needs_maintenance(t101, performance_based_maintenance), answer(needs_maintenance(t101, performance_based_maintenance))).
