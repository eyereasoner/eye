obligatory(complete_task(agent2,task1)).
obligatory(escalate_task(agent1,task1)).
permitted(execute_task(agent2,task1)).
violation(task1).
sanction(agent2).

step(rule(obligatory(A), answer(obligatory(A))), obligatory(complete_task(agent2, task1)), answer(obligatory(complete_task(agent2, task1)))).
step(rule(obligatory(A), answer(obligatory(A))), obligatory(escalate_task(agent1, task1)), answer(obligatory(escalate_task(agent1, task1)))).
step(rule(permitted(A), answer(permitted(A))), permitted(execute_task(agent2, task1)), answer(permitted(execute_task(agent2, task1)))).
step(rule(violation(A), answer(violation(A))), violation(task1), answer(violation(task1))).
step(rule(sanction(A), answer(sanction(A))), sanction(agent2), answer(sanction(agent2))).
