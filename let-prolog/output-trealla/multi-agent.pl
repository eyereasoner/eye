:- op(1200, xfx, :+).

answer(obligatory(complete_task(agent2,task1))).
answer(obligatory(escalate_task(agent1,task1))).
answer(permitted(execute_task(agent2,task1))).
answer(violation(task1)).
answer(sanction(agent2)).

step((true:+obligatory(A)),obligatory(complete_task(agent2,task1)),true).
step((true:+obligatory(A)),obligatory(escalate_task(agent1,task1)),true).
step((true:+permitted(A)),permitted(execute_task(agent2,task1)),true).
step((true:+violation(A)),violation(task1),true).
step((true:+sanction(A)),sanction(agent2),true).
