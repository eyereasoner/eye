set_random(seed(100)).
solve(METHINKS IT IS LIKE A WEASEL).

step(rule(set_random(seed(100)), answer(set_random(seed(100)))), set_random(seed(100)), answer(set_random(seed(100)))).
step(rule(solve('METHINKS IT IS LIKE A WEASEL'), answer(solve('METHINKS IT IS LIKE A WEASEL'))), solve('METHINKS IT IS LIKE A WEASEL'), answer(solve('METHINKS IT IS LIKE A WEASEL'))).
