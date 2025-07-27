mustHave(alice,dogLicense).

step(rule(mustHave(A, B), answer(mustHave(A, B))), mustHave(alice, dogLicense), answer(mustHave(alice, dogLicense))).
