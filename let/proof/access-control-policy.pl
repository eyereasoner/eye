policy(PolicyX).
pass(PolicyX,allOfTest).
pass(PolicyX,anyOfTest).
pass(PolicyX,noneOfTest).

step(rule((policy(A), pass(A, allOfTest), pass(A, anyOfTest), pass(A, noneOfTest)), answer((policy(A), pass(A, allOfTest), pass(A, anyOfTest), pass(A, noneOfTest)))), (policy('PolicyX'), pass('PolicyX', allOfTest), pass('PolicyX', anyOfTest), pass('PolicyX', noneOfTest)), answer((policy('PolicyX'), pass('PolicyX', allOfTest), pass('PolicyX', anyOfTest), pass('PolicyX', noneOfTest)))).
step(rule((policy(A), pass(A, allOfTest), pass(A, anyOfTest), pass(A, noneOfTest)), answer((policy(A), pass(A, allOfTest), pass(A, anyOfTest), pass(A, noneOfTest)))), (policy('PolicyX'), pass('PolicyX', allOfTest), pass('PolicyX', anyOfTest), pass('PolicyX', noneOfTest)), answer((policy('PolicyX'), pass('PolicyX', allOfTest), pass('PolicyX', anyOfTest), pass('PolicyX', noneOfTest)))).
step(rule((policy(A), pass(A, allOfTest), pass(A, anyOfTest), pass(A, noneOfTest)), answer((policy(A), pass(A, allOfTest), pass(A, anyOfTest), pass(A, noneOfTest)))), (policy('PolicyX'), pass('PolicyX', allOfTest), pass('PolicyX', anyOfTest), pass('PolicyX', noneOfTest)), answer((policy('PolicyX'), pass('PolicyX', allOfTest), pass('PolicyX', anyOfTest), pass('PolicyX', noneOfTest)))).
step(rule((policy(A), pass(A, allOfTest), pass(A, anyOfTest), pass(A, noneOfTest)), answer((policy(A), pass(A, allOfTest), pass(A, anyOfTest), pass(A, noneOfTest)))), (policy('PolicyX'), pass('PolicyX', allOfTest), pass('PolicyX', anyOfTest), pass('PolicyX', noneOfTest)), answer((policy('PolicyX'), pass('PolicyX', allOfTest), pass('PolicyX', anyOfTest), pass('PolicyX', noneOfTest)))).
