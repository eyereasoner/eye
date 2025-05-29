:- op(1200, xfx, :+).

answer((policy('PolicyX'), pass('PolicyX', allOfTest), pass('PolicyX', anyOfTest), pass('PolicyX', noneOfTest))).

step((true:+policy(A), pass(A, allOfTest), pass(A, anyOfTest), pass(A, noneOfTest)), (policy('PolicyX'), pass('PolicyX', allOfTest), pass('PolicyX', anyOfTest), pass('PolicyX', noneOfTest)), true).
