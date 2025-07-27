validGroup([identity,rotation_120,rotation_240,reflection_a,reflection_b,reflection_c]).
validGroup([identity,rotation_120,rotation_240,reflection_a,reflection_b]).
validGroup([identity,rotation_120,rotation_240,reflection_a,reflection_c]).
validGroup([identity,rotation_120,rotation_240,reflection_a]).
validGroup([identity,rotation_120,rotation_240,reflection_b,reflection_c]).
validGroup([identity,rotation_120,rotation_240,reflection_b]).
validGroup([identity,rotation_120,rotation_240,reflection_c]).
validGroup([identity,rotation_120,rotation_240]).
validGroup([identity,reflection_a]).
validGroup([identity,reflection_b]).
validGroup([identity,reflection_c]).
validGroup([identity]).

step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity, rotation_120, rotation_240, reflection_a, reflection_b, reflection_c]), answer(validGroup([identity, rotation_120, rotation_240, reflection_a, reflection_b, reflection_c]))).
step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity, rotation_120, rotation_240, reflection_a, reflection_b]), answer(validGroup([identity, rotation_120, rotation_240, reflection_a, reflection_b]))).
step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity, rotation_120, rotation_240, reflection_a, reflection_c]), answer(validGroup([identity, rotation_120, rotation_240, reflection_a, reflection_c]))).
step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity, rotation_120, rotation_240, reflection_a]), answer(validGroup([identity, rotation_120, rotation_240, reflection_a]))).
step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity, rotation_120, rotation_240, reflection_b, reflection_c]), answer(validGroup([identity, rotation_120, rotation_240, reflection_b, reflection_c]))).
step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity, rotation_120, rotation_240, reflection_b]), answer(validGroup([identity, rotation_120, rotation_240, reflection_b]))).
step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity, rotation_120, rotation_240, reflection_c]), answer(validGroup([identity, rotation_120, rotation_240, reflection_c]))).
step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity, rotation_120, rotation_240]), answer(validGroup([identity, rotation_120, rotation_240]))).
step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity, reflection_a]), answer(validGroup([identity, reflection_a]))).
step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity, reflection_b]), answer(validGroup([identity, reflection_b]))).
step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity, reflection_c]), answer(validGroup([identity, reflection_c]))).
step(rule(validGroup(A), answer(validGroup(A))), validGroup([identity]), answer(validGroup([identity]))).
