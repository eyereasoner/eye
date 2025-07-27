complex_quotient([[1,0],[0,1]],[0,-1]).
complex_exponentiation([[-1,0],[0.5,0]],[6.123233995736766e-17,1.0]).
complex_exponentiation([[2.718281828459045,0],[0,pi]],[-1.0,1.2246467991473532e-16]).
complex_log([[2.718281828459045,0],[-1,0]],[0.0,3.141592653589793]).
complex_log([[0,1],[0,1]],[1.0,0.0]).
complex_sin([1.570796326794897,1.316957896924817],[2.0000000000000004,-6.631275506809351e-16]).
complex_cos([0,-1.316957896924817],[2.0000000000000004,0.0]).
complex_asin([2,0],[1.5707963267948966,1.3169578969248166]).
complex_acos([2,0],[0.0,-1.3169578969248166]).

step(rule(complex_quotient([[1, 0], [0, 1]], A), answer(complex_quotient([[1, 0], [0, 1]], A))), complex_quotient([[1, 0], [0, 1]], [0, -1]), answer(complex_quotient([[1, 0], [0, 1]], [0, -1]))).
step(rule(complex_exponentiation([[-1, 0], [0.5, 0]], A), answer(complex_exponentiation([[-1, 0], [0.5, 0]], A))), complex_exponentiation([[-1, 0], [0.5, 0]], [6.123233995736766e-17, 1.0]), answer(complex_exponentiation([[-1, 0], [0.5, 0]], [6.123233995736766e-17, 1.0]))).
step(rule(complex_exponentiation([[2.718281828459045, 0], [0, pi]], A), answer(complex_exponentiation([[2.718281828459045, 0], [0, pi]], A))), complex_exponentiation([[2.718281828459045, 0], [0, pi]], [-1.0, 1.2246467991473532e-16]), answer(complex_exponentiation([[2.718281828459045, 0], [0, pi]], [-1.0, 1.2246467991473532e-16]))).
step(rule(complex_log([[2.718281828459045, 0], [-1, 0]], A), answer(complex_log([[2.718281828459045, 0], [-1, 0]], A))), complex_log([[2.718281828459045, 0], [-1, 0]], [0.0, 3.141592653589793]), answer(complex_log([[2.718281828459045, 0], [-1, 0]], [0.0, 3.141592653589793]))).
step(rule(complex_log([[0, 1], [0, 1]], A), answer(complex_log([[0, 1], [0, 1]], A))), complex_log([[0, 1], [0, 1]], [1.0, 0.0]), answer(complex_log([[0, 1], [0, 1]], [1.0, 0.0]))).
step(rule(complex_sin([1.570796326794897, 1.316957896924817], A), answer(complex_sin([1.570796326794897, 1.316957896924817], A))), complex_sin([1.570796326794897, 1.316957896924817], [2.0000000000000004, -6.631275506809351e-16]), answer(complex_sin([1.570796326794897, 1.316957896924817], [2.0000000000000004, -6.631275506809351e-16]))).
step(rule(complex_cos([0, -1.316957896924817], A), answer(complex_cos([0, -1.316957896924817], A))), complex_cos([0, -1.316957896924817], [2.0000000000000004, 0.0]), answer(complex_cos([0, -1.316957896924817], [2.0000000000000004, 0.0]))).
step(rule(complex_asin([2, 0], A), answer(complex_asin([2, 0], A))), complex_asin([2, 0], [1.5707963267948966, 1.3169578969248166]), answer(complex_asin([2, 0], [1.5707963267948966, 1.3169578969248166]))).
step(rule(complex_acos([2, 0], A), answer(complex_acos([2, 0], A))), complex_acos([2, 0], [0.0, -1.3169578969248166]), answer(complex_acos([2, 0], [0.0, -1.3169578969248166]))).
