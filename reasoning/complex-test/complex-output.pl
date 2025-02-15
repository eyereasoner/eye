:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#quotient>'([[1, 0], [0, 1]], [0, -1])).
answer('<https://eyereasoner.github.io/ns#exponentiation>'([[-1, 0], [0.5, 0]], [6.123233995736766e-17, 1.0])).
answer('<https://eyereasoner.github.io/ns#exponentiation>'([[2.718281828459045, 0], [0, pi]], [-1.0, 1.2246467991473532e-16])).
answer('<https://eyereasoner.github.io/ns#log>'([[2.718281828459045, 0], [-1, 0]], [0.0, 3.141592653589793])).
answer('<https://eyereasoner.github.io/ns#log>'([[0, 1], [0, 1]], [1.0, 0.0])).
answer('<https://eyereasoner.github.io/ns#sin>'([1.570796326794897, 1.316957896924817], [2.0000000000000004, -6.631275506809351e-16])).
answer('<https://eyereasoner.github.io/ns#cos>'([0, -1.316957896924817], [2.0000000000000004, 0.0])).
answer('<https://eyereasoner.github.io/ns#tan>'([1.338972522294493, 0.4023594781085251], [1.000000000000001, 1.9999999999999982])).
answer('<https://eyereasoner.github.io/ns#asin>'([2, 0], [1.5707963267948966, 1.3169578969248166])).
answer('<https://eyereasoner.github.io/ns#acos>'([2, 0], [0.0, -1.3169578969248166])).
answer('<https://eyereasoner.github.io/ns#atan>'([1, 2], [1.3389725222944935, 0.40235947810852507])).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#quotient>'([[1, 0], [0, 1]], _)), '<https://eyereasoner.github.io/ns#quotient>'([[1, 0], [0, 1]], [0, -1]), true).
step((true:+'<https://eyereasoner.github.io/ns#exponentiation>'([[-1, 0], [0.5, 0]], _)), '<https://eyereasoner.github.io/ns#exponentiation>'([[-1, 0], [0.5, 0]], [6.123233995736766e-17, 1.0]), true).
step((true:+'<https://eyereasoner.github.io/ns#exponentiation>'([[2.718281828459045, 0], [0, pi]], _)), '<https://eyereasoner.github.io/ns#exponentiation>'([[2.718281828459045, 0], [0, pi]], [-1.0, 1.2246467991473532e-16]), true).
step((true:+'<https://eyereasoner.github.io/ns#log>'([[2.718281828459045, 0], [-1, 0]], _)), '<https://eyereasoner.github.io/ns#log>'([[2.718281828459045, 0], [-1, 0]], [0.0, 3.141592653589793]), true).
step((true:+'<https://eyereasoner.github.io/ns#log>'([[0, 1], [0, 1]], _)), '<https://eyereasoner.github.io/ns#log>'([[0, 1], [0, 1]], [1.0, 0.0]), true).
step((true:+'<https://eyereasoner.github.io/ns#sin>'([1.570796326794897, 1.316957896924817], _)), '<https://eyereasoner.github.io/ns#sin>'([1.570796326794897, 1.316957896924817], [2.0000000000000004, -6.631275506809351e-16]), true).
step((true:+'<https://eyereasoner.github.io/ns#cos>'([0, -1.316957896924817], _)), '<https://eyereasoner.github.io/ns#cos>'([0, -1.316957896924817], [2.0000000000000004, 0.0]), true).
step((true:+'<https://eyereasoner.github.io/ns#tan>'([1.338972522294493, 0.4023594781085251], _)), '<https://eyereasoner.github.io/ns#tan>'([1.338972522294493, 0.4023594781085251], [1.000000000000001, 1.9999999999999982]), true).
step((true:+'<https://eyereasoner.github.io/ns#asin>'([2, 0], _)), '<https://eyereasoner.github.io/ns#asin>'([2, 0], [1.5707963267948966, 1.3169578969248166]), true).
step((true:+'<https://eyereasoner.github.io/ns#acos>'([2, 0], _)), '<https://eyereasoner.github.io/ns#acos>'([2, 0], [0.0, -1.3169578969248166]), true).
step((true:+'<https://eyereasoner.github.io/ns#atan>'([1, 2], _)), '<https://eyereasoner.github.io/ns#atan>'([1, 2], [1.3389725222944935, 0.40235947810852507]), true).
