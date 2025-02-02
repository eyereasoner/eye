:- op(1200, xfx, :+).

answer(('<https://eyereasoner.github.io/ns#exponentiation>'([[2.718281828459045, 0], [0, pi]], [-1.0, 1.2246467991473532e-16]), '<https://eyereasoner.github.io/ns#sum>'([[-1.0, 1.2246467991473532e-16], [1, 0]], [0.0, 1.2246467991473532e-16]))).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#exponentiation>'([[2.718281828459045, 0], [0, pi]], A), '<https://eyereasoner.github.io/ns#sum>'([A, [1, 0]], _)), ('<https://eyereasoner.github.io/ns#exponentiation>'([[2.718281828459045, 0], [0, pi]], [-1.0, 1.2246467991473532e-16]), '<https://eyereasoner.github.io/ns#sum>'([[-1.0, 1.2246467991473532e-16], [1, 0]], [0.0, 1.2246467991473532e-16])), true).
