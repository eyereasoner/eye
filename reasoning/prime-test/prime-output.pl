:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#primerange>'(0, 100, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97])).
answer('<https://eyereasoner.github.io/ns#primerange>'(1000000, 1000100, [1000003, 1000033, 1000037, 1000039, 1000081, 1000099])).
answer('<https://eyereasoner.github.io/ns#totient>'(271, 270)).
answer('<https://eyereasoner.github.io/ns#totient>'(2718281, 2718280)).
answer('<https://eyereasoner.github.io/ns#totient>'(27182818284, 8994800640)).
answer('<https://eyereasoner.github.io/ns#totient>'(271828182845904, 87459116512768)).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#primerange>'(0, 100, _)), '<https://eyereasoner.github.io/ns#primerange>'(0, 100, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]), true).
step((true:+'<https://eyereasoner.github.io/ns#primerange>'(1000000, 1000100, _)), '<https://eyereasoner.github.io/ns#primerange>'(1000000, 1000100, [1000003, 1000033, 1000037, 1000039, 1000081, 1000099]), true).
step((true:+'<https://eyereasoner.github.io/ns#totient>'(271, _)), '<https://eyereasoner.github.io/ns#totient>'(271, 270), true).
step((true:+'<https://eyereasoner.github.io/ns#totient>'(2718281, _)), '<https://eyereasoner.github.io/ns#totient>'(2718281, 2718280), true).
step((true:+'<https://eyereasoner.github.io/ns#totient>'(27182818284, _)), '<https://eyereasoner.github.io/ns#totient>'(27182818284, 8994800640), true).
step((true:+'<https://eyereasoner.github.io/ns#totient>'(271828182845904, _)), '<https://eyereasoner.github.io/ns#totient>'(271828182845904, 87459116512768), true).
