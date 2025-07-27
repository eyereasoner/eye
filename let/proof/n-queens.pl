queens(8,[4,2,7,3,6,8,5,1]).
queens(8,[5,2,4,7,3,8,6,1]).
queens(8,[3,5,2,8,6,4,7,1]).
queens(8,[3,6,4,2,8,5,7,1]).
queens(8,[5,7,1,3,8,6,4,2]).
queens(8,[4,6,8,3,1,7,5,2]).
queens(8,[3,6,8,1,4,7,5,2]).
queens(8,[5,3,8,4,7,1,6,2]).
queens(8,[5,7,4,1,3,8,6,2]).
queens(8,[4,1,5,8,6,3,7,2]).
queens(8,[3,6,4,1,8,5,7,2]).
queens(8,[4,7,5,3,1,6,8,2]).
queens(8,[6,4,2,8,5,7,1,3]).
queens(8,[6,4,7,1,8,2,5,3]).
queens(8,[1,7,4,6,8,2,5,3]).
queens(8,[6,8,2,4,1,7,5,3]).
queens(8,[6,2,7,1,4,8,5,3]).
queens(8,[4,7,1,8,5,2,6,3]).
queens(8,[5,8,4,1,7,2,6,3]).
queens(8,[4,8,1,5,7,2,6,3]).
queens(8,[2,7,5,8,1,4,6,3]).
queens(8,[1,7,5,8,2,4,6,3]).
queens(8,[2,5,7,4,1,8,6,3]).
queens(8,[4,2,7,5,1,8,6,3]).
queens(8,[5,7,1,4,2,8,6,3]).
queens(8,[6,4,1,5,8,2,7,3]).
queens(8,[5,1,4,6,8,2,7,3]).
queens(8,[5,2,6,1,7,4,8,3]).
queens(8,[6,3,7,2,8,5,1,4]).
queens(8,[2,7,3,6,8,5,1,4]).
queens(8,[7,3,1,6,8,5,2,4]).
queens(8,[5,1,8,6,3,7,2,4]).
queens(8,[1,5,8,6,3,7,2,4]).
queens(8,[3,6,8,1,5,7,2,4]).
queens(8,[6,3,1,7,5,8,2,4]).
queens(8,[7,5,3,1,6,8,2,4]).
queens(8,[7,3,8,2,5,1,6,4]).
queens(8,[5,3,1,7,2,8,6,4]).
queens(8,[2,5,7,1,3,8,6,4]).
queens(8,[3,6,2,5,8,1,7,4]).
queens(8,[6,1,5,2,8,3,7,4]).
queens(8,[8,3,1,6,2,5,7,4]).
queens(8,[2,8,6,1,3,5,7,4]).
queens(8,[5,7,2,6,3,1,8,4]).
queens(8,[3,6,2,7,5,1,8,4]).
queens(8,[6,2,7,1,3,5,8,4]).
queens(8,[3,7,2,8,6,4,1,5]).
queens(8,[6,3,7,2,4,8,1,5]).
queens(8,[4,2,7,3,6,8,1,5]).
queens(8,[7,1,3,8,6,4,2,5]).
queens(8,[1,6,8,3,7,4,2,5]).
queens(8,[3,8,4,7,1,6,2,5]).
queens(8,[6,3,7,4,1,8,2,5]).
queens(8,[7,4,2,8,6,1,3,5]).
queens(8,[4,6,8,2,7,1,3,5]).
queens(8,[2,6,1,7,4,8,3,5]).
queens(8,[2,4,6,8,3,1,7,5]).
queens(8,[3,6,8,2,4,1,7,5]).
queens(8,[6,3,1,8,4,2,7,5]).
queens(8,[8,4,1,3,6,2,7,5]).
queens(8,[4,8,1,3,6,2,7,5]).
queens(8,[2,6,8,3,1,4,7,5]).
queens(8,[7,2,6,3,1,4,8,5]).
queens(8,[3,6,2,7,1,4,8,5]).
queens(8,[4,7,3,8,2,5,1,6]).
queens(8,[4,8,5,3,1,7,2,6]).
queens(8,[3,5,8,4,1,7,2,6]).
queens(8,[4,2,8,5,7,1,3,6]).
queens(8,[5,7,2,4,8,1,3,6]).
queens(8,[7,4,2,5,8,1,3,6]).
queens(8,[8,2,4,1,7,5,3,6]).
queens(8,[7,2,4,1,8,5,3,6]).
queens(8,[5,1,8,4,2,7,3,6]).
queens(8,[4,1,5,8,2,7,3,6]).
queens(8,[5,2,8,1,4,7,3,6]).
queens(8,[3,7,2,8,5,1,4,6]).
queens(8,[3,1,7,5,8,2,4,6]).
queens(8,[8,2,5,3,1,7,4,6]).
queens(8,[3,5,2,8,1,7,4,6]).
queens(8,[3,5,7,1,4,2,8,6]).
queens(8,[5,2,4,6,8,3,1,7]).
queens(8,[6,3,5,8,1,4,2,7]).
queens(8,[5,8,4,1,3,6,2,7]).
queens(8,[4,2,5,8,6,1,3,7]).
queens(8,[4,6,1,5,2,8,3,7]).
queens(8,[6,3,1,8,5,2,4,7]).
queens(8,[5,3,1,6,8,2,4,7]).
queens(8,[4,2,8,6,1,3,5,7]).
queens(8,[6,3,5,7,1,4,2,8]).
queens(8,[6,4,7,1,3,5,2,8]).
queens(8,[4,7,5,2,6,1,3,8]).
queens(8,[5,7,2,6,3,1,4,8]).

step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 2, 7, 3, 6, 8, 5, 1]), answer(queens(8, [4, 2, 7, 3, 6, 8, 5, 1]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 2, 4, 7, 3, 8, 6, 1]), answer(queens(8, [5, 2, 4, 7, 3, 8, 6, 1]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 5, 2, 8, 6, 4, 7, 1]), answer(queens(8, [3, 5, 2, 8, 6, 4, 7, 1]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 6, 4, 2, 8, 5, 7, 1]), answer(queens(8, [3, 6, 4, 2, 8, 5, 7, 1]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 7, 1, 3, 8, 6, 4, 2]), answer(queens(8, [5, 7, 1, 3, 8, 6, 4, 2]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 6, 8, 3, 1, 7, 5, 2]), answer(queens(8, [4, 6, 8, 3, 1, 7, 5, 2]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 6, 8, 1, 4, 7, 5, 2]), answer(queens(8, [3, 6, 8, 1, 4, 7, 5, 2]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 3, 8, 4, 7, 1, 6, 2]), answer(queens(8, [5, 3, 8, 4, 7, 1, 6, 2]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 7, 4, 1, 3, 8, 6, 2]), answer(queens(8, [5, 7, 4, 1, 3, 8, 6, 2]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 1, 5, 8, 6, 3, 7, 2]), answer(queens(8, [4, 1, 5, 8, 6, 3, 7, 2]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 6, 4, 1, 8, 5, 7, 2]), answer(queens(8, [3, 6, 4, 1, 8, 5, 7, 2]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 7, 5, 3, 1, 6, 8, 2]), answer(queens(8, [4, 7, 5, 3, 1, 6, 8, 2]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 4, 2, 8, 5, 7, 1, 3]), answer(queens(8, [6, 4, 2, 8, 5, 7, 1, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 4, 7, 1, 8, 2, 5, 3]), answer(queens(8, [6, 4, 7, 1, 8, 2, 5, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [1, 7, 4, 6, 8, 2, 5, 3]), answer(queens(8, [1, 7, 4, 6, 8, 2, 5, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 8, 2, 4, 1, 7, 5, 3]), answer(queens(8, [6, 8, 2, 4, 1, 7, 5, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 2, 7, 1, 4, 8, 5, 3]), answer(queens(8, [6, 2, 7, 1, 4, 8, 5, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 7, 1, 8, 5, 2, 6, 3]), answer(queens(8, [4, 7, 1, 8, 5, 2, 6, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 8, 4, 1, 7, 2, 6, 3]), answer(queens(8, [5, 8, 4, 1, 7, 2, 6, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 8, 1, 5, 7, 2, 6, 3]), answer(queens(8, [4, 8, 1, 5, 7, 2, 6, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [2, 7, 5, 8, 1, 4, 6, 3]), answer(queens(8, [2, 7, 5, 8, 1, 4, 6, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [1, 7, 5, 8, 2, 4, 6, 3]), answer(queens(8, [1, 7, 5, 8, 2, 4, 6, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [2, 5, 7, 4, 1, 8, 6, 3]), answer(queens(8, [2, 5, 7, 4, 1, 8, 6, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 2, 7, 5, 1, 8, 6, 3]), answer(queens(8, [4, 2, 7, 5, 1, 8, 6, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 7, 1, 4, 2, 8, 6, 3]), answer(queens(8, [5, 7, 1, 4, 2, 8, 6, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 4, 1, 5, 8, 2, 7, 3]), answer(queens(8, [6, 4, 1, 5, 8, 2, 7, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 1, 4, 6, 8, 2, 7, 3]), answer(queens(8, [5, 1, 4, 6, 8, 2, 7, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 2, 6, 1, 7, 4, 8, 3]), answer(queens(8, [5, 2, 6, 1, 7, 4, 8, 3]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 3, 7, 2, 8, 5, 1, 4]), answer(queens(8, [6, 3, 7, 2, 8, 5, 1, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [2, 7, 3, 6, 8, 5, 1, 4]), answer(queens(8, [2, 7, 3, 6, 8, 5, 1, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [7, 3, 1, 6, 8, 5, 2, 4]), answer(queens(8, [7, 3, 1, 6, 8, 5, 2, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 1, 8, 6, 3, 7, 2, 4]), answer(queens(8, [5, 1, 8, 6, 3, 7, 2, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [1, 5, 8, 6, 3, 7, 2, 4]), answer(queens(8, [1, 5, 8, 6, 3, 7, 2, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 6, 8, 1, 5, 7, 2, 4]), answer(queens(8, [3, 6, 8, 1, 5, 7, 2, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 3, 1, 7, 5, 8, 2, 4]), answer(queens(8, [6, 3, 1, 7, 5, 8, 2, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [7, 5, 3, 1, 6, 8, 2, 4]), answer(queens(8, [7, 5, 3, 1, 6, 8, 2, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [7, 3, 8, 2, 5, 1, 6, 4]), answer(queens(8, [7, 3, 8, 2, 5, 1, 6, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 3, 1, 7, 2, 8, 6, 4]), answer(queens(8, [5, 3, 1, 7, 2, 8, 6, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [2, 5, 7, 1, 3, 8, 6, 4]), answer(queens(8, [2, 5, 7, 1, 3, 8, 6, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 6, 2, 5, 8, 1, 7, 4]), answer(queens(8, [3, 6, 2, 5, 8, 1, 7, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 1, 5, 2, 8, 3, 7, 4]), answer(queens(8, [6, 1, 5, 2, 8, 3, 7, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [8, 3, 1, 6, 2, 5, 7, 4]), answer(queens(8, [8, 3, 1, 6, 2, 5, 7, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [2, 8, 6, 1, 3, 5, 7, 4]), answer(queens(8, [2, 8, 6, 1, 3, 5, 7, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 7, 2, 6, 3, 1, 8, 4]), answer(queens(8, [5, 7, 2, 6, 3, 1, 8, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 6, 2, 7, 5, 1, 8, 4]), answer(queens(8, [3, 6, 2, 7, 5, 1, 8, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 2, 7, 1, 3, 5, 8, 4]), answer(queens(8, [6, 2, 7, 1, 3, 5, 8, 4]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 7, 2, 8, 6, 4, 1, 5]), answer(queens(8, [3, 7, 2, 8, 6, 4, 1, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 3, 7, 2, 4, 8, 1, 5]), answer(queens(8, [6, 3, 7, 2, 4, 8, 1, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 2, 7, 3, 6, 8, 1, 5]), answer(queens(8, [4, 2, 7, 3, 6, 8, 1, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [7, 1, 3, 8, 6, 4, 2, 5]), answer(queens(8, [7, 1, 3, 8, 6, 4, 2, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [1, 6, 8, 3, 7, 4, 2, 5]), answer(queens(8, [1, 6, 8, 3, 7, 4, 2, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 8, 4, 7, 1, 6, 2, 5]), answer(queens(8, [3, 8, 4, 7, 1, 6, 2, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 3, 7, 4, 1, 8, 2, 5]), answer(queens(8, [6, 3, 7, 4, 1, 8, 2, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [7, 4, 2, 8, 6, 1, 3, 5]), answer(queens(8, [7, 4, 2, 8, 6, 1, 3, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 6, 8, 2, 7, 1, 3, 5]), answer(queens(8, [4, 6, 8, 2, 7, 1, 3, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [2, 6, 1, 7, 4, 8, 3, 5]), answer(queens(8, [2, 6, 1, 7, 4, 8, 3, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [2, 4, 6, 8, 3, 1, 7, 5]), answer(queens(8, [2, 4, 6, 8, 3, 1, 7, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 6, 8, 2, 4, 1, 7, 5]), answer(queens(8, [3, 6, 8, 2, 4, 1, 7, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 3, 1, 8, 4, 2, 7, 5]), answer(queens(8, [6, 3, 1, 8, 4, 2, 7, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [8, 4, 1, 3, 6, 2, 7, 5]), answer(queens(8, [8, 4, 1, 3, 6, 2, 7, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 8, 1, 3, 6, 2, 7, 5]), answer(queens(8, [4, 8, 1, 3, 6, 2, 7, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [2, 6, 8, 3, 1, 4, 7, 5]), answer(queens(8, [2, 6, 8, 3, 1, 4, 7, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [7, 2, 6, 3, 1, 4, 8, 5]), answer(queens(8, [7, 2, 6, 3, 1, 4, 8, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 6, 2, 7, 1, 4, 8, 5]), answer(queens(8, [3, 6, 2, 7, 1, 4, 8, 5]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 7, 3, 8, 2, 5, 1, 6]), answer(queens(8, [4, 7, 3, 8, 2, 5, 1, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 8, 5, 3, 1, 7, 2, 6]), answer(queens(8, [4, 8, 5, 3, 1, 7, 2, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 5, 8, 4, 1, 7, 2, 6]), answer(queens(8, [3, 5, 8, 4, 1, 7, 2, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 2, 8, 5, 7, 1, 3, 6]), answer(queens(8, [4, 2, 8, 5, 7, 1, 3, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 7, 2, 4, 8, 1, 3, 6]), answer(queens(8, [5, 7, 2, 4, 8, 1, 3, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [7, 4, 2, 5, 8, 1, 3, 6]), answer(queens(8, [7, 4, 2, 5, 8, 1, 3, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [8, 2, 4, 1, 7, 5, 3, 6]), answer(queens(8, [8, 2, 4, 1, 7, 5, 3, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [7, 2, 4, 1, 8, 5, 3, 6]), answer(queens(8, [7, 2, 4, 1, 8, 5, 3, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 1, 8, 4, 2, 7, 3, 6]), answer(queens(8, [5, 1, 8, 4, 2, 7, 3, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 1, 5, 8, 2, 7, 3, 6]), answer(queens(8, [4, 1, 5, 8, 2, 7, 3, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 2, 8, 1, 4, 7, 3, 6]), answer(queens(8, [5, 2, 8, 1, 4, 7, 3, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 7, 2, 8, 5, 1, 4, 6]), answer(queens(8, [3, 7, 2, 8, 5, 1, 4, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 1, 7, 5, 8, 2, 4, 6]), answer(queens(8, [3, 1, 7, 5, 8, 2, 4, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [8, 2, 5, 3, 1, 7, 4, 6]), answer(queens(8, [8, 2, 5, 3, 1, 7, 4, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 5, 2, 8, 1, 7, 4, 6]), answer(queens(8, [3, 5, 2, 8, 1, 7, 4, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [3, 5, 7, 1, 4, 2, 8, 6]), answer(queens(8, [3, 5, 7, 1, 4, 2, 8, 6]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 2, 4, 6, 8, 3, 1, 7]), answer(queens(8, [5, 2, 4, 6, 8, 3, 1, 7]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 3, 5, 8, 1, 4, 2, 7]), answer(queens(8, [6, 3, 5, 8, 1, 4, 2, 7]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 8, 4, 1, 3, 6, 2, 7]), answer(queens(8, [5, 8, 4, 1, 3, 6, 2, 7]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 2, 5, 8, 6, 1, 3, 7]), answer(queens(8, [4, 2, 5, 8, 6, 1, 3, 7]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 6, 1, 5, 2, 8, 3, 7]), answer(queens(8, [4, 6, 1, 5, 2, 8, 3, 7]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 3, 1, 8, 5, 2, 4, 7]), answer(queens(8, [6, 3, 1, 8, 5, 2, 4, 7]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 3, 1, 6, 8, 2, 4, 7]), answer(queens(8, [5, 3, 1, 6, 8, 2, 4, 7]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 2, 8, 6, 1, 3, 5, 7]), answer(queens(8, [4, 2, 8, 6, 1, 3, 5, 7]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 3, 5, 7, 1, 4, 2, 8]), answer(queens(8, [6, 3, 5, 7, 1, 4, 2, 8]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [6, 4, 7, 1, 3, 5, 2, 8]), answer(queens(8, [6, 4, 7, 1, 3, 5, 2, 8]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [4, 7, 5, 2, 6, 1, 3, 8]), answer(queens(8, [4, 7, 5, 2, 6, 1, 3, 8]))).
step(rule(queens(8, A), answer(queens(8, A))), queens(8, [5, 7, 2, 6, 3, 1, 4, 8]), answer(queens(8, [5, 7, 2, 6, 3, 1, 4, 8]))).
