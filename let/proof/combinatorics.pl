combination([0,[1,2,3,4,5]],[]).
combination([1,[1,2,3,4,5]],[1]).
combination([1,[1,2,3,4,5]],[2]).
combination([1,[1,2,3,4,5]],[3]).
combination([1,[1,2,3,4,5]],[4]).
combination([1,[1,2,3,4,5]],[5]).
combination([2,[1,2,3,4,5]],[1,2]).
combination([2,[1,2,3,4,5]],[1,3]).
combination([2,[1,2,3,4,5]],[1,4]).
combination([2,[1,2,3,4,5]],[1,5]).
combination([2,[1,2,3,4,5]],[2,3]).
combination([2,[1,2,3,4,5]],[2,4]).
combination([2,[1,2,3,4,5]],[2,5]).
combination([2,[1,2,3,4,5]],[3,4]).
combination([2,[1,2,3,4,5]],[3,5]).
combination([2,[1,2,3,4,5]],[4,5]).
combination([3,[1,2,3,4,5]],[1,2,3]).
combination([3,[1,2,3,4,5]],[1,2,4]).
combination([3,[1,2,3,4,5]],[1,2,5]).
combination([3,[1,2,3,4,5]],[1,3,4]).
combination([3,[1,2,3,4,5]],[1,3,5]).
combination([3,[1,2,3,4,5]],[1,4,5]).
combination([3,[1,2,3,4,5]],[2,3,4]).
combination([3,[1,2,3,4,5]],[2,3,5]).
combination([3,[1,2,3,4,5]],[2,4,5]).
combination([3,[1,2,3,4,5]],[3,4,5]).
combination([4,[1,2,3,4,5]],[1,2,3,4]).
combination([4,[1,2,3,4,5]],[1,2,3,5]).
combination([4,[1,2,3,4,5]],[1,2,4,5]).
combination([4,[1,2,3,4,5]],[1,3,4,5]).
combination([4,[1,2,3,4,5]],[2,3,4,5]).
combination([5,[1,2,3,4,5]],[1,2,3,4,5]).

step(rule(combination([0, [1, 2, 3, 4, 5]], A), answer(combination([0, [1, 2, 3, 4, 5]], A))), combination([0, [1, 2, 3, 4, 5]], []), answer(combination([0, [1, 2, 3, 4, 5]], []))).
step(rule(combination([1, [1, 2, 3, 4, 5]], A), answer(combination([1, [1, 2, 3, 4, 5]], A))), combination([1, [1, 2, 3, 4, 5]], [1]), answer(combination([1, [1, 2, 3, 4, 5]], [1]))).
step(rule(combination([1, [1, 2, 3, 4, 5]], A), answer(combination([1, [1, 2, 3, 4, 5]], A))), combination([1, [1, 2, 3, 4, 5]], [2]), answer(combination([1, [1, 2, 3, 4, 5]], [2]))).
step(rule(combination([1, [1, 2, 3, 4, 5]], A), answer(combination([1, [1, 2, 3, 4, 5]], A))), combination([1, [1, 2, 3, 4, 5]], [3]), answer(combination([1, [1, 2, 3, 4, 5]], [3]))).
step(rule(combination([1, [1, 2, 3, 4, 5]], A), answer(combination([1, [1, 2, 3, 4, 5]], A))), combination([1, [1, 2, 3, 4, 5]], [4]), answer(combination([1, [1, 2, 3, 4, 5]], [4]))).
step(rule(combination([1, [1, 2, 3, 4, 5]], A), answer(combination([1, [1, 2, 3, 4, 5]], A))), combination([1, [1, 2, 3, 4, 5]], [5]), answer(combination([1, [1, 2, 3, 4, 5]], [5]))).
step(rule(combination([2, [1, 2, 3, 4, 5]], A), answer(combination([2, [1, 2, 3, 4, 5]], A))), combination([2, [1, 2, 3, 4, 5]], [1, 2]), answer(combination([2, [1, 2, 3, 4, 5]], [1, 2]))).
step(rule(combination([2, [1, 2, 3, 4, 5]], A), answer(combination([2, [1, 2, 3, 4, 5]], A))), combination([2, [1, 2, 3, 4, 5]], [1, 3]), answer(combination([2, [1, 2, 3, 4, 5]], [1, 3]))).
step(rule(combination([2, [1, 2, 3, 4, 5]], A), answer(combination([2, [1, 2, 3, 4, 5]], A))), combination([2, [1, 2, 3, 4, 5]], [1, 4]), answer(combination([2, [1, 2, 3, 4, 5]], [1, 4]))).
step(rule(combination([2, [1, 2, 3, 4, 5]], A), answer(combination([2, [1, 2, 3, 4, 5]], A))), combination([2, [1, 2, 3, 4, 5]], [1, 5]), answer(combination([2, [1, 2, 3, 4, 5]], [1, 5]))).
step(rule(combination([2, [1, 2, 3, 4, 5]], A), answer(combination([2, [1, 2, 3, 4, 5]], A))), combination([2, [1, 2, 3, 4, 5]], [2, 3]), answer(combination([2, [1, 2, 3, 4, 5]], [2, 3]))).
step(rule(combination([2, [1, 2, 3, 4, 5]], A), answer(combination([2, [1, 2, 3, 4, 5]], A))), combination([2, [1, 2, 3, 4, 5]], [2, 4]), answer(combination([2, [1, 2, 3, 4, 5]], [2, 4]))).
step(rule(combination([2, [1, 2, 3, 4, 5]], A), answer(combination([2, [1, 2, 3, 4, 5]], A))), combination([2, [1, 2, 3, 4, 5]], [2, 5]), answer(combination([2, [1, 2, 3, 4, 5]], [2, 5]))).
step(rule(combination([2, [1, 2, 3, 4, 5]], A), answer(combination([2, [1, 2, 3, 4, 5]], A))), combination([2, [1, 2, 3, 4, 5]], [3, 4]), answer(combination([2, [1, 2, 3, 4, 5]], [3, 4]))).
step(rule(combination([2, [1, 2, 3, 4, 5]], A), answer(combination([2, [1, 2, 3, 4, 5]], A))), combination([2, [1, 2, 3, 4, 5]], [3, 5]), answer(combination([2, [1, 2, 3, 4, 5]], [3, 5]))).
step(rule(combination([2, [1, 2, 3, 4, 5]], A), answer(combination([2, [1, 2, 3, 4, 5]], A))), combination([2, [1, 2, 3, 4, 5]], [4, 5]), answer(combination([2, [1, 2, 3, 4, 5]], [4, 5]))).
step(rule(combination([3, [1, 2, 3, 4, 5]], A), answer(combination([3, [1, 2, 3, 4, 5]], A))), combination([3, [1, 2, 3, 4, 5]], [1, 2, 3]), answer(combination([3, [1, 2, 3, 4, 5]], [1, 2, 3]))).
step(rule(combination([3, [1, 2, 3, 4, 5]], A), answer(combination([3, [1, 2, 3, 4, 5]], A))), combination([3, [1, 2, 3, 4, 5]], [1, 2, 4]), answer(combination([3, [1, 2, 3, 4, 5]], [1, 2, 4]))).
step(rule(combination([3, [1, 2, 3, 4, 5]], A), answer(combination([3, [1, 2, 3, 4, 5]], A))), combination([3, [1, 2, 3, 4, 5]], [1, 2, 5]), answer(combination([3, [1, 2, 3, 4, 5]], [1, 2, 5]))).
step(rule(combination([3, [1, 2, 3, 4, 5]], A), answer(combination([3, [1, 2, 3, 4, 5]], A))), combination([3, [1, 2, 3, 4, 5]], [1, 3, 4]), answer(combination([3, [1, 2, 3, 4, 5]], [1, 3, 4]))).
step(rule(combination([3, [1, 2, 3, 4, 5]], A), answer(combination([3, [1, 2, 3, 4, 5]], A))), combination([3, [1, 2, 3, 4, 5]], [1, 3, 5]), answer(combination([3, [1, 2, 3, 4, 5]], [1, 3, 5]))).
step(rule(combination([3, [1, 2, 3, 4, 5]], A), answer(combination([3, [1, 2, 3, 4, 5]], A))), combination([3, [1, 2, 3, 4, 5]], [1, 4, 5]), answer(combination([3, [1, 2, 3, 4, 5]], [1, 4, 5]))).
step(rule(combination([3, [1, 2, 3, 4, 5]], A), answer(combination([3, [1, 2, 3, 4, 5]], A))), combination([3, [1, 2, 3, 4, 5]], [2, 3, 4]), answer(combination([3, [1, 2, 3, 4, 5]], [2, 3, 4]))).
step(rule(combination([3, [1, 2, 3, 4, 5]], A), answer(combination([3, [1, 2, 3, 4, 5]], A))), combination([3, [1, 2, 3, 4, 5]], [2, 3, 5]), answer(combination([3, [1, 2, 3, 4, 5]], [2, 3, 5]))).
step(rule(combination([3, [1, 2, 3, 4, 5]], A), answer(combination([3, [1, 2, 3, 4, 5]], A))), combination([3, [1, 2, 3, 4, 5]], [2, 4, 5]), answer(combination([3, [1, 2, 3, 4, 5]], [2, 4, 5]))).
step(rule(combination([3, [1, 2, 3, 4, 5]], A), answer(combination([3, [1, 2, 3, 4, 5]], A))), combination([3, [1, 2, 3, 4, 5]], [3, 4, 5]), answer(combination([3, [1, 2, 3, 4, 5]], [3, 4, 5]))).
step(rule(combination([4, [1, 2, 3, 4, 5]], A), answer(combination([4, [1, 2, 3, 4, 5]], A))), combination([4, [1, 2, 3, 4, 5]], [1, 2, 3, 4]), answer(combination([4, [1, 2, 3, 4, 5]], [1, 2, 3, 4]))).
step(rule(combination([4, [1, 2, 3, 4, 5]], A), answer(combination([4, [1, 2, 3, 4, 5]], A))), combination([4, [1, 2, 3, 4, 5]], [1, 2, 3, 5]), answer(combination([4, [1, 2, 3, 4, 5]], [1, 2, 3, 5]))).
step(rule(combination([4, [1, 2, 3, 4, 5]], A), answer(combination([4, [1, 2, 3, 4, 5]], A))), combination([4, [1, 2, 3, 4, 5]], [1, 2, 4, 5]), answer(combination([4, [1, 2, 3, 4, 5]], [1, 2, 4, 5]))).
step(rule(combination([4, [1, 2, 3, 4, 5]], A), answer(combination([4, [1, 2, 3, 4, 5]], A))), combination([4, [1, 2, 3, 4, 5]], [1, 3, 4, 5]), answer(combination([4, [1, 2, 3, 4, 5]], [1, 3, 4, 5]))).
step(rule(combination([4, [1, 2, 3, 4, 5]], A), answer(combination([4, [1, 2, 3, 4, 5]], A))), combination([4, [1, 2, 3, 4, 5]], [2, 3, 4, 5]), answer(combination([4, [1, 2, 3, 4, 5]], [2, 3, 4, 5]))).
step(rule(combination([5, [1, 2, 3, 4, 5]], A), answer(combination([5, [1, 2, 3, 4, 5]], A))), combination([5, [1, 2, 3, 4, 5]], [1, 2, 3, 4, 5]), answer(combination([5, [1, 2, 3, 4, 5]], [1, 2, 3, 4, 5]))).
