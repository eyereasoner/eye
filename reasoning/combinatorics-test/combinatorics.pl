% Combinatorics

:- op(1200, xfx, :+).

% combination
'<https://eyereasoner.github.io/ns#combination>'([0, _], []).
'<https://eyereasoner.github.io/ns#combination>'([I, As], Bs) :-
    I > 0,
    select(B, As, Cs),
    J is I-1,
    '<https://eyereasoner.github.io/ns#combination>'([J, Cs], Ds),
    sort([B|Ds], Bs).

% permutation
'<https://eyereasoner.github.io/ns#permutation>'(A, B) :-
    permutation(A, B).

% query
true :+ '<https://eyereasoner.github.io/ns#combination>'([0, [1, 2, 3, 4, 5]], _).
true :+ '<https://eyereasoner.github.io/ns#combination>'([1, [1, 2, 3, 4, 5]], _).
true :+ '<https://eyereasoner.github.io/ns#combination>'([2, [1, 2, 3, 4, 5]], _).
true :+ '<https://eyereasoner.github.io/ns#combination>'([3, [1, 2, 3, 4, 5]], _).
true :+ '<https://eyereasoner.github.io/ns#combination>'([4, [1, 2, 3, 4, 5]], _).
true :+ '<https://eyereasoner.github.io/ns#combination>'([5, [1, 2, 3, 4, 5]], _).
true :+ '<https://eyereasoner.github.io/ns#permutation>'([1, 2, 3, 4, 5], _).
