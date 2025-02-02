% Dining Philosophers

:- op(1200, xfx, :+).

'<https://eyereasoner.github.io/ns#chopstick>'('<https://eyereasoner.github.io/ns#person1>', '<https://eyereasoner.github.io/ns#chopstick1>', '<https://eyereasoner.github.io/ns#chopstick5>').
'<https://eyereasoner.github.io/ns#chopstick>'('<https://eyereasoner.github.io/ns#person2>', '<https://eyereasoner.github.io/ns#chopstick2>', '<https://eyereasoner.github.io/ns#chopstick1>').
'<https://eyereasoner.github.io/ns#chopstick>'('<https://eyereasoner.github.io/ns#person3>', '<https://eyereasoner.github.io/ns#chopstick3>', '<https://eyereasoner.github.io/ns#chopstick2>').
'<https://eyereasoner.github.io/ns#chopstick>'('<https://eyereasoner.github.io/ns#person4>', '<https://eyereasoner.github.io/ns#chopstick4>', '<https://eyereasoner.github.io/ns#chopstick3>').
'<https://eyereasoner.github.io/ns#chopstick>'('<https://eyereasoner.github.io/ns#person5>', '<https://eyereasoner.github.io/ns#chopstick5>', '<https://eyereasoner.github.io/ns#chopstick4>').

'<https://eyereasoner.github.io/ns#pickup>'(A) :-
    '<https://eyereasoner.github.io/ns#chopstick>'(A, B, C),
    mutex_lock(B),
    mutex_lock(C).

'<https://eyereasoner.github.io/ns#putdown>'(A) :-
    '<https://eyereasoner.github.io/ns#chopstick>'(A, B, C),
    mutex_unlock(C),
    mutex_unlock(B).

'<https://eyereasoner.github.io/ns#run>'(A, B, C) :-
    sleep(B),
    %format('% ~q thinking for ~w seconds~n', [A, B]),
    '<https://eyereasoner.github.io/ns#pickup>'(A),
    sleep(C),
    %format('% ~q eating for ~w seconds~n', [A, C]),
    '<https://eyereasoner.github.io/ns#putdown>'(A).

'<https://eyereasoner.github.io/ns#got>'('<https://eyereasoner.github.io/ns#all>', '<https://eyereasoner.github.io/ns#dinner>') :-
    thread_create('<https://eyereasoner.github.io/ns#run>'('<https://eyereasoner.github.io/ns#person1>', 0.1, 0.1), A, []),
    thread_create('<https://eyereasoner.github.io/ns#run>'('<https://eyereasoner.github.io/ns#person2>', 0.2, 0.2), B, []),
    thread_create('<https://eyereasoner.github.io/ns#run>'('<https://eyereasoner.github.io/ns#person3>', 0.15, 0.1), C, []),
    thread_create('<https://eyereasoner.github.io/ns#run>'('<https://eyereasoner.github.io/ns#person4>', 0.25, 0.2), D, []),
    thread_create('<https://eyereasoner.github.io/ns#run>'('<https://eyereasoner.github.io/ns#person5>', 0.25, 0.1), E, []),
    thread_join(A, _),
    thread_join(B, _),
    thread_join(C, _),
    thread_join(D, _),
    thread_join(E, _).

% query
true :+ '<https://eyereasoner.github.io/ns#got>'('<https://eyereasoner.github.io/ns#all>', '<https://eyereasoner.github.io/ns#dinner>').
