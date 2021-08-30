% See https://en.wikipedia.org/wiki/Universal_Turing_machine

% interpreter for Univeral Turing Machine

:- use_module(library(lists)).

told(el/1,'https://josd.github.io/eye/lateral/ns#').

el(compute([],OutTape)) :-
    start(I),
    find(I,[],#,[ ],OutTape).
el(compute([Head|Tail],OutTape)) :-
    start(I),
    find(I,[],Head,Tail,OutTape).

find(State,Left,Cell,Right,OutTape) :-
    t(State,Cell,Write,Move,Next),
    move(Move,Left,Write,Right,A,B,C),
    continue(Next,A,B,C,OutTape).

continue(halt,Left,Cell,Right,OutTape) :-
    reverse(Left,R),
    append(R,[Cell|Right],OutTape).
continue(State,Left,Cell,Right,OutTape) :-
    find(State,Left,Cell,Right,OutTape).

move(l,[],Cell,Right,[],#,[Cell|Right]).
move(l,[Head|Tail],Cell,Right,Tail,Head,[Cell|Right]).
move(s,Left,Cell,Right,Left,Cell,Right).
move(r,Left,Cell,[],[Cell|Left],#,[] ).
move(r,Left,Cell,[Head|Tail],[Cell|Left],Head,Tail).

% a Turing machine to add 1 to a binary number

start(0).

t(0,0,0,r,0).
t(0,1,1,r,0).
t(0,#,#,l,1).
t(1,0,1,s,halt).
t(1,1,0,l,1).
t(1,#,1,s,halt).

% test cases
case(told(_NS,_P)).
case(el(compute([1,0,1,0,0,1],_ANSWER))).
case(el(compute([1,0,1,1,1,1],_ANSWER))).
case(el(compute([1,1,1,1,1,1],_ANSWER))).
case(el(compute([],_))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
