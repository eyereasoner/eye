% See https://en.wikipedia.org/wiki/Universal_Turing_machine

% interpreter for Univeral Turing Machine

:- use_module(library(lists)).

web_nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_compute([],OutTape) :-
    etc_start(I),
    etc_find(I,[],#,[],OutTape).
etc_compute([Head|Tail],OutTape) :-
    etc_start(I),
    etc_find(I,[],Head,Tail,OutTape).

etc_find(State,Left,Cell,Right,OutTape) :-
    etc_t(State,Cell,Write,Move,Next),
    etc_move(Move,Left,Write,Right,A,B,C),
    etc_continue(Next,A,B,C,OutTape).

etc_continue(etc_halt,Left,Cell,Right,OutTape) :-
    reverse(Left,R),
    append(R,[Cell|Right],OutTape).
etc_continue(State,Left,Cell,Right,OutTape) :-
    etc_find(State,Left,Cell,Right,OutTape).

etc_move(etc_l,[],Cell,Right,[],#,[Cell|Right]).
etc_move(etc_l,[Head|Tail],Cell,Right,Tail,Head,[Cell|Right]).
etc_move(etc_s,Left,Cell,Right,Left,Cell,Right).
etc_move(etc_r,Left,Cell,[],[Cell|Left],#,[] ).
etc_move(etc_r,Left,Cell,[Head|Tail],[Cell|Left],Head,Tail).

% a Turing machine to add 1 to a binary number

etc_start(0).

etc_t(0,0,0,etc_r,0).
etc_t(0,1,1,etc_r,0).
etc_t(0,#,#,etc_l,1).
etc_t(1,0,1,etc_s,etc_halt).
etc_t(1,1,0,etc_l,1).
etc_t(1,#,1,etc_s,etc_halt).

% test cases
case(etc_compute([1,0,1,0,0,1],_ANSWER)).
case(etc_compute([1,0,1,1,1,1],_ANSWER)).
case(etc_compute([1,1,1,1,1,1],_ANSWER)).
case(etc_compute([],_)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
