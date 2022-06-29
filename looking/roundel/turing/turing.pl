% See https://en.wikipedia.org/wiki/Universal_Turing_machine

% interpreter for Univeral Turing Machine

'https://josd.github.io/eye/ns#compute'([],OutTape) :-
    start(_MACHINE,I),
    find(I,[],#,[ ],OutTape).
'https://josd.github.io/eye/ns#compute'([Head|Tail],OutTape) :-
    start(_MACHINE,I),
    find(I,[],Head,Tail,OutTape).

find(State,Left,Cell,Right,OutTape) :-
    t([State,Cell,Write,Move],Next),
    move(Move,Left,Write,Right,A,B,C),
    continue(Next,A,B,C,OutTape).

continue(halt,Left,Cell,Right,OutTape) :-
    rev(Left,R),
    append(R,[Cell|Right],OutTape).
continue(State,Left,Cell,Right,OutTape) :-
    find(State,Left,Cell,Right,OutTape).

move(l,[],Cell,Right,[],#,[Cell|Right]).
move(l,[Head|Tail],Cell,Right,Tail,Head,[Cell|Right]).
move(s,Left,Cell,Right,Left,Cell,Right).
move(r,Left,Cell,[],[Cell|Left],#,[] ).
move(r,Left,Cell,[Head|Tail],[Cell|Left],Head,Tail).

rev([],[]).
rev([A|B],C) :-
    rev(B,D),
    append(D,[A],C).

% a Turing machine to add 1 to a binary number

start(add1,0).

t([0,0,0,r],0).
t([0,1,1,r],0).
t([0,#,#,l],1).
t([1,0,1,s],halt).
t([1,1,0,l],1).
t([1,#,1,s],halt).

% query
query('https://josd.github.io/eye/ns#compute'([1,0,1,0,0,1],_ANSWER)).
query('https://josd.github.io/eye/ns#compute'([1,0,1,1,1,1],_ANSWER)).
query('https://josd.github.io/eye/ns#compute'([1,1,1,1,1,1],_ANSWER)).
query('https://josd.github.io/eye/ns#compute'([],_ANSWER)).

run :-
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
