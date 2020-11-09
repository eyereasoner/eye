:- initialization(main).

main :-
    colors(Places),
    Places = [p1-c4,p2-c3,p3-c2,p4-c1,p5-c1],
    write(true),
    halt.

colors(Places) :-
    findall(Place-_,neighbours(Place,_),Places),
    places(Places).

places([]).
places([Place-Color|Tail]) :-
    places(Tail),
    neighbours(Place,Neighbours),
    member(Color,[c1,c2,c3,c4]),
    \+ (member(Neighbour-Color,Tail),member(Neighbour,Neighbours)).

neighbours(p1,[p2,p5,p4,p3]).
neighbours(p2,[p1,p4,p3]).
neighbours(p3,[p5,p1,p4,p2]).
neighbours(p4,[p1,p2,p3]).
neighbours(p5,[p1,p3]).
