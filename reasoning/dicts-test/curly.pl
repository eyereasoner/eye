% using curly braces

:- op(1200, xfx, :+).

'<https://eyereasoner.github.io/ns#test>'(V) :-
    Item = {
        author:'Philip K Dick',
        works:[
            title:'The Man in the High Castle',
            title:'Do Androids Dream of Electric Sheep'
        ]
    },
    Item = {
        Author,
        _
    },
    Author = author:V.

% query
true :+ '<https://eyereasoner.github.io/ns#test>'(_).
