% Using curly braces

test(V) :-
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
true :+ test(_).
