% Using curly braces

:- op(1200, xfx, :+).

'urn:example:test'(V) :-
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
true :+ 'urn:example:test'(_).
