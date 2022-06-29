% See https://en.wikipedia.org/wiki/Four_color_theorem

:- use_module(library(lists)).

'https://josd.github.io/eye/ns#colors'(_Map,Places) :-
    findall([Place,_],'https://josd.github.io/eye/ns#neighbours'(Place,_),Places),
    places(Places).

places([]).
places([[Place,Color]|Tail]) :-
    places(Tail),
    'https://josd.github.io/eye/ns#neighbours'(Place,Neighbours),
    member(Color,['http://example.org/ns#c1','http://example.org/ns#c2','http://example.org/ns#c3','http://example.org/ns#c4']),
    \+ (member([Neighbour,Color],Tail),member(Neighbour,Neighbours)).

% test data
'https://josd.github.io/eye/ns#neighbours'(
    'http://example.org/ns#p1',
    [   'http://example.org/ns#p2',
        'http://example.org/ns#p5',
        'http://example.org/ns#p4',
        'http://example.org/ns#p3'
    ]
).
'https://josd.github.io/eye/ns#neighbours'(
    'http://example.org/ns#p2',
    [   'http://example.org/ns#p1',
        'http://example.org/ns#p4',
        'http://example.org/ns#p3'
    ]
).
'https://josd.github.io/eye/ns#neighbours'(
    'http://example.org/ns#p3',
    [   'http://example.org/ns#p5',
        'http://example.org/ns#p1',
        'http://example.org/ns#p4',
        'http://example.org/ns#p2'
    ]
).
'https://josd.github.io/eye/ns#neighbours'(
    'http://example.org/ns#p4',
    [   'http://example.org/ns#p1',
        'http://example.org/ns#p2',
        'http://example.org/ns#p3'
    ]
).
'https://josd.github.io/eye/ns#neighbours'(
    'http://example.org/ns#p5',
    [   'http://example.org/ns#p1',
        'http://example.org/ns#p3'
    ]
).

% query
'https://josd.github.io/eye/ns#colors'('http://example.org/ns#map1',_X) => true.
