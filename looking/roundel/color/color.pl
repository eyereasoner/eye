% See https://en.wikipedia.org/wiki/Four_color_theorem

'https://josd.github.io/eye/ns#color'(_Map,Places) :-
    findall([Place,_],neighbours(Place,_),Places),
    places(Places).

places([]).
places([[Place,Color]|Tail]) :-
    places(Tail),
    neighbours(Place,Neighbours),
    member(Color,[red,green,blue,yellow]),
    \+ (member([Neighbour,Color],Tail),member(Neighbour,Neighbours)).

% test data
% map of European Union
neighbours(austria,[czech_republic,germany,hungary,italy,slovenia,slovakia]).
neighbours(belgium,[france,netherlands,luxemburg,germany,united_kingdom]).
neighbours(bulgaria,[romania,greece]).
neighbours(croatia,[slovenia,hungary]).
neighbours(cyprus,[greece]).
neighbours(czech_republic,[germany,poland,slovakia,austria]).
neighbours(denmark,[germany,sweden]).
neighbours(estonia,[finland,latvia,lithuania]).
neighbours(finland,[estonia,sweden]).
neighbours(france,[spain,belgium,luxemburg,germany,italy,united_kingdom]).
neighbours(germany,[netherlands,belgium,luxemburg,denmark,france,austria,poland,czech_republic]).
neighbours(greece,[bulgaria,cyprus]).
neighbours(hungary,[austria,slovakia,romania,croatia,slovenia]).
neighbours(ireland,[united_kingdom]).
neighbours(italy,[france,austria,slovenia]).
neighbours(latvia,[estonia,lithuania]).
neighbours(lithuania,[estonia,latvia,poland]).
neighbours(luxemburg,[belgium,france,germany]).
neighbours(malta,[]).
neighbours(netherlands,[belgium,germany,united_kingdom]).
neighbours(poland,[germany,czech_republic,slovakia,lithuania]).
neighbours(portugal,[spain]).
neighbours(romania,[hungary,bulgaria]).
neighbours(slovakia,[czech_republic,poland,hungary,austria]).
neighbours(slovenia,[austria,italy,hungary,croatia]).
neighbours(spain,[france,portugal]).
neighbours(sweden,[finland,denmark]).
neighbours(united_kingdom,[ireland,netherlands,belgium,france]).

% query
query('https://josd.github.io/eye/ns#color'(map1,_ANSWER)).

run :-
    query(Q),
    Q,
    writeq(Q),
    write('.\n').
