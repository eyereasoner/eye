% See https://en.wikipedia.org/wiki/Four_color_theorem

:- op(1200, xfx, :+).

'urn:example:colors'(_Map, Places) :-
    findall([Place, _], 'urn:example:neighbours'(Place, _), Places),
    places(Places),
    !.

places([]).
places([[Place, Color]|Tail]) :-
    places(Tail),
    'urn:example:neighbours'(Place, Neighbours),
    member(Color, ['urn:example:red', 'urn:example:green', 'urn:example:blue', 'urn:example:yellow']),
    \+ (member([Neighbour, Color], Tail), member(Neighbour, Neighbours)).

% map of European Union
'urn:example:neighbours'('urn:example:Belgium', ['urn:example:France', 'urn:example:Netherlands', 'urn:example:Luxemburg', 'urn:example:Germany']).
'urn:example:neighbours'('urn:example:Netherlands', ['urn:example:Belgium', 'urn:example:Germany']).
'urn:example:neighbours'('urn:example:Luxemburg', ['urn:example:Belgium', 'urn:example:France', 'urn:example:Germany']).
'urn:example:neighbours'('urn:example:France', ['urn:example:Spain', 'urn:example:Belgium', 'urn:example:Luxemburg', 'urn:example:Germany', 'urn:example:Italy']).
'urn:example:neighbours'('urn:example:Germany', ['urn:example:Netherlands', 'urn:example:Belgium''urn:example:Luxemburg', 'urn:example:Denmark', 'urn:example:France', 'urn:example:Austria', 'urn:example:Poland', 'urn:example:Czech_Republic']).
'urn:example:neighbours'('urn:example:Italy', ['urn:example:France', 'urn:example:Austria', 'urn:example:Slovenia']).
'urn:example:neighbours'('urn:example:Denmark', ['urn:example:Germany']).
'urn:example:neighbours'('urn:example:Ireland', []).
'urn:example:neighbours'('urn:example:Greece', ['urn:example:Bulgaria']).
'urn:example:neighbours'('urn:example:Spain', ['urn:example:France', 'urn:example:Portugal']).
'urn:example:neighbours'('urn:example:Portugal', ['urn:example:Spain']).
'urn:example:neighbours'('urn:example:Austria', ['urn:example:Czech_Republic', 'urn:example:Germany', 'urn:example:Hungary', 'urn:example:Italy', 'urn:example:Slovenia', 'urn:example:Slovakia']).
'urn:example:neighbours'('urn:example:Sweden', ['urn:example:Finland']).
'urn:example:neighbours'('urn:example:Finland', ['urn:example:Sweden']).
'urn:example:neighbours'('urn:example:Cyprus', []).
'urn:example:neighbours'('urn:example:Malta', []).
'urn:example:neighbours'('urn:example:Poland', ['urn:example:Germany', 'urn:example:Czech_Republic', 'urn:example:Slovakia', 'urn:example:Lithuania']).
'urn:example:neighbours'('urn:example:Hungary', ['urn:example:Austria', 'urn:example:Slovakia', 'urn:example:Romania', 'urn:example:Croatia', 'urn:example:Slovenia']).
'urn:example:neighbours'('urn:example:Czech_Republic', ['urn:example:Germany', 'urn:example:Poland', 'urn:example:Slovakia', 'urn:example:Austria']).
'urn:example:neighbours'('urn:example:Slovakia', ['urn:example:Czech_Republic', 'urn:example:Poland', 'urn:example:Hungary', 'urn:example:Austria']).
'urn:example:neighbours'('urn:example:Slovenia', ['urn:example:Austria', 'urn:example:Italy', 'urn:example:Hungary', 'urn:example:Croatia']).
'urn:example:neighbours'('urn:example:Estonia', ['urn:example:Latvia']).
'urn:example:neighbours'('urn:example:Latvia', ['urn:example:Estonia', 'urn:example:Lithuania']).
'urn:example:neighbours'('urn:example:Lithuania', ['urn:example:Latvia', 'urn:example:Poland']).
'urn:example:neighbours'('urn:example:Bulgaria', ['urn:example:Romania', 'urn:example:Greece']).
'urn:example:neighbours'('urn:example:Romania', ['urn:example:Hungary', 'urn:example:Bulgaria']).
'urn:example:neighbours'('urn:example:Croatia', ['urn:example:Slovenia', 'urn:example:Hungary']).

% query
true :+ 'urn:example:colors'('urn:example:mapEU', _).
