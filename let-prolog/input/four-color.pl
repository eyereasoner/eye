% See https://en.wikipedia.org/wiki/Four_color_theorem

:- op(1200, xfx, :+).

colors(_Map, Places) :-
    findall([Place, _], neighbours(Place, _), Places),
    places(Places),
    !.

places([]).
places([[Place, Color]|Tail]) :-
    places(Tail),
    neighbours(Place, Neighbours),
    member(Color, [red, green, blue, yellow]),
    \+ (member([Neighbour, Color], Tail), member(Neighbour, Neighbours)).

% map of European Union
neighbours('Belgium', ['France', 'Netherlands', 'Luxemburg', 'Germany']).
neighbours('Netherlands', ['Belgium', 'Germany']).
neighbours('Luxemburg', ['Belgium', 'France', 'Germany']).
neighbours('France', ['Spain', 'Belgium', 'Luxemburg', 'Germany', 'Italy']).
neighbours('Germany', ['Netherlands', 'Belgium', 'Luxemburg', 'Denmark', 'France', 'Austria', 'Poland', 'Czech Republic']).
neighbours('Italy', ['France', 'Austria', 'Slovenia']).
neighbours('Denmark', ['Germany']).
neighbours('Ireland', []).
neighbours('Greece', ['Bulgaria']).
neighbours('Spain', ['France', 'Portugal']).
neighbours('Portugal', ['Spain']).
neighbours('Austria', ['Czech Republic', 'Germany', 'Hungary', 'Italy', 'Slovenia', 'Slovakia']).
neighbours('Sweden', ['Finland']).
neighbours('Finland', ['Sweden']).
neighbours('Cyprus', []).
neighbours('Malta', []).
neighbours('Poland', ['Germany', 'Czech Republic', 'Slovakia', 'Lithuania']).
neighbours('Hungary', ['Austria', 'Slovakia', 'Romania', 'Croatia', 'Slovenia']).
neighbours('Czech Republic', ['Germany', 'Poland', 'Slovakia', 'Austria']).
neighbours('Slovakia', ['Czech Republic', 'Poland', 'Hungary', 'Austria']).
neighbours('Slovenia', ['Austria', 'Italy', 'Hungary', 'Croatia']).
neighbours('Estonia', ['Latvia']).
neighbours('Latvia', ['Estonia', 'Lithuania']).
neighbours('Lithuania', ['Latvia', 'Poland']).
neighbours('Bulgaria', ['Romania', 'Greece']).
neighbours('Romania', ['Hungary', 'Bulgaria']).
neighbours('Croatia', ['Slovenia', 'Hungary']).

% query
true :+ colors(mapEU, _).
