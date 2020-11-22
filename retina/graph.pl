:- initialization(main).

main :-
    findall(Path,path(paris,nantes,Path),Paths),
    Paths = [[paris,orleans,blois,tours,lemans,angers,nantes],[paris,chartres,lemans,angers,nantes]],
    write('[] = "PASS".'),
    nl.

oneway(paris,orleans).
oneway(paris,chartres).
oneway(paris,amiens).
oneway(orleans,blois).
oneway(orleans,bourges).
oneway(blois,tours).
oneway(chartres,lemans).
oneway(lemans,angers).
oneway(lemans,tours).
oneway(angers,nantes).

twoway(From,To) :-
    oneway(From,To).
twoway(From,To) :-
    oneway(To,From).

path(From,To,Path) :-
    find(From,To,[From],Back),
    reverse(Back,Path).

find(From,To,Sofar,[To|Sofar]) :-
    twoway(From,To).
find(From,To,Sofar,Back) :-
    twoway(From,Via),
    Via \= To,
    \+ member(Via,Sofar),
    find(Via,To,[Via|Sofar],Back).
