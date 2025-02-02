% Path discovery

:- op(1200, xfx, :+).

% connectivity
'<https://eyereasoner.github.io/ns#route>'([From, To, Visited, Length, Max], [From, To]) :-
    Length =< Max,
    '<http://neptune.aws.com/ontology/airroutes/hasOutboundRouteTo>'(From, To),
    \+member(To, Visited).
'<https://eyereasoner.github.io/ns#route>'([From, To, Visited, Length, Max], [From|NewRoute]) :-
    Length =< Max,
    '<http://neptune.aws.com/ontology/airroutes/hasOutboundRouteTo>'(From, Via),
    \+member(Via, Visited),
    NewLength is Length+1,
    '<https://eyereasoner.github.io/ns#route>'([Via, To, [From|Visited], NewLength, Max], NewRoute).

% discover routes from source to destination with at most 2 stopovers
'<https://eyereasoner.github.io/ns#discover>'([SourceLabel, DestinationLabel], Route) :-
    '<http://www.w3.org/2000/01/rdf-schema#label>'(Source, literal(SourceLabel, type('<http://www.w3.org/2001/XMLSchema#string>'))),
    '<http://www.w3.org/2000/01/rdf-schema#label>'(Destination, literal(DestinationLabel, type('<http://www.w3.org/2001/XMLSchema#string>'))),
    '<https://eyereasoner.github.io/ns#route>'([Source, Destination, [], 0, 2], Airports),
    findall(City,
        (   member(Airport, Airports),
            '<http://www.w3.org/2000/01/rdf-schema#label>'(Airport, literal(City, type('<http://www.w3.org/2001/XMLSchema#string>')))
        ),
        Route
    ).

% query
true :+ '<https://eyereasoner.github.io/ns#discover>'(['Manchester-Boston Regional Airport', 'Helsinki Vantaa Airport'], _).
