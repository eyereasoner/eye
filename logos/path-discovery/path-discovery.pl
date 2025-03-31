% Path discovery

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
'<https://eyereasoner.github.io/ns#discover>'([SourceLit, DestinationLit], Route) :-
    '<http://www.w3.org/2000/01/rdf-schema#label>'(Source, SourceLit),
    '<http://www.w3.org/2000/01/rdf-schema#label>'(Destination, DestinationLit),
    '<https://eyereasoner.github.io/ns#route>'([Source, Destination, [], 0, 2], Airports),
    findall(AirportLit,
        (   member(Airport, Airports),
            '<http://www.w3.org/2000/01/rdf-schema#label>'(Airport, AirportLit)
        ),
        Route
    ).
