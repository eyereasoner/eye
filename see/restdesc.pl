% RESTdesc
% See http://restdesc.org/

:- use_module(library(iso_ext)).
:- use_module(library(terms)).

:- dynamic(brake/0).
:- dynamic('http://example.org/restaurant#hasReservation'/2).
:- dynamic('http://www.w3.org/2011/http#methodName'/2).
:- dynamic('http://www.w3.org/2011/http#requestURI'/2).
:- dynamic('http://www.w3.org/2011/http#resp'/2).
:- dynamic('http://www.w3.org/2011/http#body'/2).
:- dynamic('http://xmlns.com/foaf/0.1/based_near'/2).
:- dynamic('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'/2).
:- dynamic('http://www.w3.org/2003/01/geo/wgs84_pos#lat'/2).
:- dynamic('http://www.w3.org/2003/01/geo/wgs84_pos#long'/2).
:- dynamic('http://purl.org/ns/meteo#temperature'/2).
:- dynamic('http://purl.org/ns/meteo#celsius'/2).
:- dynamic('http://purl.org/ns/meteo#pressure'/2).
:- dynamic('http://purl.org/ns/meteo#millibar'/2).
:- dynamic('http://example.org/restaurant#isAt'/2).
:- dynamic('http://example.org/restaurant#reservationList'/2).
:- dynamic('http://example.org/restaurant#isOutside'/2).
:- dynamic('http://example.org/restaurant#onDate'/2).
:- dynamic('http://example.org/restaurant#place'/2).

% facts
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('http://example.org/vocab#Areeb','http://xmlns.com/foaf/0.1/Person').
'http://example.org/restaurant#hasDate'('http://example.org/vocab#RestAppointment',"12/12/12").
'http://example.org/restaurant#isOn'('http://example.org/vocab#RestAppointment',"#myAppointments").

% descriptions
'https://eyereasoner.github.io/see#restdesc'(
    (
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,'http://xmlns.com/foaf/0.1/Person')
    ),(
        'http://www.w3.org/2011/http#methodName'(B,"GET"),
        'http://www.w3.org/2011/http#requestURI'(B,[A]),
        'http://www.w3.org/2011/http#resp'(B,C),
        'http://www.w3.org/2011/http#body'(C,D),
        'http://xmlns.com/foaf/0.1/based_near'(A,D),
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(D,'http://www.w3.org/2000/10/swap/pim/contact#Address')
    )
).

'https://eyereasoner.github.io/see#restdesc'(
    (
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,'http://www.w3.org/2000/10/swap/pim/contact#Address')
    ),(
        'http://www.w3.org/2011/http#methodName'(B,"GET"),
        'http://www.w3.org/2011/http#requestURI'(B,["?location= ",A]),
        'http://www.w3.org/2011/http#resp'(B,C),
        'http://www.w3.org/2011/http#body'(C,D),
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(D,'http://www.w3.org/2003/01/geo/wgs84_pos#Point'),
        'http://www.w3.org/2003/01/geo/wgs84_pos#lat'(D,_),'http://www.w3.org/2003/01/geo/wgs84_pos#long'(D,_)
    )
).

'https://eyereasoner.github.io/see#restdesc'(
    (
        'http://www.w3.org/2003/01/geo/wgs84_pos#lat'(A,B),
        'http://www.w3.org/2003/01/geo/wgs84_pos#long'(A,C)
    ),(
        'http://www.w3.org/2011/http#methodName'(D,"GET"),
        'http://www.w3.org/2011/http#requestURI'(D,["CurrentTemperature?lat=",B,"&long=",C]),
        'http://www.w3.org/2011/http#resp'(D,E),
        'http://www.w3.org/2011/http#body'(E,F),
        'http://purl.org/ns/meteo#temperature'(A,G),
        'http://purl.org/ns/meteo#celsius'(G,F)
    )
).

'https://eyereasoner.github.io/see#restdesc'(
    (
        'http://www.w3.org/2003/01/geo/wgs84_pos#lat'(A,B),
        'http://www.w3.org/2003/01/geo/wgs84_pos#long'(A,C)
    ),(
        'http://www.w3.org/2011/http#methodName'(D,"GET"),
        'http://www.w3.org/2011/http#requestURI'(D,["CurrentPressure?lat=",B,"&long=",C]),
        'http://www.w3.org/2011/http#resp'(D,E),
        'http://www.w3.org/2011/http#body'(E,F),
        'http://purl.org/ns/meteo#pressure'(A,G),
        'http://purl.org/ns/meteo#millibar'(G,F)
    )
).

'https://eyereasoner.github.io/see#restdesc'(
    (
        'http://www.w3.org/2003/01/geo/wgs84_pos#lat'(A,_),
        'http://www.w3.org/2003/01/geo/wgs84_pos#long'(A,_),
        'http://purl.org/ns/meteo#pressure'(A,B),
        'http://purl.org/ns/meteo#temperature'(A,C),
        'http://purl.org/ns/meteo#millibar'(B,_),
        'http://purl.org/ns/meteo#celsius'(C,_),
        'http://example.org/restaurant#isOn'(D,E),
        'http://example.org/restaurant#hasDate'(D,_)
    ),(
        'http://www.w3.org/2011/http#methodName'(F,"POST"),
        'http://www.w3.org/2011/http#requestURI'(F,E),
        'http://www.w3.org/2011/http#body'(F,[D,G,H]),
        'http://www.w3.org/2011/http#resp'(F,I),
        'http://www.w3.org/2011/http#body'(I,D),
        'http://example.org/restaurant#isAt'(D,G),
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(G,'http://example.org/restaurant#Restaurant'),
        'http://example.org/restaurant#reservationList'(G,_),
        'http://example.org/restaurant#isOutside'(_,H)
    )
).

'https://eyereasoner.github.io/see#restdesc'(
    (
        'http://example.org/restaurant#reservationList'(A,B),
        'http://example.org/restaurant#isOutside'(_,C),
        'http://example.org/restaurant#hasDate'(_,D)
    ),(
        'http://www.w3.org/2011/http#methodName'(E,"POST"),
        'http://www.w3.org/2011/http#requestURI'(E,B),
        'http://www.w3.org/2011/http#body'(E,[D,C]),
        'http://www.w3.org/2011/http#resp'(E,F),
        'http://www.w3.org/2011/http#body'(F,G),
        'http://example.org/restaurant#hasReservation'(A,G),
        'http://example.org/restaurant#onDate'(G,D),
        'http://example.org/restaurant#place'(G,H),
        'http://example.org/restaurant#isOutside'(H,C)
    )
).

% composition
compose :-
    (   'https://eyereasoner.github.io/see#restdesc'(Pre,Post),
        Pre,
        \+Post,
        label(Post),
        step(Post),
        retract(brake),
        false
    ;   brake,
        !
    ;   assertz(brake),
        compose
    ).

label(A) :-
    (   bb_get(label,B)
    ->  true
    ;   B=0
    ),
    numbervars(A,B,C),
    bb_put(label,C).

step((A,B)) :-
    !,
    step(A),
    step(B).
step(A) :-
    (   \+A
    ->  assertz(A)
    ;   true
    ).

% query
query(
    (
        'http://www.w3.org/2011/http#methodName'(A,"GET"),
        'http://www.w3.org/2011/http#requestURI'(A,_B),
        'http://www.w3.org/2011/http#resp'(A,C),
        'http://www.w3.org/2011/http#body'(C,_D)
    )
).
query(
    (
        'http://www.w3.org/2011/http#methodName'(A,"POST"),
        'http://www.w3.org/2011/http#requestURI'(A,_B),
        'http://www.w3.org/2011/http#body'(A,_C),
        'http://www.w3.org/2011/http#resp'(A,D),
        'http://www.w3.org/2011/http#body'(D,_E)
    )
).

test :-
    compose,
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
