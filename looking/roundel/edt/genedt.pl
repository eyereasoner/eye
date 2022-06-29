% Generating extended deep taxonomy

run :-
    open('edt.pl',write,Out),
    format(Out,"% Extended deep taxonomy~n",[]),
    format(Out,"% See http://ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf~n",[]),
    format(Out,"~n",[]),
    (   between(0,100000,I),
        format(Out,"'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('http://example.org/ns#i~d','http://example.org/ns#N0').~n",[I]),
        fail
    ;   true
    ),
    format(Out,"~n",[]),
    format(Out,"'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,C) :-~n",[]),
    format(Out,"    'http://www.w3.org/2000/01/rdf-schema#subClassOf'(B,C),~n",[]),
    format(Out,"    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(A,B).~n",[]),
    format(Out,"~n",[]),
    (   between(0,99999,I),
        J is I+1,
        format(Out,"'http://www.w3.org/2000/01/rdf-schema#subClassOf'('http://example.org/ns#N~d','http://example.org/ns#N~d').~n",[I,J]),
        format(Out,"'http://www.w3.org/2000/01/rdf-schema#subClassOf'('http://example.org/ns#N~d','http://example.org/ns#I~d').~n",[I,J]),
        format(Out,"'http://www.w3.org/2000/01/rdf-schema#subClassOf'('http://example.org/ns#N~d','http://example.org/ns#J~d').~n",[I,J]),
        fail
    ;   true
    ),
    format(Out,"~n",[]),
    format(Out,"% query~n",[]),
    format(Out,"query('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('http://example.org/ns#i100000','http://example.org/ns#N100000')).~n",[]),
    format(Out,"~n",[]),
    format(Out,"run :-~n",[]),
    format(Out,"    query(Q),~n",[]),
    format(Out,"    Q,~n",[]),
    format(Out,"    writeq(Q),~n",[]),
    format(Out,"    write('.\\n'),~n",[]),
    format(Out,"    fail;~n",[]),
    format(Out,"    true.~n",[]),
    close(Out).
