% Generating deep taxonomy

:- use_module(library(between)).
:- use_module(library(format)).

run :-
    open('examples/dt.pl',write,Out),
    write(Out,'% Deep taxonomy\n'),
    write(Out,'% See http://ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf\n'),
    write(Out,'\n'),
    write(Out,':- dynamic(\'http://www.w3.org/1999/02/22-rdf-syntax-ns#type\'/2).\n'),
    write(Out,'\n'),
    write(Out,'\'http://www.w3.org/1999/02/22-rdf-syntax-ns#type\'(\'http://example.org/ns#z\',\'http://example.org/ns#N0\').\n'),
    write(Out,'\n'),
    (   between(0,9999,I),
        J is I+1,
        format(Out,"'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X,'http://example.org/ns#N~d') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X,'http://example.org/ns#N~d').~n",[J,I]),
        format(Out,"'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X,'http://example.org/ns#I~d') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X,'http://example.org/ns#N~d').~n",[J,I]),
        format(Out,"'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X,'http://example.org/ns#J~d') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X,'http://example.org/ns#N~d').~n",[J,I]),
        fail
    ;   true
    ),
    write(Out,'\n'),
    write(Out,'% query\n'),
    write(Out,'\'http://www.w3.org/1999/02/22-rdf-syntax-ns#type\'(_ELEMENT,\'http://example.org/ns#N10000\') => true.\n'),
    close(Out).
