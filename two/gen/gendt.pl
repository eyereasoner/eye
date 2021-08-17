% Generating deep taxonomy

:- use_module(library(between)).
:- use_module(library(format)).

main :-
    open('dt.pl',write,Out),
    write(Out,'% Deep taxonomy\n'),
    write(Out,'% See http://ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf\n'),
    write(Out,'\n'),
    write(Out,'namespace(rdf,\'http://www.w3.org/1999/02/22-rdf-syntax-ns#\').\n'),
    write(Out,'namespace(that,\'http://josd.github.io/eye/two/sample-ns#\').\n'),
    write(Out,'\n'),
    write(Out,'rdf-type(that-z,that-n0).\n'),
    (   between(0,9999,I),
        J is I+1,
        format(Out,"rdf-type(X,that-n~d) :- rdf-type(X,that-n~d).~n",[J,I]),
        format(Out,"rdf-type(X,that-i~d) :- rdf-type(X,that-n~d).~n",[J,I]),
        format(Out,"rdf-type(X,that-j~d) :- rdf-type(X,that-n~d).~n",[J,I]),
        fail
    ;   true
    ),
    write(Out,'\n'),
    write(Out,'% test cases\n'),
    write(Out,'case(rdf-type(_ELEMENT,that-n1)).\n'),
    write(Out,'case(rdf-type(_ELEMENT,that-n10)).\n'),
    write(Out,'case(rdf-type(_ELEMENT,that-n100)).\n'),
    write(Out,'case(rdf-type(_ELEMENT,that-n1000)).\n'),
    write(Out,'case(rdf-type(_ELEMENT,that-n10000)).\n'),
    write(Out,'\n'),
    write(Out,'test :-\n'),
    write(Out,'    case(A),\n'),
    write(Out,'    A,\n'),
    write(Out,'    write(A),\n'),
    write(Out,'    write(\'.\\n\'),\n'),
    write(Out,'    fail.\n'),
    write(Out,'test :-\n'),
    write(Out,'    halt.\n'),
    close(Out),
    halt.
