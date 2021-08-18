% Generating deep taxonomy

:- use_module(library(between)).
:- use_module(library(format)).

main :-
    open('dt.pl',write,Out),
    write(Out,'% Deep taxonomy\n'),
    write(Out,'% See http://ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf\n'),
    write(Out,'\n'),
    write(Out,'context(type,\'http://www.w3.org/1999/02/22-rdf-syntax-ns#type\').\n'),
    write(Out,'\n'),
    write(Out,'type(z,n0).\n'),
    (   between(0,9999,I),
        J is I+1,
        format(Out,"type(X,n~d) :- type(X,n~d).~n",[J,I]),
        format(Out,"type(X,i~d) :- type(X,n~d).~n",[J,I]),
        format(Out,"type(X,j~d) :- type(X,n~d).~n",[J,I]),
        fail
    ;   true
    ),
    write(Out,'\n'),
    write(Out,'% test cases\n'),
    write(Out,'case(context(_PRED,_URI)).\n'),
    write(Out,'case(type(_ELEMENT,n1)).\n'),
    write(Out,'case(type(_ELEMENT,n10)).\n'),
    write(Out,'case(type(_ELEMENT,n100)).\n'),
    write(Out,'case(type(_ELEMENT,n1000)).\n'),
    write(Out,'case(type(_ELEMENT,n10000)).\n'),
    write(Out,'\n'),
    write(Out,'test :-\n'),
    write(Out,'    case(A),\n'),
    write(Out,'    A,\n'),
    write(Out,'    writeq(A),\n'),
    write(Out,'    write(\'.\\n\'),\n'),
    write(Out,'    fail.\n'),
    write(Out,'test :-\n'),
    write(Out,'    halt.\n'),
    close(Out),
    halt.
