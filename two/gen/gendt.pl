% Generating deep taxonomy

:- use_module(library(between)).
:- use_module(library(format)).

main :-
    open('dt.pl',write,Out),
    write(Out,'% Deep taxonomy\n'),
    write(Out,'% See http://ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf\n'),
    write(Out,'\n'),
    write(Out,'nsp(rdf_,\'http://www.w3.org/1999/02/22-rdf_syntax-ns#\').\n'),
    write(Out,'nsp(etc_,\'http://josd.github.io/eye/two/cases#\').\n'),
    write(Out,'\n'),
    write(Out,'rdf_type(etc_z,etc_n0).\n'),
    (   between(0,9999,I),
        J is I+1,
        format(Out,"rdf_type(X,etc_n~d) :- rdf_type(X,etc_n~d).~n",[J,I]),
        format(Out,"rdf_type(X,etc_i~d) :- rdf_type(X,etc_n~d).~n",[J,I]),
        format(Out,"rdf_type(X,etc_j~d) :- rdf_type(X,etc_n~d).~n",[J,I]),
        fail
    ;   true
    ),
    write(Out,'\n'),
    write(Out,'% test cases\n'),
    write(Out,'case(rdf_type(_ELEMENT,etc_n1)).\n'),
    write(Out,'case(rdf_type(_ELEMENT,etc_n10)).\n'),
    write(Out,'case(rdf_type(_ELEMENT,etc_n100)).\n'),
    write(Out,'case(rdf_type(_ELEMENT,etc_n1000)).\n'),
    write(Out,'case(rdf_type(_ELEMENT,etc_n10000)).\n'),
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
