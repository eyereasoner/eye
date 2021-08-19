% Generating deep taxonomy

:- use_module(library(between)).
:- use_module(library(format)).

main :-
    open('dt.pl',write,Out),
    write(Out,'% Deep taxonomy\n'),
    write(Out,'% See http://ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf\n'),
    write(Out,'\n'),
    write(Out,'n0(z).\n'),
    (   between(0,9999,I),
        J is I+1,
        format(Out,"n~d(X) :- n~d(X).~n",[J,I]),
        format(Out,"i~d(X) :- n~d(X).~n",[J,I]),
        format(Out,"j~d(X) :- n~d(X).~n",[J,I]),
        fail
    ;   true
    ),
    write(Out,'\n'),
    write(Out,'% test cases\n'),
    write(Out,'case(n1(_ELEMENT)).\n'),
    write(Out,'case(n10(_ELEMENT)).\n'),
    write(Out,'case(n100(_ELEMENT)).\n'),
    write(Out,'case(n1000(_ELEMENT)).\n'),
    write(Out,'case(n10000(_ELEMENT)).\n'),
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
