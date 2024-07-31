% Good Cobbler
% Example from https://shs.hal.science/halshs-04148373/document
% Using functional logic http://intrologic.stanford.edu/chapters/chapter_11.html

% some x is a good cobbler
'urn:example:is'('https://eyereasoner.github.io/.well-known/genid/995bae55-140a-4a1a-b68f-bc0153f79503#x','urn:example:good'('urn:example:Cobbler')).

% query
% is there some x which is good at some y
query('urn:example:is'(_X,'urn:example:good'(_Y))).

test :-
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
