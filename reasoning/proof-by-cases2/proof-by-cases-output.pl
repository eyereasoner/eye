:- op(1200, xfx, :+).

answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:water>', '<urn:example:InorganicCompound>')).
answer('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:water>', '<urn:example:Observablee>')).

% proof steps
step(('<urn:example:allPossibleCases>'(A, [type(A, '<urn:example:Solid>'), type(A, '<urn:example:Liquid>'), type(A, '<urn:example:Gas>')]):+type(A, '<urn:example:InorganicCompound>')), type('<urn:example:water>', '<urn:example:InorganicCompound>'), '<urn:example:allPossibleCases>'('<urn:example:water>', [type('<urn:example:water>', '<urn:example:Solid>'), type('<urn:example:water>', '<urn:example:Liquid>'), type('<urn:example:water>', '<urn:example:Gas>')])).
step((true:+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, _)), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:water>', '<urn:example:InorganicCompound>'), true).
step((true:+'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_, _)), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<urn:example:water>', '<urn:example:Observablee>'), true).
