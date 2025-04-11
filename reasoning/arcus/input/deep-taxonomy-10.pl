% Deep taxonomy
% See https://web.archive.org/web/20101025233525/http://www.ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf

:- op(1200, xfx, :+).

'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('urn:example:z', 'urn:example:N0').

'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N1') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N0').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:I1') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N0').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:J1') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N0').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N2') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N1').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:I2') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N1').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:J2') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N1').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N3') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N2').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:I3') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N2').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:J3') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N2').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N4') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N3').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:I4') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N3').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:J4') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N3').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N5') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N4').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:I5') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N4').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:J5') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N4').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N6') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N5').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:I6') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N5').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:J6') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N5').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N7') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N6').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:I7') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N6').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:J7') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N6').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N8') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N7').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:I8') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N7').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:J8') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N7').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N9') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N8').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:I9') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N8').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:J9') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N8').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N10') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N9').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:I10') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N9').
'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:J10') :- 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(X, 'urn:example:N9').

% query
true :+ 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_, 'urn:example:N10').
