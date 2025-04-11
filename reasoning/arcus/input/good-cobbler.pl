% Good Cobbler
% Example from https://shs.hal.science/halshs-04148373/document
% Using functional logic http://intrologic.stanford.edu/chapters/chapter_11.html

:- op(1200, xfx, :+).

% some x is a good cobbler
'urn:example:is'(_, 'urn:example:good'('urn:example:Cobbler')) :+ true.

% query: is there some x which is good at some y
true :+ ('urn:example:is'(_, 'urn:example:good'(_)) :+ true).
