% Good Cobbler
% Example from https://shs.hal.science/halshs-04148373/document
% Using functional logic http://intrologic.stanford.edu/chapters/chapter_11.html

:- op(1200, xfx, :+).

% some x is a good cobbler
ascribed(_, good(cobbler)) :+ true.

% query: is there some x which is good at some y
true :+ (ascribed(_, good(_)) :+ true).
