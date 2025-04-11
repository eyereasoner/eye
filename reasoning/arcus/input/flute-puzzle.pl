% A logic puzzle involving jobs and musical instruments
%
% In conversation, Chris, Sandy and Pat discovered that they had distinct
% occupations and played distinct musical instruments. Also
%   1. Chris is married to the doctor.
%   2. The lawyer plays the piano.
%   3. Chris is not the engineer.
%   4. Sandy is a patient of the violinist.
% Who plays the flute?
%
% Original code from https://www.cs.toronto.edu/~hector/PublicTCSlides.pdf

:- op(1200, xfx, :+).

% people
person(chris).
person(sandy).
person(pat).

% solve puzzle
'urn:example:solution'(Flute) :-
    uniq_people(Doctor, Lawyer, Engineer),
    uniq_people(Piano, Violin, Flute),
    chris \= Doctor,        % Chris is married to the doctor.
    Lawyer = Piano,         % The lawyer plays the piano.
    Engineer \= chris,      % The engineer is not Chris.
    Violin = Doctor,        % Sandy is a patient of the violinist.
    sandy \= Violin.

uniq_people(A, B, C) :-
    person(A),
    person(B),
    person(C),
    A \= B,
    A \= C,
    B \= C.

% query
true :+ 'urn:example:solution'(_).
