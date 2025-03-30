% Meta-interpretation
% Original code from https://www.youtube.com/watch?v=nmBkU-l1zyc&t=1870s

:- op(1200, xfx, :+).

'urn:example:mi'([], []).
'urn:example:mi'([G|Gs], []) :-
    head_body_(G, Goals, Gs),
    'urn:example:mi'(Goals, []).

head_body_('urn:example:mi'([], []), Rs, Rs).
head_body_('urn:example:mi'([G|Gs], []), [head_body_(G, Goals, Gs), 'urn:example:mi'(Goals, [])|Rs], Rs).

head_body_(head_body_(Head, Goals0, Goals), Rs, Rs) :-
    head_body_(Head, Goals0, Goals).

head_body_('urn:example:factorial'(0, s(0)), Rs, Rs).
head_body_('urn:example:factorial'(s(N), F), ['urn:example:factorial'(N, F1), prod(s(N), F1, F)|Rs], Rs).

head_body_(prod(0, _, 0), Rs, Rs).
head_body_(prod(s(N), M, P), [prod(N, M, K), sum(K, M, P)|Rs], Rs).

head_body_(sum(0, M, M), Rs, Rs).
head_body_(sum(s(N), M, s(K)), [sum(N, M, K)|Rs], Rs).

% query
true :+ 'urn:example:mi'(['urn:example:mi'(['urn:example:factorial'(s(s(s(s(s(0))))), _)], [])], []).
