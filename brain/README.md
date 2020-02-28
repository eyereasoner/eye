# BRAnches od INtelligence

N3 proof engine inspired by http://www.ibiblio.org/obp/py4fun/prolog/prolog3.py
and is with depth first search and euler path anti-looping detection.
Thanks to Tim Berners-Lee and Dan Connolly for inventing N3 and for creating N3
running code.

Usage: python3 brain.py [--why] [--once] [--debug] triples
Test:  python3 brain.py --why http://josd.github.io/eye/brain/tree.n3

Remark that the N3 that is actually understood is very limited:
qnames, () lists, facts and => rules on a single line and not yet [] ; , ^ ! ^^ @
