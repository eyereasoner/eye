% inference fuse

:- op(1200, xfx, :+).

'<urn:example:color>'('<urn:example:stone>', '<urn:example:black>').
'<urn:example:color>'('<urn:example:stone>', '<urn:example:white>').

false :+
    '<urn:example:color>'(X, '<urn:example:black>'),
    '<urn:example:color>'(X, '<urn:example:white>').