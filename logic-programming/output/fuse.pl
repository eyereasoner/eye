:- op(1200, xfx, :+).

% inference fuse, return code 2
fuse(('<urn:example:color>'('<urn:example:stone>', '<urn:example:black>'), '<urn:example:color>'('<urn:example:stone>', '<urn:example:white>'))).
