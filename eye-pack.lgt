:- object(eye_pack, implements(pack_protocol)).

    :- info([
        version is 21:1110:1436,
        author is 'Jos De Roo',
        date is 2021-11-11,
        comment is 'Pack manifest for EYE'
    ]).

    name('eye-pack').

    description('Euler Yet another proof Engine').

    license('MIT').

    home('https://github.com/josd/eye').

    version(
        21:1110:1436,
        stable,
        'https://github.com/josd/eye/archive/refs/tags/v21.1110.1436.zip',
        sha256 - 'd60e8db1c052c2d2181dcdaaa3681ca0eee26a15b242a372f45ff5ff22b1fd7c',
        [],
        ['SWI-Prolog']
    ).

:- end_object.
