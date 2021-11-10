:- object(eye_registry, implements(registry_protocol)).

    :- info([
        version is 1:0:0,
        author is 'Jos De Roo',
        date is 2021-11-10,
        comment is 'Pack registry for eye.'
    ]).

    name('eye-registry').

    description('Pack registry for eye.').

    home('https://github.com/josd/eye').

    clone('https://github.com/josd/eye.git').

    archive('https://github.com/josd/eye/eye.zip').

:- end_object.
