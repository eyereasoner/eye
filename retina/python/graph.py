# See https://en.wikipedia.org/wiki/Graph_theory

if __name__ == "__main__":
    oneway = [
        ('paris', 'orleans'),
        ('paris', 'chartres'),
        ('paris', 'amiens'),
        ('orleans', 'blois'),
        ('orleans', 'bourges'),
        ('blois', 'tours'),
        ('chartres', 'lemans'),
        ('lemans', 'angers'),
        ('lemans', 'tours'),
        ('angers', 'nantes')
    ]

    path = []

    path += oneway

    while True:
        ol = len(path)
        for (A, B) in oneway:
            for (C, D) in path:
                if B == C and (A, D) not in path:
                    path += [(A, D)]
        if len(path) == ol:
            break

    cases = [
        "path"
    ]

    for c in cases:
        print('[] :python-answer """%s = %s""".' % (c, eval(c)))
