# See https://en.wikipedia.org/wiki/Graph_theory

if __name__ == "__main__":
    oneway = [
        ('city#paris', 'city#orleans'),
        ('city#paris', 'city#chartres'),
        ('city#paris', 'city#amiens'),
        ('city#orleans', 'city#blois'),
        ('city#orleans', 'city#bourges'),
        ('city#blois', 'city#tours'),
        ('city#chartres', 'city#lemans'),
        ('city#lemans', 'city#angers'),
        ('city#lemans', 'city#tours'),
        ('city#angers', 'city#nantes')
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

    print('[] :python-result "path = %s".' % (path))
