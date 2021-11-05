# See https://en.wikipedia.org/wiki/Inference

if __name__ == "__main__":
    element_of = [
        ('socrates', 'man')
    ]

    subclass_of = [
        ('man', 'human'),
        ('human', 'mortal')
    ]

    while True:
        ol = len(element_of)
        for (A, B) in element_of:
            for (C, D) in subclass_of:
                if C == B and (A, D) not in element_of:
                    element_of += [(A, D)]
        if len(element_of) == ol:
            break

    cases = [
        "element_of"
    ]

    for c in cases:
        print('[] :python-answer """%s = %s""".' % (c, eval(c)))
