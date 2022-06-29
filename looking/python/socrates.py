# See https://en.wikipedia.org/wiki/Inference

if __name__ == "__main__":
    element_of = [
        ('who#socrates', 'class#man')
    ]

    subclass_of = [
        ('class#man', 'class#animate'),
        ('class#animate', 'class#mortal')
    ]

    while True:
        ol = len(element_of)
        for (A, B) in element_of:
            for (C, D) in subclass_of:
                if C == B and (A, D) not in element_of:
                    element_of += [(A, D)]
        if len(element_of) == ol:
            break

    print('[] :python-result "element_of = %s".' % (element_of))
