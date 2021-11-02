# See https://en.wikipedia.org/wiki/Padovan_sequence
# Original code from https://www.geeksforgeeks.org/padovan-sequence/

def padovan(n):
    pPrevPrev, pPrev, pCurr, pNext = 1, 1, 1, 1
    for i in range(3, n+1):
        pNext = pPrevPrev + pPrev
        pPrevPrev = pPrev
        pPrev = pCurr
        pCurr = pNext
    return pNext;

if __name__ == "__main__":
    cases = [
        "padovan(0)",
        "padovan(1)",
        "padovan(2)",
        "padovan(3)",
        "padovan(4)",
        "padovan(5)",
        "padovan(6)",
        "padovan(91)",
        "padovan(283)",
        "padovan(3674)"
    ]

    for c in cases:
        print('[] :python-answer """%s = %s""".' % (c, eval(c)))
