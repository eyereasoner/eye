from datetime import date

def easter(year):
    "Returns Easter as a date object."
    "see http://code.activestate.com/recipes/576517-calculate-easter-western-given-a-year/"
    a = year % 19
    b = year // 100
    c = year % 100
    d = (19 * a + b - b // 4 - ((b - (b + 8) // 25 + 1) // 3) + 15) % 30
    e = (32 + 2 * (b % 4) + 2 * (c // 4) - d - (c % 4)) % 7
    f = d + e - 7 * ((a + 11 * d + 22 * e) // 451) + 114
    month = f // 31
    day = f % 31 + 1    
    return date(year, month, day)

if __name__ == "__main__":
    print('PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>')
    print('PREFIX : <http://josd.github.io/eye/resources#>')
    print('')
    for y in range(2019, 2040):
        print('[] :easter-date "%s"^^xsd:date.' % (easter(y)))
