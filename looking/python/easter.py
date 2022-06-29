# See https://en.wikipedia.org/wiki/List_of_dates_for_Easter
# Original code from http://code.activestate.com/recipes/576517-calculate-easter-western-given-a-year/

from datetime import date

def easter(year):
    "Returns Easter as a date object."
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
    for y in range(2021, 2050):
        print('[] :python-result "easter(%d) = %s".' % (y, easter(y)))
