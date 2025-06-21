#include <stdio.h>

// Function to compute the month and day of Easter for a given year
void compute_easter_date(int year, int* month, int* day) {
    // Meeus/Jones/Butcher Gregorian algorithm
    int a = year % 19;
    int b = year / 100;
    int c = year % 100;
    int d = b / 4;
    int e = b % 4;
    int f = (b + 8) / 25;
    int g = (b - f + 1) / 3;
    int h = (19 * a + b - d - g + 15) % 30;
    int i = c / 4;
    int k = c % 4;
    int l = (32 + 2 * e + 2 * i - h - k) % 7;
    int m = (a + 11 * h + 22 * l) / 451;
    int n = (h + l - 7 * m + 114) / 31;     // Month (3=March, 4=April)
    int p = (h + l - 7 * m + 114) % 31;     // Day offset

    *month = n;
    *day = p + 1;
}

int main() {
    printf("Easter Sunday dates from 2025 to 2050:\n");
    printf("--------------------------------------\n");

    for (int year = 2025; year <= 2050; year++) {
        int month, day;
        compute_easter_date(year, &month, &day);
        printf("%d: %s %d\n", year, (month == 3) ? "March" : "April", day);
    }

    return 0;
}

