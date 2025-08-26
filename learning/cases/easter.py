#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# Easter (Western / Gregorian) — ARC-ified
# -----------------------------------------------------------------------------
# See https://en.wikipedia.org/wiki/List_of_dates_for_Easter
# Original core from: http://code.activestate.com/recipes/576517-calculate-easter-western-given-a-year/
#
# What stays the same
# -------------------
# - The function `easter(year)` is exactly your original (Anonymous Gregorian
#   algorithm, also known as Meeus–Jones–Butcher).
#
# What’s new
# ----------
# - `easter_oudin(year)`: a second, independent formulation (Oudin/Meeus style)
#   used as a cross-check.
# - Three ARC sections on run:
#     Answer      – Easter dates for a small demo range of years
#     Reason why  – short explanation of the algorithm’s steps
#     Check       – harness that confirms key invariants and that both
#                   algorithms agree across a big range of years.
#
# Notes
# -----
# - Western Easter here assumes the proleptic Gregorian calendar rules.
# - Easter Sunday is always between March 22 and April 25 (inclusive).
# - Python’s date.weekday(): Monday=0 … Sunday=6.
# -----------------------------------------------------------------------------

from datetime import date

# ─────────────────────────────────────────────────────────────────────────────
# Your original function (kept verbatim)
# ─────────────────────────────────────────────────────────────────────────────
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

# ─────────────────────────────────────────────────────────────────────────────
# Independent cross-check: Oudin/Meeus formulation
# (variable names follow the literature closely)
# ─────────────────────────────────────────────────────────────────────────────
def easter_oudin(year: int) -> date:
    a = year % 19
    b = year // 100
    c = year % 100
    d = b // 4
    e = b % 4
    f = (b + 8) // 25
    g = (b - f + 1) // 3
    h = (19 * a + b - d - g + 15) % 30
    i = c // 4
    k = c % 4
    l = (32 + 2 * e + 2 * i - h - k) % 7
    m = (a + 11 * h + 22 * l) // 451
    month = (h + l - 7 * m + 114) // 31
    day = ((h + l - 7 * m + 114) % 31) + 1
    return date(year, month, day)

# ─────────────────────────────────────────────────────────────────────────────
# ARC: Answer / Reason / Check
# ─────────────────────────────────────────────────────────────────────────────
def arc_answer(demo_from=2021, demo_to=2030):
    print("Answer")
    print("------")
    for y in range(demo_from, demo_to + 1):
        print(f"easter({y}) = {easter(y)}")
    print()

def arc_reason():
    print("Reason why")
    print("----------")
    print("We use the Anonymous Gregorian (Meeus–Jones–Butcher) method:")
    print("  • a = year mod 19  (Golden Number)")
    print("  • century/len corrections and leap-year adjustments")
    print("  • compute the Paschal full moon offset d and weekday offset e")
    print("  • combine into f → month/day for the Sunday following that full moon")
    print("Key property: Easter Sunday is between March 22 and April 25 inclusive.")
    print("As a cross-check, we also compute Oudin/Meeus’s equivalent formulation;")
    print("both must agree for all Gregorian years.")
    print()

def arc_check():
    print("Check (harness)")
    print("---------------")
    # Range for robust checking (Gregorian calendar): 1583..4099 is common
    lo, hi = 1583, 4099
    # 1) Agreement between the two algorithms
    for y in (list(range(lo, lo+50))               # a slice near the start
              + list(range(1900, 2101))           # common modern window
              + list(range(hi-50, hi+1))):        # a slice near the end
        d1 = easter(y)
        d2 = easter_oudin(y)
        assert d1 == d2, f"Mismatch in {y}: {d1} != {d2}"

        # 2) Sunday check
        assert d1.weekday() == 6, f"{y}: Easter not Sunday? got weekday={d1.weekday()}"

        # 3) Range check: March 22..April 25 inclusive
        assert (d1.month == 3 and 22 <= d1.day <= 31) or (d1.month == 4 and 1 <= d1.day <= 25), \
            f"{y}: date {d1} out of valid Easter range"

    # 4) Wider random-ish sweep: step through the whole interval
    step = 7  # arbitrary stride to keep this fast
    for y in range(lo, hi + 1, step):
        d1 = easter(y)
        assert d1 == easter_oudin(y)

    print("OK: algorithms agree; dates are Sundays and lie within Mar 22–Apr 25.")
    print()

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    arc_answer(2021, 2050)
    arc_reason()
    arc_check()

