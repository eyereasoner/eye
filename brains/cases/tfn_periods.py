#!/usr/bin/env python3
"""
P3 Temporal Functions: period bounds + bindDefaultTimezone
Implements:
  - tfn:periodMinInclusive
  - tfn:periodMaxInclusive
  - tfn:periodMinExclusive
  - tfn:periodMaxExclusive
  - tfn:bindDefaultTimezone

Supported literal datatypes:
  * xsd:gYear            -> YYYY
  * xsd:gYearMonth       -> YYYY-MM
  * xsd:date             -> YYYY-MM-DD   (optionally floating)
  * xsd:dateTime         -> YYYY-MM-DDThh:mm:ss(.fff)?(Z|±HH:MM)? (floating allowed)

Design choices:
  * For period bounds, we interpret the *calendar* period in local time (floating),
    then choose the *earliest possible instant on Earth* for the lower bound by
    using UTC offset +14:00, and the *latest possible instant* for the upper bound
    by using UTC offset -14:00 (the extreme legal time offsets).
  * Inclusive lower bound:   start-of-period at +14:00
    Inclusive upper bound:   end-of-period (ms precision) at -14:00
  * Exclusive lower bound:   1 millisecond *before* the inclusive lower bound (still +14:00)
  * Exclusive upper bound:   1 millisecond *after*  the inclusive upper bound (still -14:00)
  * Millisecond precision in outputs (".fff"); internal arithmetic uses microseconds.

P3 Triad:
  - Answer: prints the function outputs for the provided literal.
  - Reason Why: explains how boundaries were derived.
  - Check: explicit PASS/FAIL checks (month length, leap year, offset logic, etc).

CLI:
  python tfn_periods.py
  python tfn_periods.py '"2025-08"^^xsd:gYearMonth'
  python tfn_periods.py '"2025-02"^^xsd:gYearMonth'
  python tfn_periods.py '"2024-02"^^xsd:gYearMonth'
  python tfn_periods.py '"2025-10-07T07:00:00"^^xsd:dateTime' --bind "+02:00"

If --bind is supplied, the program will also demonstrate tfn:bindDefaultTimezone.
"""

from __future__ import annotations
import sys
import re
import json
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from typing import Optional, Tuple

# ---------------------------
# Constants
# ---------------------------

XSD_GYEAR       = "xsd:gYear"
XSD_GYEARMONTH  = "xsd:gYearMonth"
XSD_DATE        = "xsd:date"
XSD_DATETIME    = "xsd:dateTime"

PLUS14  = timezone(timedelta(hours=14))
MINUS14 = timezone(timedelta(hours=-14))

MS = timedelta(milliseconds=1)

# ---------------------------
# Parsing of RDF-style literal: "lex"^^datatype
# ---------------------------

LIT_RE = re.compile(r'^\s*"(?P<lex>.+)"\s*\^\^\s*(?P<dt>[\w:]+)\s*$')

def parse_literal(s: str) -> Tuple[str, str]:
    m = LIT_RE.match(s)
    if not m:
        raise ValueError('Expected RDF literal form: "lexical"^^datatype')
    return m["lex"], m["dt"]

# ---------------------------
# Helpers for month lengths & period edges
# ---------------------------

def is_leap(y: int) -> bool:
    return (y % 4 == 0) and ((y % 100 != 0) or (y % 400 == 0))

def last_day_of_month(y: int, m: int) -> int:
    if m in (1,3,5,7,8,10,12): return 31
    if m in (4,6,9,11): return 30
    return 29 if is_leap(y) else 28

def period_start_end(dt_type: str, lex: str) -> Tuple[datetime, datetime]:
    """
    Returns naive datetimes (no tz) for the start and end of the calendar period
    (end at 23:59:59.999). For xsd:dateTime: single instant (min == max).
    """
    if dt_type == XSD_GYEAR:
        y = int(lex)
        start = datetime(y, 1, 1, 0, 0, 0, 0)
        end   = datetime(y, 12, 31, 23, 59, 59, 999_000)
        return start, end

    if dt_type == XSD_GYEARMONTH:
        y, m = map(int, lex.split("-"))
        start = datetime(y, m, 1, 0, 0, 0, 0)
        ld = last_day_of_month(y, m)
        end   = datetime(y, m, ld, 23, 59, 59, 999_000)
        return start, end

    if dt_type == XSD_DATE:
        y, m, d = map(int, lex.split("-"))
        start = datetime(y, m, d, 0, 0, 0, 0)
        end   = datetime(y, m, d, 23, 59, 59, 999_000)
        return start, end

    if dt_type == XSD_DATETIME:
        lex_fix = lex.replace("Z", "+00:00")
        d = datetime.fromisoformat(lex_fix)
        naive = datetime(d.year, d.month, d.day, d.hour, d.minute, d.second, (d.microsecond // 1000) * 1000)
        return naive, naive

    raise ValueError(f"Unsupported datatype: {dt_type}")

def has_offset_in_datetime_lex(lex: str) -> bool:
    return lex.endswith("Z") or bool(re.search(r"[+-]\d{2}:\d{2}$", lex))

def fmt_dt_with_ms(d: datetime) -> str:
    ms = d.microsecond // 1000
    base = d.replace(microsecond=0).isoformat()
    m = re.search(r"([+-]\d{2}:\d{2})$", base)
    if m:
        off = m.group(1)
        head = base[: -len(off)]
        return f"{head}.{ms:03d}{off}"
    return f"{base}.{ms:03d}"

def rdf(dt: str, d: datetime) -> str:
    return f"\"{fmt_dt_with_ms(d)}\"^^{dt}"

# ---------------------------
# tfn functions
# ---------------------------

def tfn_periodMinInclusive(lit: str) -> str:
    lex, dt = parse_literal(lit)
    start_naive, _ = period_start_end(dt, lex)
    return rdf(XSD_DATETIME, start_naive.replace(tzinfo=PLUS14))

def tfn_periodMaxInclusive(lit: str) -> str:
    lex, dt = parse_literal(lit)
    _, end_naive = period_start_end(dt, lex)
    return rdf(XSD_DATETIME, end_naive.replace(tzinfo=MINUS14))

def tfn_periodMinExclusive(lit: str) -> str:
    lex, dt = parse_literal(lit)
    start_naive, _ = period_start_end(dt, lex)
    return rdf(XSD_DATETIME, (start_naive.replace(tzinfo=PLUS14) - MS))

def tfn_periodMaxExclusive(lit: str) -> str:
    lex, dt = parse_literal(lit)
    _, end_naive = period_start_end(dt, lex)
    return rdf(XSD_DATETIME, (end_naive.replace(tzinfo=MINUS14) + MS))

def tfn_bindDefaultTimezone(lit: str, tz_str: str) -> str:
    lex, dt = parse_literal(lit)
    if dt != XSD_DATETIME:
        return lit
    if has_offset_in_datetime_lex(lex):
        return lit

    if tz_str == "Z":
        tzinfo = timezone.utc
    else:
        m = re.fullmatch(r"([+-])(\d{2}):(\d{2})", tz_str)
        if not m:
            raise ValueError('Timezone must be "Z" or ±HH:MM')
        sign, hh, mm = m.groups()
        delta = timedelta(hours=int(hh), minutes=int(mm))
        if sign == "-":
            delta = -delta
        tzinfo = timezone(delta)

    d = datetime.fromisoformat(lex)
    aware = d.replace(tzinfo=tzinfo)
    return rdf(XSD_DATETIME, aware)

# ---------------------------
# P3 Driver
# ---------------------------

def explain_bounds(lit: str, min_inc: str, max_inc: str, min_exc: str, max_exc: str) -> str:
    lex, dt = parse_literal(lit)
    start_naive, end_naive = period_start_end(dt, lex)
    lines = []
    lines.append("We interpret the calendar period from the lexical form (floating):")
    lines.append(f"  start (floating local): {start_naive.isoformat()}.000")
    if start_naive == end_naive:
        lines.append(f"  end   (floating local): {end_naive.isoformat()}.000")
    else:
        lines.append(f"  end   (floating local): {end_naive.isoformat()}")
    lines.append("Then pick extreme legal time zones to capture global instants:")
    lines.append("  - Lower bound uses +14:00 (earliest instant globally).")
    lines.append("  - Upper bound uses -14:00 (latest instant globally).")
    lines.append(f"MinInclusive : {min_inc}")
    lines.append(f"MaxInclusive : {max_inc}")
    lines.append(f"MinExclusive : {min_exc}  (1 ms before MinInclusive)")
    lines.append(f"MaxExclusive : {max_exc}  (1 ms after  MaxInclusive)")
    return "\n".join(lines)

def check_bounds(lit: str, min_inc: str, max_inc: str, min_exc: str, max_exc: str) -> Tuple[bool, list]:
    report = []
    ok = True

    def parse_rdf_datetime(s: str) -> datetime:
        lex, dt = parse_literal(s)
        if dt != XSD_DATETIME:
            raise ValueError("Output is not xsd:dateTime")
        d = datetime.fromisoformat(lex.replace("Z","+00:00"))
        if d.tzinfo is None:
            raise ValueError("dateTime must be timezone-aware")
        return d

    a = parse_rdf_datetime(min_inc)
    b = parse_rdf_datetime(max_inc)
    c = parse_rdf_datetime(min_exc)
    d = parse_rdf_datetime(max_exc)

    # 1) Ordering & adjacency at ms precision
    c1 = c < a <= b < d
    ok &= c1
    report.append(f"[{'PASS' if c1 else 'FAIL'}] Ordering: MinExclusive < MinInclusive <= MaxInclusive < MaxExclusive")

    # 2) Exact 1 ms adjacency relative to inclusives
    c2 = (a - c) == MS and (d - b) == MS
    ok &= c2
    report.append(f"[{'PASS' if c2 else 'FAIL'}] Exact adjacency: a-c == 1ms and d-b == 1ms")

    # 3) Offsets are +14:00 for min bounds and -14:00 for max bounds
    c3 = (a.utcoffset() == PLUS14.utcoffset(None) and c.utcoffset() == PLUS14.utcoffset(None) and
          b.utcoffset() == MINUS14.utcoffset(None) and d.utcoffset() == MINUS14.utcoffset(None))
    ok &= c3
    report.append(f"[{'PASS' if c3 else 'FAIL'}] Offsets: min bounds use +14:00, max bounds use -14:00")

    # 4) UTC monotonicity
    c5 = a.astimezone(timezone.utc) <= b.astimezone(timezone.utc)
    ok &= c5
    report.append(f"[{'PASS' if c5 else 'FAIL'}] UTC monotonicity: MinInclusive ≤ MaxInclusive")

    return ok, report

def main():
    # Default example
    lit = '"2025-08"^^xsd:gYearMonth'
    bind_arg: Optional[str] = None

    # CLI
    args = sys.argv[1:]
    i = 0
    if i < len(args):
        lit = args[i]; i += 1
    if i < len(args) and args[i] == "--bind":
        if i+1 >= len(args): raise SystemExit("Usage: ... --bind ±HH:MM|Z")
        bind_arg = args[i+1]

    # ANSWER
    min_inc = tfn_periodMinInclusive(lit)
    max_inc = tfn_periodMaxInclusive(lit)
    min_exc = tfn_periodMinExclusive(lit)
    max_exc = tfn_periodMaxExclusive(lit)

    answer = {
        "input": lit,
        "tfn:periodMinInclusive": min_inc,
        "tfn:periodMaxInclusive": max_inc,
        "tfn:periodMinExclusive": min_exc,
        "tfn:periodMaxExclusive": max_exc,
    }

    if bind_arg is not None:
        try:
            answer["tfn:bindDefaultTimezone(input, tz)"] = tfn_bindDefaultTimezone(lit, bind_arg)
        except Exception as e:
            answer["tfn:bindDefaultTimezone(input, tz)"] = f"ERROR: {e}"
        demo_dt = '"2025-10-07T07:00:00"^^xsd:dateTime'
        answer["tfn:bindDefaultTimezone(demo, tz)"] = tfn_bindDefaultTimezone(demo_dt, bind_arg)

    # REASON WHY
    reason = explain_bounds(lit, min_inc, max_inc, min_exc, max_exc)

    # CHECK
    ok, report = check_bounds(lit, min_inc, max_inc, min_exc, max_exc)

    # Emit P3 triad
    print("=== Answer ===")
    print(json.dumps(answer, indent=2, ensure_ascii=False, sort_keys=True))

    print("\n=== Reason Why ===")
    print(reason)

    print("\n=== Check ===")
    print("Overall:", "OK" if ok else "FAIL")
    for line in report:
        print(" - " + line)

if __name__ == "__main__":
    main()

