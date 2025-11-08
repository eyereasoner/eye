#!/usr/bin/env python3
"""
P3 Temporal Ops — Final Version
Date/Time & Duration Arithmetic with RDF-style literals

Implements the following (all examples covered and verified):

  DAYTIME DURATION <-> DATE/TIME/DATE-TIME
  ----------------------------------------
  - func:subtract-dateTimes( xsd:dateTime, xsd:dateTime ) -> xsd:dayTimeDuration
  - func:subtract-dates( xsd:date, xsd:date ) -> xsd:dayTimeDuration
  - func:subtract-times( xsd:time, xsd:time ) -> xsd:dayTimeDuration

  - func:add-dayTimeDurations( xsd:dayTimeDuration, xsd:dayTimeDuration ) -> xsd:dayTimeDuration
  - func:subtract-dayTimeDurations( xsd:dayTimeDuration, xsd:dayTimeDuration ) -> xsd:dayTimeDuration

  - func:add-dayTimeDuration-to-dateTime( xsd:dateTime, xsd:dayTimeDuration ) -> xsd:dateTime
  - func:subtract-dayTimeDuration-from-dateTime( xsd:dateTime, xsd:dayTimeDuration ) -> xsd:dateTime

  - func:add-dayTimeDuration-to-date( xsd:date, xsd:dayTimeDuration ) -> xsd:date
  - func:subtract-dayTimeDuration-from-date( xsd:date, xsd:dayTimeDuration ) -> xsd:date

  - func:add-dayTimeDuration-to-time( xsd:time, xsd:dayTimeDuration ) -> xsd:time
  - func:subtract-dayTimeDuration-from-time( xsd:time, xsd:dayTimeDuration ) -> xsd:time

  YEAR-MONTH DURATION <-> YEAR/MONTH FIELDS
  -----------------------------------------
  - func:add-yearMonthDurations( xsd:yearMonthDuration, xsd:yearMonthDuration ) -> xsd:yearMonthDuration
  - func:subtract-yearMonthDurations( xsd:yearMonthDuration, xsd:yearMonthDuration ) -> xsd:yearMonthDuration

  - func:add-yearMonthDuration-to-dateTime( xsd:dateTime, xsd:yearMonthDuration ) -> xsd:dateTime
  - func:subtract-yearMonthDuration-from-dateTime( xsd:dateTime, xsd:yearMonthDuration ) -> xsd:dateTime

  - func:add-yearMonthDuration-to-date( xsd:date, xsd:yearMonthDuration ) -> xsd:date
  - func:subtract-yearMonthDuration-from-date( xsd:date, xsd:yearMonthDuration ) -> xsd:date


Design principles and conventions
---------------------------------
• Literals are provided as RDF-style strings:  "lex"^^xsd:TYPE
• xsd:dayTimeDuration is parsed to/from datetime.timedelta (no fractional seconds in examples).
• xsd:yearMonthDuration is parsed to total months (integer) and formatted back as Y/M.
• When comparing or subtracting instants (dateTime/time with time zones), we normalize to UTC.
• xsd:time arithmetic:
    - If a time has a timezone (e.g., "Z" or "±HH:MM"), we do the math in that timezone and return the
      time of day in the SAME timezone, modulo 24h (days are discarded for the result).
    - If both times are floating (no tz), we treat them as same-day wall times.
    - Special case "24:00:00" (floating): treated as "00:00:00" ON THE SAME DAY (no rollover).
• xsd:date arithmetic: a dayTimeDuration added/subtracted yields a date; we add the duration at
  midnight UTC if the input is "…Z", then take the resulting UTC calendar date; for floating dates,
  we operate in naive mode and return the resulting calendar date (no tz suffix).
• Year/month adds/subtracts clamp the day to the last valid day of the target month (e.g., adding
  1 month to Jan 31 => Feb 29 in leap years, else Feb 28). Time of day and timezone are preserved.
• Duration formatting matches your examples:
    - Zero -> "PT0S"
    - No non-zero time units -> omit 'T'
    - No spurious "P0D" or "P0DT…"; negative sign applied once.

P3 triad output
---------------
• === Answer ===    JSON with each example's expected vs computed output.
• === Reason Why === Explanations of how each op is computed.
• === Check ===     PASS/FAIL per example + overall status.
"""

from __future__ import annotations
import re
import json
from dataclasses import dataclass
from typing import Tuple, Optional, List, Callable
from datetime import datetime, date, time, timedelta, timezone

# ----------------------------
# RDF literal parsing helpers
# ----------------------------

LIT_RE = re.compile(r'^\s*"(?P<lex>.+)"\s*\^\^\s*(?P<dt>[\w:]+)\s*$')

def parse_literal(lit: str) -> Tuple[str, str]:
    m = LIT_RE.match(lit)
    if not m:
        raise ValueError(f"Bad literal syntax: {lit}")
    return m["lex"], m["dt"]

# ----------------------------
# Duration parsing/formatting
# ----------------------------

DT_DUR_RE = re.compile(
    r'^(?P<sign>-)?P(?:(?P<days>\d+)D)?(?:T(?:(?P<h>\d+)H)?(?:(?P<m>\d+)M)?(?:(?P<s>\d+)(?:\.\d+)?S)?)?$'
)
YM_DUR_RE = re.compile(r'^(?P<sign>-)?P(?:(?P<y>\d+)Y)?(?:(?P<m>\d+)M)?$')

def parse_dayTimeDuration(lex: str) -> timedelta:
    m = DT_DUR_RE.match(lex)
    if not m:
        raise ValueError(f"Bad xsd:dayTimeDuration: {lex}")
    sign = -1 if m["sign"] else 1
    days = int(m["days"]) if m["days"] else 0
    hh = int(m["h"]) if m["h"] else 0
    mm = int(m["m"]) if m["m"] else 0
    ss = int(m["s"]) if m["s"] else 0
    td = timedelta(days=days, hours=hh, minutes=mm, seconds=ss)
    return sign * td

def format_dayTimeDuration(td: timedelta) -> str:
    # Emit canonical-ish forms that match the examples exactly.
    total_seconds = int(td.total_seconds())
    if total_seconds == 0:
        return "PT0S"

    sign = "-" if total_seconds < 0 else ""
    total_seconds = abs(total_seconds)
    days, rem = divmod(total_seconds, 86400)
    hours, rem = divmod(rem, 3600)
    minutes, seconds = divmod(rem, 60)

    parts = ["P"]
    if days:
        parts.append(f"{days}D")

    if hours or minutes or seconds:
        parts.append("T")
        if hours:
            parts.append(f"{hours}H")
        if minutes:
            parts.append(f"{minutes}M")
        if seconds:
            parts.append(f"{seconds}S")

    return sign + "".join(parts)

@dataclass(frozen=True)
class YMDur:
    months: int  # total months, sign included

def parse_yearMonthDuration(lex: str) -> YMDur:
    m = YM_DUR_RE.match(lex)
    if not m:
        raise ValueError(f"Bad xsd:yearMonthDuration: {lex}")
    sign = -1 if m["sign"] else 1
    y = int(m["y"]) if m["y"] else 0
    mo = int(m["m"]) if m["m"] else 0
    return YMDur(sign * (12*y + mo))

def format_yearMonthDuration(d: YMDur) -> str:
    sign = "-" if d.months < 0 else ""
    n = abs(d.months)
    y, m = divmod(n, 12)
    parts = []
    if y: parts.append(f"{y}Y")
    if m: parts.append(f"{m}M")
    if not parts:
        parts = ["0M"]
    return f"{sign}P{''.join(parts)}"

# ----------------------------
# dateTime / date / time parsing and formatting
# ----------------------------

def parse_xsd_dateTime(lex: str) -> datetime:
    # Accept 'Z' and ±HH:MM
    return datetime.fromisoformat(lex.replace("Z", "+00:00"))

def parse_xsd_date(lex: str) -> Tuple[date, bool]:
    # "YYYY-MM-DD" or "YYYY-MM-DDZ"
    had_z = lex.endswith("Z")
    if had_z:
        lex = lex[:-1]
    return date.fromisoformat(lex), had_z

TIME_TZ_RE = re.compile(
    r'^(?P<h>\d{2}):(?P<m>\d{2}):(?P<s>\d{2})(?P<off>Z|[+-]\d{2}:\d{2})?$'
)

def tzinfo_from_suffix(suffix: Optional[str]) -> Optional[timezone]:
    if suffix is None:
        return None
    if suffix == "Z":
        return timezone.utc
    sign = 1 if suffix[0] == "+" else -1
    oh, om = map(int, suffix[1:].split(":"))
    return timezone(sign * timedelta(hours=oh, minutes=om))

def parse_xsd_time_to_local_dt(lex: str) -> Tuple[datetime, Optional[str]]:
    """
    Build a timezone-aware or naive datetime on reference date 1970-01-01 representing the LOCAL clock time.
    Returns (local_dt, tz_suffix) where tz_suffix is "Z" / "±HH:MM" / None.
    Special case (no tz): "24:00:00" becomes 00:00:00 on the same day (no rollover).
    """
    m = TIME_TZ_RE.match(lex)
    if not m:
        raise ValueError(f"Bad xsd:time: {lex}")
    h = int(m["h"]); mi = int(m["m"]); s = int(m["s"])
    off = m["off"]
    if off is None and h == 24 and mi == 0 and s == 0:
        h = 0
    tz = tzinfo_from_suffix(off)
    return datetime(1970, 1, 1, h, mi, s, tzinfo=tz), off

def format_date_with_optional_Z(d: date, had_z: bool) -> str:
    return f"\"{d.isoformat()}{'Z' if had_z else ''}\"^^xsd:date"

def format_time_with_suffix(local_dt: datetime, tz_suffix: Optional[str]) -> str:
    """
    Takes a datetime (should be naive for floating, or aware in the proper tz) and formats "HH:MM:SS" + tz suffix.
    If tz_suffix is provided, ensure time-of-day is expressed in that tz.
    """
    if tz_suffix is None:
        return f"\"{local_dt.time().strftime('%H:%M:%S')}\"^^xsd:time"
    tz = tzinfo_from_suffix(tz_suffix)
    t_local = local_dt.astimezone(tz).time()
    suffix = "Z" if tz_suffix == "Z" else tz_suffix
    return f"\"{t_local.strftime('%H:%M:%S')}{suffix}\"^^xsd:time"

def format_datetime_preserve_z(dt: datetime) -> str:
    # If dt is UTC, emit 'Z'; otherwise ±HH:MM
    iso = dt.isoformat()
    if iso.endswith("+00:00"):
        iso = iso[:-6] + "Z"
    return f"\"{iso}\"^^xsd:dateTime"

# ----------------------------
# Month arithmetic helpers
# ----------------------------

def last_day_of_month(y: int, m: int) -> int:
    if m in (1,3,5,7,8,10,12): return 31
    if m in (4,6,9,11): return 30
    # February
    if (y % 4 == 0 and (y % 100 != 0 or y % 400 == 0)):
        return 29
    return 28

def add_months_ym(y: int, m: int, delta_months: int) -> Tuple[int, int]:
    total = (y * 12 + (m - 1)) + delta_months
    ny = total // 12
    nm = (total % 12) + 1
    return ny, nm

def add_yearMonth_to_ymd(y: int, m: int, d: int, delta_months: int) -> Tuple[int, int, int]:
    ny, nm = add_months_ym(y, m, delta_months)
    dmax = last_day_of_month(ny, nm)
    nd = d if d <= dmax else dmax
    return ny, nm, nd

# ----------------------------
# Operations: subtract (existing)
# ----------------------------

def func_subtract_dateTimes(a_lit: str, b_lit: str) -> str:
    a_lex, a_dt = parse_literal(a_lit)
    b_lex, b_dt = parse_literal(b_lit)
    if a_dt != "xsd:dateTime" or b_dt != "xsd:dateTime":
        raise TypeError("subtract-dateTimes expects xsd:dateTime arguments")
    a = parse_xsd_dateTime(a_lex).astimezone(timezone.utc)
    b = parse_xsd_dateTime(b_lex).astimezone(timezone.utc)
    td = a - b
    return f"\"{format_dayTimeDuration(td)}\"^^xsd:dayTimeDuration"

def func_subtract_dates(a_lit: str, b_lit: str) -> str:
    a_lex, a_dt = parse_literal(a_lit)
    b_lex, b_dt = parse_literal(b_lit)
    if a_dt != "xsd:date" or b_dt != "xsd:date":
        raise TypeError("subtract-dates expects xsd:date arguments")
    a, _za = parse_xsd_date(a_lex)
    b, _zb = parse_xsd_date(b_lex)
    td = timedelta(days=(a - b).days)
    return f"\"{format_dayTimeDuration(td)}\"^^xsd:dayTimeDuration"

def func_subtract_times(a_lit: str, b_lit: str) -> str:
    a_lex, a_dt = parse_literal(a_lit)
    b_lex, b_dt = parse_literal(b_lit)
    if a_dt != "xsd:time" or b_dt != "xsd:time":
        raise TypeError("subtract-times expects xsd:time arguments")

    # Parse to local reference dts (naive or with tzinfo)
    a_local, a_suffix = parse_xsd_time_to_local_dt(a_lex)
    b_local, b_suffix = parse_xsd_time_to_local_dt(b_lex)

    # To compare instants with tz, convert to UTC; for floating, compare as naive times on same ref date.
    if a_suffix is not None or b_suffix is not None:
        a_utc = a_local.astimezone(timezone.utc) if a_local.tzinfo else a_local.replace(tzinfo=timezone.utc)
        b_utc = b_local.astimezone(timezone.utc) if b_local.tzinfo else b_local.replace(tzinfo=timezone.utc)
        td = a_utc - b_utc
    else:
        td = a_local - b_local

    return f"\"{format_dayTimeDuration(td)}\"^^xsd:dayTimeDuration"

# ----------------------------
# Operations: dayTimeDuration +/- dayTimeDuration
# ----------------------------

def func_add_dayTimeDurations(a_lit: str, b_lit: str) -> str:
    a_lex, a_dt = parse_literal(a_lit)
    b_lex, b_dt = parse_literal(b_lit)
    if a_dt != "xsd:dayTimeDuration" or b_dt != "xsd:dayTimeDuration":
        raise TypeError("add-dayTimeDurations expects dayTimeDuration arguments")
    td = parse_dayTimeDuration(a_lex) + parse_dayTimeDuration(b_lex)
    return f"\"{format_dayTimeDuration(td)}\"^^xsd:dayTimeDuration"

def func_subtract_dayTimeDurations(a_lit: str, b_lit: str) -> str:
    a_lex, a_dt = parse_literal(a_lit)
    b_lex, b_dt = parse_literal(b_lit)
    if a_dt != "xsd:dayTimeDuration" or b_dt != "xsd:dayTimeDuration":
        raise TypeError("subtract-dayTimeDurations expects dayTimeDuration arguments")
    td = parse_dayTimeDuration(a_lex) - parse_dayTimeDuration(b_lex)
    return f"\"{format_dayTimeDuration(td)}\"^^xsd:dayTimeDuration"

# ----------------------------
# Operations: yearMonthDuration +/- yearMonthDuration  ← (ADDED)
# ----------------------------

def func_add_yearMonthDurations(a_lit: str, b_lit: str) -> str:
    a_lex, a_dt = parse_literal(a_lit)
    b_lex, b_dt = parse_literal(b_lit)
    if a_dt != "xsd:yearMonthDuration" or b_dt != "xsd:yearMonthDuration":
        raise TypeError("add-yearMonthDurations expects yearMonthDuration arguments")
    a = parse_yearMonthDuration(a_lex)
    b = parse_yearMonthDuration(b_lex)
    res = YMDur(a.months + b.months)
    return f"\"{format_yearMonthDuration(res)}\"^^xsd:yearMonthDuration"

def func_subtract_yearMonthDurations(a_lit: str, b_lit: str) -> str:
    a_lex, a_dt = parse_literal(a_lit)
    b_lex, b_dt = parse_literal(b_lit)
    if a_dt != "xsd:yearMonthDuration" or b_dt != "xsd:yearMonthDuration":
        raise TypeError("subtract-yearMonthDurations expects yearMonthDuration arguments")
    a = parse_yearMonthDuration(a_lex)
    b = parse_yearMonthDuration(b_lex)
    res = YMDur(a.months - b.months)
    return f"\"{format_yearMonthDuration(res)}\"^^xsd:yearMonthDuration"

# ----------------------------
# Operations: add/subtract yearMonthDuration to/from dateTime/date
# ----------------------------

def func_add_yearMonthDuration_to_dateTime(dt_lit: str, ym_lit: str) -> str:
    dt_lex, dt_typ = parse_literal(dt_lit)
    ym_lex, ym_typ = parse_literal(ym_lit)
    if dt_typ != "xsd:dateTime" or ym_typ != "xsd:yearMonthDuration":
        raise TypeError("add-yearMonthDuration-to-dateTime expects (dateTime, yearMonthDuration)")
    dta = parse_xsd_dateTime(dt_lex)
    months = parse_yearMonthDuration(ym_lex).months
    ny, nm, nd = add_yearMonth_to_ymd(dta.year, dta.month, dta.day, months)
    res = dta.replace(year=ny, month=nm, day=nd)
    return format_datetime_preserve_z(res)

def func_subtract_yearMonthDuration_from_dateTime(dt_lit: str, ym_lit: str) -> str:
    dt_lex, dt_typ = parse_literal(dt_lit)
    ym_lex, ym_typ = parse_literal(ym_lit)
    if dt_typ != "xsd:dateTime" or ym_typ != "xsd:yearMonthDuration":
        raise TypeError("subtract-yearMonthDuration-from-dateTime expects (dateTime, yearMonthDuration)")
    dta = parse_xsd_dateTime(dt_lex)
    months = parse_yearMonthDuration(ym_lex).months
    ny, nm, nd = add_yearMonth_to_ymd(dta.year, dta.month, dta.day, -months)
    res = dta.replace(year=ny, month=nm, day=nd)
    return format_datetime_preserve_z(res)

def func_add_yearMonthDuration_to_date(date_lit: str, ym_lit: str) -> str:
    d_lex, d_typ = parse_literal(date_lit)
    ym_lex, ym_typ = parse_literal(ym_lit)
    if d_typ != "xsd:date" or ym_typ != "xsd:yearMonthDuration":
        raise TypeError("add-yearMonthDuration-to-date expects (date, yearMonthDuration)")
    d, had_z = parse_xsd_date(d_lex)
    months = parse_yearMonthDuration(ym_lex).months
    ny, nm, nd = add_yearMonth_to_ymd(d.year, d.month, d.day, months)
    return format_date_with_optional_Z(date(ny, nm, nd), had_z)

def func_subtract_yearMonthDuration_from_date(date_lit: str, ym_lit: str) -> str:
    d_lex, d_typ = parse_literal(date_lit)
    ym_lex, ym_typ = parse_literal(ym_lit)
    if d_typ != "xsd:date" or ym_typ != "xsd:yearMonthDuration":
        raise TypeError("subtract-yearMonthDuration-from-date expects (date, yearMonthDuration)")
    d, had_z = parse_xsd_date(d_lex)
    months = parse_yearMonthDuration(ym_lex).months
    ny, nm, nd = add_yearMonth_to_ymd(d.year, d.month, d.day, -months)
    return format_date_with_optional_Z(date(ny, nm, nd), had_z)

# ----------------------------
# Operations: add/subtract dayTimeDuration to/from dateTime/date/time
# ----------------------------

def func_add_dayTimeDuration_to_dateTime(dt_lit: str, dtd_lit: str) -> str:
    dt_lex, dt_typ = parse_literal(dt_lit)
    dtd_lex, dtd_typ = parse_literal(dtd_lit)
    if dt_typ != "xsd:dateTime" or dtd_typ != "xsd:dayTimeDuration":
        raise TypeError("add-dayTimeDuration-to-dateTime expects (dateTime, dayTimeDuration)")
    base = parse_xsd_dateTime(dt_lex)
    td = parse_dayTimeDuration(dtd_lex)
    res = base + td
    return format_datetime_preserve_z(res)

def func_subtract_dayTimeDuration_from_dateTime(dt_lit: str, dtd_lit: str) -> str:
    dt_lex, dt_typ = parse_literal(dt_lit)
    dtd_lex, dtd_typ = parse_literal(dtd_lit)
    if dt_typ != "xsd:dateTime" or dtd_typ != "xsd:dayTimeDuration":
        raise TypeError("subtract-dayTimeDuration-from-dateTime expects (dateTime, dayTimeDuration)")
    base = parse_xsd_dateTime(dt_lex)
    td = parse_dayTimeDuration(dtd_lex)
    res = base - td
    return format_datetime_preserve_z(res)

def func_add_dayTimeDuration_to_date(date_lit: str, dtd_lit: str) -> str:
    d_lex, d_typ = parse_literal(date_lit)
    dtd_lex, dtd_typ = parse_literal(dtd_lit)
    if d_typ != "xsd:date" or dtd_typ != "xsd:dayTimeDuration":
        raise TypeError("add-dayTimeDuration-to-date expects (date, dayTimeDuration)")
    d, had_z = parse_xsd_date(d_lex)
    td = parse_dayTimeDuration(dtd_lex)
    if had_z:
        # operate at UTC midnight, then take resulting UTC date
        base = datetime(d.year, d.month, d.day, tzinfo=timezone.utc)
        res_date = (base + td).astimezone(timezone.utc).date()
        return format_date_with_optional_Z(res_date, True)
    else:
        base = datetime(d.year, d.month, d.day)
        res_date = (base + td).date()
        return format_date_with_optional_Z(res_date, False)

def func_subtract_dayTimeDuration_from_date(date_lit: str, dtd_lit: str) -> str:
    d_lex, d_typ = parse_literal(date_lit)
    dtd_lex, dtd_typ = parse_literal(dtd_lit)
    if d_typ != "xsd:date" or dtd_typ != "xsd:dayTimeDuration":
        raise TypeError("subtract-dayTimeDuration-from-date expects (date, dayTimeDuration)")
    d, had_z = parse_xsd_date(d_lex)
    td = parse_dayTimeDuration(dtd_lex)
    if had_z:
        base = datetime(d.year, d.month, d.day, tzinfo=timezone.utc)
        res_date = (base - td).astimezone(timezone.utc).date()
        return format_date_with_optional_Z(res_date, True)
    else:
        base = datetime(d.year, d.month, d.day)
        res_date = (base - td).date()
        return format_date_with_optional_Z(res_date, False)

def func_add_dayTimeDuration_to_time(time_lit: str, dtd_lit: str) -> str:
    t_lex, t_typ = parse_literal(time_lit)
    dtd_lex, dtd_typ = parse_literal(dtd_lit)
    if t_typ != "xsd:time" or dtd_typ != "xsd:dayTimeDuration":
        raise TypeError("add-dayTimeDuration-to-time expects (time, dayTimeDuration)")
    local, suffix = parse_xsd_time_to_local_dt(t_lex)
    td = parse_dayTimeDuration(dtd_lex)

    # Do arithmetic in the original timezone (or floating)
    res_local = local + td
    if suffix is not None:
        # normalize to same tz, then drop the day; wrap modulo 24h by taking time-of-day
        return format_time_with_suffix(res_local, suffix)
    else:
        # floating: naive arithmetic, drop day
        return f"\"{(res_local.time()).strftime('%H:%M:%S')}\"^^xsd:time"

def func_subtract_dayTimeDuration_from_time(time_lit: str, dtd_lit: str) -> str:
    t_lex, t_typ = parse_literal(time_lit)
    dtd_lex, dtd_typ = parse_literal(dtd_lit)
    if t_typ != "xsd:time" or dtd_typ != "xsd:dayTimeDuration":
        raise TypeError("subtract-dayTimeDuration-from-time expects (time, dayTimeDuration)")
    local, suffix = parse_xsd_time_to_local_dt(t_lex)
    td = parse_dayTimeDuration(dtd_lex)
    res_local = local - td
    if suffix is not None:
        return format_time_with_suffix(res_local, suffix)
    else:
        return f"\"{(res_local.time()).strftime('%H:%M:%S')}\"^^xsd:time"

# ----------------------------
# Example cases (your full list)
# ----------------------------

EXAMPLES: List[Tuple[str, Callable[[], str], str, str]] = [
    # 4.8.1.22 func:subtract-dateTimes
    (
        '{("2000-10-30T06:12:00-05:00"^^xsd:dateTime "1999-11-28T09:00:00Z"^^xsd:dateTime) func:subtract-dateTimes "P337DT2H12M"^^xsd:dayTimeDuration}',
        lambda: func_subtract_dateTimes(
            '"2000-10-30T06:12:00-05:00"^^xsd:dateTime',
            '"1999-11-28T09:00:00Z"^^xsd:dateTime',
        ),
        '"P337DT2H12M"^^xsd:dayTimeDuration',
        ":subtract-dateTimes-1"
    ),
    # 4.8.1.23 func:subtract-dates
    (
        '{("2000-10-30Z"^^xsd:date "1999-11-28Z"^^xsd:date) func:subtract-dates "P337D"^^xsd:dayTimeDuration}',
        lambda: func_subtract_dates(
            '"2000-10-30Z"^^xsd:date',
            '"1999-11-28Z"^^xsd:date',
        ),
        '"P337D"^^xsd:dayTimeDuration',
        ":subtract-dates-1"
    ),
    # 4.8.1.24 func:subtract-times
    (
        '{("11:12:00Z"^^xsd:time "04:00:00-05:00"^^xsd:time) func:subtract-times "PT2H12M"^^xsd:dayTimeDuration}',
        lambda: func_subtract_times(
            '"11:12:00Z"^^xsd:time',
            '"04:00:00-05:00"^^xsd:time',
        ),
        '"PT2H12M"^^xsd:dayTimeDuration',
        ":subtract-times-1"
    ),
    (
        '{("11:00:00-05:00"^^xsd:time "21:30:00+05:30"^^xsd:time) func:subtract-times "PT0S"^^xsd:dayTimeDuration}',
        lambda: func_subtract_times(
            '"11:00:00-05:00"^^xsd:time',
            '"21:30:00+05:30"^^xsd:time',
        ),
        '"PT0S"^^xsd:dayTimeDuration',
        ":subtract-times-2"
    ),
    (
        '{("17:00:00-06:00"^^xsd:time "08:00:00+09:00"^^xsd:time) func:subtract-times "P1D"^^xsd:dayTimeDuration}',
        lambda: func_subtract_times(
            '"17:00:00-06:00"^^xsd:time',
            '"08:00:00+09:00"^^xsd:time',
        ),
        '"P1D"^^xsd:dayTimeDuration',
        ":subtract-times-3"
    ),
    (
        '{("24:00:00"^^xsd:time "23:59:59"^^xsd:time) func:subtract-times "-PT23H59M59S"^^xsd:dayTimeDuration}',
        lambda: func_subtract_times(
            '"24:00:00"^^xsd:time',
            '"23:59:59"^^xsd:time',
        ),
        '"-PT23H59M59S"^^xsd:dayTimeDuration',
        ":subtract-times-4"
    ),

    # 4.8.1.25 func:add-yearMonthDurations
    (
        '{("P2Y11M"^^xsd:yearMonthDuration "P3Y3M"^^xsd:yearMonthDuration) func:add-yearMonthDurations "P6Y2M"^^xsd:yearMonthDuration}',
        lambda: func_add_yearMonthDurations(
            '"P2Y11M"^^xsd:yearMonthDuration',
            '"P3Y3M"^^xsd:yearMonthDuration',
        ),
        '"P6Y2M"^^xsd:yearMonthDuration',
        ":add-yearMonthDurations-1"
    ),
    # 4.8.1.26 func:subtract-yearMonthDurations
    (
        '{("P2Y11M"^^xsd:yearMonthDuration "P3Y3M"^^xsd:yearMonthDuration) func:subtract-yearMonthDurations "-P4M"^^xsd:yearMonthDuration}',
        lambda: func_subtract_yearMonthDurations(
            '"P2Y11M"^^xsd:yearMonthDuration',
            '"P3Y3M"^^xsd:yearMonthDuration',
        ),
        '"-P4M"^^xsd:yearMonthDuration',
        ":subtract-yearMonthDurations-1"
    ),

    # 4.8.1.30 func:add-dayTimeDurations
    (
        '{("P2DT12H5M"^^xsd:dayTimeDuration "P5DT12H"^^xsd:dayTimeDuration) func:add-dayTimeDurations "P8DT5M"^^xsd:dayTimeDuration}',
        lambda: func_add_dayTimeDurations(
            '"P2DT12H5M"^^xsd:dayTimeDuration',
            '"P5DT12H"^^xsd:dayTimeDuration',
        ),
        '"P8DT5M"^^xsd:dayTimeDuration',
        ":add-dayTimeDurations-1"
    ),

    # 4.8.1.31 func:subtract-dayTimeDurations
    (
        '{("P2DT12H"^^xsd:dayTimeDuration "P1DT10H30M"^^xsd:dayTimeDuration) func:subtract-dayTimeDurations "P1DT1H30M"^^xsd:dayTimeDuration}',
        lambda: func_subtract_dayTimeDurations(
            '"P2DT12H"^^xsd:dayTimeDuration',
            '"P1DT10H30M"^^xsd:dayTimeDuration',
        ),
        '"P1DT1H30M"^^xsd:dayTimeDuration',
        ":subtract-dayTimeDurations-1"
    ),

    # 4.8.1.35 func:add-yearMonthDuration-to-dateTime
    (
        '{("2000-10-30T11:12:00Z"^^xsd:dateTime "P1Y2M"^^xsd:yearMonthDuration) func:add-yearMonthDuration-to-dateTime "2001-12-30T11:12:00Z"^^xsd:dateTime}',
        lambda: func_add_yearMonthDuration_to_dateTime(
            '"2000-10-30T11:12:00Z"^^xsd:dateTime',
            '"P1Y2M"^^xsd:yearMonthDuration',
        ),
        '"2001-12-30T11:12:00Z"^^xsd:dateTime',
        ":add-yearMonthDuration-to-dateTime-1"
    ),

    # 4.8.1.36 func:add-yearMonthDuration-to-date
    (
        '{("2000-10-30Z"^^xsd:date "P1Y2M"^^xsd:yearMonthDuration) func:add-yearMonthDuration-to-date "2001-12-30Z"^^xsd:date}',
        lambda: func_add_yearMonthDuration_to_date(
            '"2000-10-30Z"^^xsd:date',
            '"P1Y2M"^^xsd:yearMonthDuration',
        ),
        '"2001-12-30Z"^^xsd:date',
        ":add-yearMonthDuration-to-date-1"
    ),

    # 4.8.1.37 func:add-dayTimeDuration-to-dateTime
    (
        '{("2000-10-30T11:12:00Z"^^xsd:dateTime "P3DT1H15M"^^xsd:dayTimeDuration) func:add-dayTimeDuration-to-dateTime "2000-11-02T12:27:00Z"^^xsd:dateTime}',
        lambda: func_add_dayTimeDuration_to_dateTime(
            '"2000-10-30T11:12:00Z"^^xsd:dateTime',
            '"P3DT1H15M"^^xsd:dayTimeDuration',
        ),
        '"2000-11-02T12:27:00Z"^^xsd:dateTime',
        ":add-dayTimeDuration-to-dateTime-1"
    ),

    # 4.8.1.38 func:add-dayTimeDuration-to-date
    (
        '{("2004-10-30Z"^^xsd:date "P2DT2H30M0S"^^xsd:dayTimeDuration) func:add-dayTimeDuration-to-date "2004-11-01Z"^^xsd:date}',
        lambda: func_add_dayTimeDuration_to_date(
            '"2004-10-30Z"^^xsd:date',
            '"P2DT2H30M0S"^^xsd:dayTimeDuration',
        ),
        '"2004-11-01Z"^^xsd:date',
        ":add-dayTimeDuration-to-date-1"
    ),

    # 4.8.1.39 func:add-dayTimeDuration-to-time
    (
        '{("11:12:00Z"^^xsd:time "P3DT1H15M"^^xsd:dayTimeDuration) func:add-dayTimeDuration-to-time "12:27:00Z"^^xsd:time}',
        lambda: func_add_dayTimeDuration_to_time(
            '"11:12:00Z"^^xsd:time',
            '"P3DT1H15M"^^xsd:dayTimeDuration',
        ),
        '"12:27:00Z"^^xsd:time',
        ":add-dayTimeDuration-to-time-1"
    ),

    # 4.8.1.40 func:subtract-yearMonthDuration-from-dateTime
    (
        '{("2000-10-30T11:12:00Z"^^xsd:dateTime "P1Y2M"^^xsd:yearMonthDuration) func:subtract-yearMonthDuration-from-dateTime "1999-08-30T11:12:00Z"^^xsd:dateTime}',
        lambda: func_subtract_yearMonthDuration_from_dateTime(
            '"2000-10-30T11:12:00Z"^^xsd:dateTime',
            '"P1Y2M"^^xsd:yearMonthDuration',
        ),
        '"1999-08-30T11:12:00Z"^^xsd:dateTime',
        ":subtract-yearMonthDuration-from-dateTime-1"
    ),

    # 4.8.1.41 func:subtract-yearMonthDuration-from-date
    (
        '{("2000-10-30Z"^^xsd:date "P1Y2M"^^xsd:yearMonthDuration) func:subtract-yearMonthDuration-from-date "1999-08-30Z"^^xsd:date}',
        lambda: func_subtract_yearMonthDuration_from_date(
            '"2000-10-30Z"^^xsd:date',
            '"P1Y2M"^^xsd:yearMonthDuration',
        ),
        '"1999-08-30Z"^^xsd:date',
        ":subtract-yearMonthDuration-from-date-1"
    ),

    # 4.8.1.42 func:subtract-dayTimeDuration-from-dateTime
    (
        '{("2000-10-30T11:12:00Z"^^xsd:dateTime "P3DT1H15M"^^xsd:dayTimeDuration) func:subtract-dayTimeDuration-from-dateTime "2000-10-27T09:57:00Z"^^xsd:dateTime}',
        lambda: func_subtract_dayTimeDuration_from_dateTime(
            '"2000-10-30T11:12:00Z"^^xsd:dateTime',
            '"P3DT1H15M"^^xsd:dayTimeDuration',
        ),
        '"2000-10-27T09:57:00Z"^^xsd:dateTime',
        ":subtract-dayTimeDuration-from-dateTime-1"
    ),

    # 4.8.1.43 func:subtract-dayTimeDuration-from-date
    (
        '{("2000-10-30Z"^^xsd:date "P3DT1H15M"^^xsd:dayTimeDuration) func:subtract-dayTimeDuration-from-date "2000-10-26Z"^^xsd:date}',
        lambda: func_subtract_dayTimeDuration_from_date(
            '"2000-10-30Z"^^xsd:date',
            '"P3DT1H15M"^^xsd:dayTimeDuration',
        ),
        '"2000-10-26Z"^^xsd:date',
        ":subtract-dayTimeDuration-from-date-1"
    ),

    # 4.8.1.44 func:subtract-dayTimeDuration-from-time
    (
        '{("11:12:00Z"^^xsd:time "P3DT1H15M"^^xsd:dayTimeDuration) func:subtract-dayTimeDuration-from-time "09:57:00Z"^^xsd:time}',
        lambda: func_subtract_dayTimeDuration_from_time(
            '"11:12:00Z"^^xsd:time',
            '"P3DT1H15M"^^xsd:dayTimeDuration',
        ),
        '"09:57:00Z"^^xsd:time',
        ":subtract-dayTimeDuration-from-time-1"
    ),
    (
        '{("08:20:00-05:00"^^xsd:time "P23DT10H10M"^^xsd:dayTimeDuration) func:subtract-dayTimeDuration-from-time "22:10:00-05:00"^^xsd:time}',
        lambda: func_subtract_dayTimeDuration_from_time(
            '"08:20:00-05:00"^^xsd:time',
            '"P23DT10H10M"^^xsd:dayTimeDuration',
        ),
        '"22:10:00-05:00"^^xsd:time',
        ":subtract-dayTimeDuration-from-time-2"
    ),
]

# ----------------------------
# P3 triad
# ----------------------------

def main():
    # ANSWER: compute each example
    results = []
    for text, fn, expected, label in EXAMPLES:
        try:
            got = fn()
            ok = (got == expected)
        except Exception as e:
            got = f"ERROR: {e}"
            ok = False
        results.append({
            "case": text,
            "label": label,
            "expected": expected,
            "got": got,
            "ok": ok
        })

    print("=== Answer ===")
    print(json.dumps(results, indent=2, ensure_ascii=False, sort_keys=False))

    # REASON WHY
    reason_lines = [
        "subtract-dateTimes: normalize both xsd:dateTime values to UTC instants and subtract (A − B) to a dayTimeDuration.",
        "subtract-dates: compute calendar-day difference (A − B) and emit as dayTimeDuration with only the day part.",
        "subtract-times: if any tz present, anchor local times to 1970-01-01 in their tz, convert to UTC, then subtract; if both are floating, subtract same-day wall times. '24:00:00' (floating) == '00:00:00' same day.",
        "add/subtract-dayTimeDurations: add/subtract timedeltas; format canonically (no spurious P0D/T).",
        "add/subtract-yearMonthDurations: convert Y/M to total months, add/subtract, format Y/M.",
        "add/subtract-yearMonthDuration-to/from-dateTime: add months to (Y, M) and clamp day to month end; keep time and tz.",
        "add/subtract-yearMonthDuration-to/from-date: same as above on the (Y, M, D); preserve 'Z' suffix if present.",
        "add/subtract-dayTimeDuration-to/from-dateTime: add/subtract timedeltas directly.",
        "add/subtract-dayTimeDuration-to/from-date: add/subtract at midnight (UTC if 'Z'), take resulting calendar date.",
        "add/subtract-dayTimeDuration-to/from-time: do arithmetic in the time’s own timezone (or floating), then return the time-of-day in the same timezone; days are discarded in the result.",
    ]
    print("\n=== Reason Why ===")
    print("\n".join(reason_lines))

    # CHECK
    print("\n=== Check ===")
    overall = True
    for r in results:
        overall &= r["ok"]
        print(f" - [{'PASS' if r['ok'] else 'FAIL'}] {r['label']}: {r['case']}")
        if not r["ok"]:
            print(f"     expected: {r['expected']}")
            print(f"     got     : {r['got']}")
    print("Overall:", "OK" if overall else "FAIL")

if __name__ == "__main__":
    main()

