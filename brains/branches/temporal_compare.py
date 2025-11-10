#!/usr/bin/env python3
"""
P3-style Temporal Comparator (focus on ">")
Goal: decide whether A > B for ISO 8601 timestamps with UTC offsets.

Delivers:
  - Answer: boolean for "Is A > B?"
  - Reason Why: explicit normalization steps and delta
  - Check: a battery of independent checks with PASS/FAIL per item

Run:
  python temporal_compare.py "2025-10-07T07:00:00-12:00" "2025-10-07T07:00:00+12:00"
If no args are provided, the above pair is used by default.

Notes:
  * Supports 'Z' as +00:00, fractional seconds, and ±HH:MM offsets.
  * Two independent normalization paths:
      - stdlib (fromisoformat + astimezone)
      - manual (regex parse + arithmetic on offsets)
"""

from __future__ import annotations
import sys
import re
from dataclasses import dataclass
from datetime import datetime, timezone, timedelta
from typing import List, Tuple, Dict, Any

# ---------------------------
# Defaults + CLI
# ---------------------------

DEFAULT_A = "2025-10-07T07:00:00-12:00"
DEFAULT_B = "2025-10-07T07:00:00+12:00"

def read_inputs() -> Tuple[str, str]:
    if len(sys.argv) >= 3:
        return sys.argv[1], sys.argv[2]
    return DEFAULT_A, DEFAULT_B

# ---------------------------
# Parsing & Normalization
# ---------------------------

# Accepts fractional seconds and 'Z'
ISO_RE = re.compile(
    r"""^
    (?P<date>\d{4}-\d{2}-\d{2})
    T
    (?P<h>\d{2}):(?P<m>\d{2}):(?P<s>\d{2})
    (?P<f>\.\d+)?                              # optional fractional seconds
    (?P<offset>
        Z
        |
        (?P<sign>[+-])
        (?P<oh>\d{2})
        :
        (?P<om>\d{2})
    )
    $
    """,
    re.VERBOSE,
)

@dataclass(frozen=True)
class Parsed:
    aware: datetime   # timezone-aware original instant
    utc: datetime     # same instant normalized to UTC
    offset: timedelta # offset relative to UTC (e.g., -12:00 => -12h)

def parse_via_stdlib(s: str) -> Parsed:
    """
    Method #1 (primary): use Python’s ISO parser + astimezone(UTC).
    """
    s_fix = s.replace("Z", "+00:00")
    d = datetime.fromisoformat(s_fix)  # supports fractional seconds and ±HH:MM
    if d.tzinfo is None:
        raise ValueError(f"Timestamp must include a timezone offset: {s}")
    utc = d.astimezone(timezone.utc)
    return Parsed(aware=d, utc=utc, offset=d.utcoffset() or timedelta(0))

def parse_via_manual(s: str) -> Parsed:
    """
    Method #2 (independent): regex-parse fields and compute UTC via arithmetic.
    """
    m = ISO_RE.match(s)
    if not m:
        raise ValueError(f"Not valid ISO 8601 with offset: {s}")

    year, month, day = map(int, m["date"].split("-"))
    h = int(m["h"]); mi = int(m["m"]); sec = int(m["s"])
    frac = m["f"] or ""
    micro = 0
    if frac:
        # Convert fractional seconds to microseconds, trimming/padding as needed
        micro = int(float(frac) * 1_000_000 + 0.5)

    naive = datetime(year, month, day, h, mi, sec, micro)

    if m["offset"] == "Z":
        sign = "+"
        oh = "00"
        om = "00"
    else:
        sign = m["sign"]
        oh = m["oh"]
        om = m["om"]

    off = timedelta(hours=int(oh), minutes=int(om))
    if sign == "-":
        off = -off  # negative offset means local = UTC - 12h etc.

    # Relation: local = UTC + offset  =>  UTC = local - offset
    utc_naive = naive - off
    aware = naive.replace(tzinfo=timezone(off))
    utc = utc_naive.replace(tzinfo=timezone.utc)
    return Parsed(aware=aware, utc=utc, offset=off)

# ---------------------------
# Utilities
# ---------------------------

def to_epoch_seconds(d: datetime) -> float:
    """Epoch seconds of an aware datetime (UTC-aligned recommended)."""
    return d.timestamp()

def format_td(td: timedelta) -> str:
    total_us = int(td.total_seconds() * 1_000_000)
    sign = "-" if total_us < 0 else "+"
    total_us = abs(total_us)
    seconds, micros = divmod(total_us, 1_000_000)
    hh, rem = divmod(seconds, 3600)
    mm, ss = divmod(rem, 60)
    if micros:
        return f"{sign}{hh:02d}:{mm:02d}:{ss:02d}.{micros:06d}"
    return f"{sign}{hh:02d}:{mm:02d}:{ss:02d}"

def cmp_gt(a_utc: datetime, b_utc: datetime) -> bool:
    return a_utc > b_utc

# ---------------------------
# Explanations
# ---------------------------

def explain(a: Parsed, b: Parsed, result_gt: bool) -> str:
    lines = []
    lines.append("Normalize to UTC and compare instants:")
    lines.append(f"  A local: {a.aware.isoformat()}  (offset {format_td(a.offset)})")
    lines.append(f"  B local: {b.aware.isoformat()}  (offset {format_td(b.offset)})")
    lines.append(f"  A UTC : {a.utc.isoformat().replace('+00:00','Z')}")
    lines.append(f"  B UTC : {b.utc.isoformat().replace('+00:00','Z')}")
    delta = a.utc - b.utc
    if delta.total_seconds() == 0:
        lines.append("  Difference: 0 seconds (same instant).")
    else:
        when = "later than" if delta.total_seconds() > 0 else "earlier than"
        lines.append(f"  Difference: {format_td(delta)} ⇒ A is {when} B.")
    lines.append(f"Therefore, A > B is {result_gt}.")
    return "\n".join(lines)

# ---------------------------
# Explicit Checks (battery)
# ---------------------------

def run_checks(A: str, B: str) -> Tuple[bool, List[str]]:
    """
    Returns (overall_ok, report_lines).
    Each check adds a PASS/FAIL line with details.
    """
    report: List[str] = []
    ok = True

    # Parse via both methods
    a_std = parse_via_stdlib(A); b_std = parse_via_stdlib(B)
    a_man = parse_via_manual(A); b_man = parse_via_manual(B)

    # 1) UTC equality across methods
    c1 = (a_std.utc == a_man.utc) and (b_std.utc == b_man.utc)
    ok &= c1
    report.append(f"[{'PASS' if c1 else 'FAIL'}] stdlib UTC == manual UTC for A and B")

    # 2) Aware sameness of offsets (methods agree on offsets)
    c2 = (a_std.offset == a_man.offset) and (b_std.offset == b_man.offset)
    ok &= c2
    report.append(f"[{'PASS' if c2 else 'FAIL'}] stdlib offset == manual offset for A and B")

    # 3) Comparison consistency (both methods agree on A > B)
    gt_std = a_std.utc > b_std.utc
    gt_man = a_man.utc > b_man.utc
    c3 = (gt_std == gt_man)
    ok &= c3
    report.append(f"[{'PASS' if c3 else 'FAIL'}] stdlib (A>B) == manual (A>B)  => {gt_std}")

    # 4) Antisymmetry: if A > B then not (A < B) and not (A == B); if not, these should be coherent.
    lt_std = a_std.utc < b_std.utc
    eq_std = a_std.utc == b_std.utc
    c4 = not (gt_std and (lt_std or eq_std)) and not (eq_std and (gt_std or lt_std))
    ok &= c4
    report.append(f"[{'PASS' if c4 else 'FAIL'}] antisymmetry/trichotomy coherence (>, <, ==)")

    # 5) Sign of delta coheres with '>'
    delta = a_std.utc - b_std.utc
    c5 = (gt_std and delta.total_seconds() > 0) or ((not gt_std) and True)  # if not greater, we don't constrain sign
    ok &= c5
    report.append(f"[{'PASS' if c5 else 'FAIL'}] delta sign matches (A>B) when claimed greater (Δ={format_td(delta)})")

    # 6) Epoch seconds comparison == datetime comparison
    ea = to_epoch_seconds(a_std.utc); eb = to_epoch_seconds(b_std.utc)
    c6 = (ea > eb) == gt_std
    ok &= c6
    report.append(f"[{'PASS' if c6 else 'FAIL'}] epoch seconds comparison equals datetime comparison (ea={ea}, eb={eb})")

    # 7) Round-trip stability: isoformat -> parse again -> same UTC
    a_rt = parse_via_stdlib(a_std.aware.isoformat().replace("+00:00","Z"))
    b_rt = parse_via_stdlib(b_std.aware.isoformat().replace("+00:00","Z"))
    c7 = (a_rt.utc == a_std.utc) and (b_rt.utc == b_std.utc)
    ok &= c7
    report.append(f"[{'PASS' if c7 else 'FAIL'}] round-trip parse of isoformat preserves UTC")

    # 8) Offset identity: local == (UTC + offset)
    a_local_from_utc = (a_std.utc + a_std.offset).replace(tzinfo=a_std.aware.tzinfo)
    b_local_from_utc = (b_std.utc + b_std.offset).replace(tzinfo=b_std.aware.tzinfo)
    c8 = (a_local_from_utc == a_std.aware) and (b_local_from_utc == b_std.aware)
    ok &= c8
    report.append(f"[{'PASS' if c8 else 'FAIL'}] identity local = UTC + offset holds for A and B")

    # 9) Comparator invariance under equal shift: (A+k) > (B+k) for any k
    k = timedelta(hours=5, minutes=13, seconds=7, microseconds=123456)
    c9 = ((a_std.utc + k) > (b_std.utc + k)) == gt_std
    ok &= c9
    report.append(f"[{'PASS' if c9 else 'FAIL'}] invariance under equal temporal shift k")

    # 10) Z vs +00:00 equivalence (if any input is UTC)
    def z_equiv(s: str) -> bool:
        if s.endswith("Z"):
            z_std = parse_via_stdlib(s)
            plus_std = parse_via_stdlib(s.replace("Z","+00:00"))
            return z_std.utc == plus_std.utc
        return True
    c10 = z_equiv(A) and z_equiv(B)
    ok &= c10
    report.append(f"[{'PASS' if c10 else 'FAIL'}] 'Z' is equivalent to '+00:00' (where applicable)")

    return ok, report

# ---------------------------
# Main (P3 triad emitter)
# ---------------------------

def main():
    A, B = read_inputs()

    # ANSWER (using stdlib path to compute)
    a = parse_via_stdlib(A)
    b = parse_via_stdlib(B)
    answer_gt = cmp_gt(a.utc, b.utc)

    # REASON WHY
    reason = explain(a, b, answer_gt)

    # CHECK (explicit, independent)
    overall_ok, report = run_checks(A, B)

    # Emit P3 triad
    print("=== Answer ===")
    print({"goal": "Is A > B?", "A": A, "B": B, "result": answer_gt})

    print("\n=== Reason Why ===")
    print(reason)

    print("\n=== Check ===")
    status = "OK" if overall_ok else "FAIL"
    print(f"Overall: {status}")
    for line in report:
        print(" - " + line)

if __name__ == "__main__":
    main()

