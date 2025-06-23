#!/usr/bin/env python3
"""
Clinical-data negotiation demo (hard-coded tests, no interactive I/O)

  • Same rule-matrix as the C / JS versions
  • Python 3.9+ (uses Enum.auto and dataclasses)
"""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum, auto
from typing import Dict, Set, List


# ── Taxonomy ────────────────────────────────────────────────────
class DataField(Enum):
    PATIENT_ID   = auto()
    DOB          = auto()      # date of birth
    DIAGNOSIS    = auto()
    LAB_RESULTS  = auto()

    def label(self) -> str:
        return {
            DataField.PATIENT_ID:  "Patient-ID",
            DataField.DOB:         "Date-of-Birth",
            DataField.DIAGNOSIS:   "Diagnosis",
            DataField.LAB_RESULTS: "Lab-Results",
        }[self]


class Purpose(Enum):
    TREATMENT = auto()
    RESEARCH  = auto()
    BILLING   = auto()

    def label(self) -> str:
        return {
            Purpose.TREATMENT: "Treatment",
            Purpose.RESEARCH:  "Research",
            Purpose.BILLING:   "Billing",
        }[self]


class Result(Enum):
    GRANTED       = "GRANTED"
    GRANTED_DEID  = "GRANTED-WITH-DEID"
    DENIED        = "DENIED"


# ── Policy objects ───────────────────────────────────────────────
@dataclass
class FieldPolicy:
    allow: Dict[Purpose, bool]
    require_consent: bool
    allow_identifiable: bool


def make_demo_provider() -> Dict[DataField, FieldPolicy]:
    """Return the fixed rule-table used in the other languages."""
    allow_all = {Purpose.TREATMENT: True,
                 Purpose.RESEARCH:  True,
                 Purpose.BILLING:   True}

    return {
        # Patient-ID: treatment & billing allowed; research allowed only if
        # de-identified and with consent.
        DataField.PATIENT_ID: FieldPolicy(
            allow={Purpose.TREATMENT: True,
                   Purpose.RESEARCH:  True,     # <- !! was False in the buggy draft
                   Purpose.BILLING:   True},
            require_consent=True,
            allow_identifiable=True         # raw ID may leave only for TREAT/BILL
        ),

        # DOB: shareable everywhere, but never identifiable.
        DataField.DOB: FieldPolicy(
            allow=allow_all,
            require_consent=False,
            allow_identifiable=False
        ),

        # Diagnosis: shareable & identifiable for any purpose.
        DataField.DIAGNOSIS: FieldPolicy(
            allow=allow_all,
            require_consent=False,
            allow_identifiable=True
        ),

        # Lab results: research allowed if consent and de-identified.
        DataField.LAB_RESULTS: FieldPolicy(
            allow={Purpose.TREATMENT: True,
                   Purpose.RESEARCH:  True,
                   Purpose.BILLING:   False},
            require_consent=True,
            allow_identifiable=False
        ),
    }


# ── Negotiation engine ───────────────────────────────────────────
@dataclass
class Request:
    requester: str
    field: DataField
    purpose: Purpose
    wants_identifiable: bool
    patient_consent: bool


def negotiate(rule: FieldPolicy, req: Request) -> Result:
    # 1) purpose gate
    if not rule.allow.get(req.purpose, False):
        return Result.DENIED

    # 2) consent check
    if rule.require_consent and not req.patient_consent:
        return Result.DENIED

    # 3) identifiability
    if req.wants_identifiable:
        return Result.GRANTED if rule.allow_identifiable else Result.DENIED
    return Result.GRANTED_DEID


# ── Hard-coded test-vector ───────────────────────────────────────
def main() -> None:
    provider = make_demo_provider()

    tests: List[Request] = [
        Request("Dr.Smith",    DataField.DIAGNOSIS,   Purpose.TREATMENT,
                wants_identifiable=True,  patient_consent=False),

        Request("ResearchLab", DataField.DOB,         Purpose.RESEARCH,
                wants_identifiable=True,  patient_consent=True),

        Request("ResearchLab", DataField.DOB,         Purpose.RESEARCH,
                wants_identifiable=False, patient_consent=True),

        Request("ResearchLab", DataField.LAB_RESULTS, Purpose.RESEARCH,
                wants_identifiable=False, patient_consent=False),

        Request("BillingSvc",  DataField.PATIENT_ID,  Purpose.BILLING,
                wants_identifiable=True,  patient_consent=False),

        Request("ResearchLab", DataField.PATIENT_ID,  Purpose.RESEARCH,
                wants_identifiable=True,  patient_consent=False),

        Request("ResearchLab", DataField.PATIENT_ID,  Purpose.RESEARCH,
                wants_identifiable=False, patient_consent=True),
    ]

    print("=== Clinical-data Negotiation Demo ============================\n")
    for r in tests:
        res = negotiate(provider[r.field], r)
        print(f"Request by {r.requester:<12}  "
              f"Field={r.field.label():<13}  "
              f"Purpose={r.purpose.label():<9}  "
              f"Identifiable={'yes' if r.wants_identifiable else 'no ':<3}  "
              f"Consent={'yes' if r.patient_consent else 'no ':<3}  ->  {res.value}")
    print("\n===============================================================")


if __name__ == "__main__":
    main()

