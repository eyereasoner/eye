#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
===============================================================================
Goal-Oriented Proof Demo over a Minimal Verifiable Data Model (Python, stdlib)
===============================================================================

What this script does
---------------------
1) Defines a small, human-centric Python data model (using dataclasses) for:
   - A Person ("Bob" with a name).
   - One or more Data Integrity "Proofs" that assert facts about either:
       * the person data ("about" == "person"), or
       * another proof's graph/metadata ("about" == ID of the other proof).

2) Provides a tiny *goal-oriented* (backward-chaining style) proof engine that
   evaluates a specific goal and prints an indented proof tree with ✔︎/✘:
   Goal: "The person record is asserted by EXPECTED_ISSUER at TIME using a
          supported proof; optionally also require a meta-proof over that proof."

3) Builds the example dataset from the original TriG content and runs 3 demo
   goals to illustrate success/failure reasons (issuer mismatch, validity window,
   meta-proof presence, etc.).

Why this shape?
---------------
- The original data was RDF/TriG with 3 named graphs: g0 (person facts),
  g1 (proof over g0), g3 (proof over g1). Instead of mirroring RDF syntax,
  this model captures the *meaning* directly in Python objects.

Mapping from RDF/TriG to this model
-----------------------------------
- g0:   :Bob foaf:name "Bob".             -> Person(id="Bob", name="Bob")
- g1:   proof about g0 (signature1)       -> Proof(id="signature1", about="person", ...)
- g3:   proof about g1 (signature2)       -> Proof(id="signature2", about="signature1", ...)

Assumptions & design choices
----------------------------
- Timezones: all datetimes must be *timezone-aware* (UTC recommended). The demo
  uses `timezone.utc`. If you pass naive datetimes, validation will raise.

- Validity window is treated as *inclusive* on both ends: [start, end].

- Cryptography: this demo does NOT verify signatures. It only checks the presence
  of a `proof.value` and structural fields (type/suite/purpose). Hooks are marked
  where real crypto verification could be integrated.

- Supported values:
  type  == "DataIntegrityProof"
  suite in {"ecdsa-rdfc-2019"}
  purpose == "assertionMethod"
  These are easily adjustable in `ProofEngine`.

- "Meta-proof" means: a second proof whose `about` equals the first proof's `id`.

- Issuer policy: the top-level goal constrains the *person proof's* issuer to
  match a caller-provided expected issuer. The meta-proof's issuer is not
  constrained (but you can add that policy if desired).

Extending this script
---------------------
- Add real cryptographic checks in `_check_supported` (replace the placeholder
  "proofValue is present" with actual signature verification).
- Add more policies: key revocation lists, method dereferencing, issuer allowlist,
  proof chain length limits, clock skew tolerance, etc.
- Swap the simple dataclasses for Pydantic or attrs if you need validation on
  object construction time.

How to run
----------
$ python verifiable_data.py

You will see 3 proof trees printed for three different goals:
  A) Valid time, correct issuer, meta-proof required  -> should succeed.
  B) Time after validUntil                           -> validity fails.
  C) Wrong issuer                                    -> issuer check fails.

===============================================================================
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import List, Optional, Iterable


# -----------------------------------------------------------------------------
# Domain model (dataclasses)
# -----------------------------------------------------------------------------

@dataclass
class Validity:
    """
    Represents an optional validity window. If provided, it is treated as
    inclusive on both ends: [start, end].

    Fields
    ------
    start: Optional[datetime]
        Earliest instant for which the proof is considered valid (inclusive).
    end: Optional[datetime]
        Latest instant for which the proof is considered valid (inclusive).

    Notes
    -----
    - Both `start` and `end` should be timezone-aware datetimes (UTC recommended).
    - If either endpoint is None, that side is considered unconstrained.
    """
    start: Optional[datetime] = None
    end: Optional[datetime] = None

    def contains(self, instant: datetime) -> bool:
        """
        Return True if `instant` lies within the inclusive range [start, end].
        Raises if `instant` is naive (no tzinfo).
        """
        if instant.tzinfo is None:
            raise ValueError("instant must be timezone-aware (UTC recommended)")
        if self.start and instant < self.start:
            return False
        if self.end and instant > self.end:
            return False
        return True


@dataclass
class Proof:
    """
    A Data Integrity proof over some subject.

    Fields
    ------
    id: str
        Identifier for this proof (e.g., "signature1").
    about: str
        What this proof is about. In this script:
          - "person"      -> the person data graph (facts about Bob)
          - "<proof id>"  -> the proof/graph identified by that id (meta-proof)
    type: str
        Proof type. Expected: "DataIntegrityProof".
    suite: str
        Cryptosuite label. Example: "ecdsa-rdfc-2019".
    created: datetime
        Proof creation instant (must not be in the future relative to evaluation).
    verification_method: str
        URI identifying the key/method used to verify.
    purpose: str
        Proof purpose (policy label). Expected: "assertionMethod".
    value: str
        Encoded proof value (e.g., signature bytes in a specific encoding).
    issuer: Optional[str]
        Issuer identifier/URI. Required by our goal for the *person* proof.
    valid: Optional[Validity]
        Optional validity window; if absent, considered unconstrained.
    """
    id: str
    about: str
    type: str
    suite: str
    created: datetime
    verification_method: str
    purpose: str
    value: str
    issuer: Optional[str] = None
    valid: Optional[Validity] = None


@dataclass
class Person:
    """
    The subject of the attested data. Here we keep only an id and a name for
    simplicity, matching the single triple (:Bob foaf:name "Bob").
    """
    id: str
    name: str


@dataclass
class Dataset:
    """
    A minimal container tying together:
      - the person record; and
      - the list of proofs about either the person or some other proof.

    Methods
    -------
    proofs_about(about: str) -> Iterable[Proof]
        Generator over proofs whose `about` equals the provided label/id.
    """
    person: Person
    proofs: List[Proof] = field(default_factory=list)

    def proofs_about(self, about: str) -> Iterable[Proof]:
        return (p for p in self.proofs if p.about == about)


# -----------------------------------------------------------------------------
# Proof engine (goal-oriented / backward-chaining flavor)
# -----------------------------------------------------------------------------

@dataclass
class ProofStep:
    """
    A single node in the proof tree.

    Fields
    ------
    claim: str
        Human-readable statement being checked.
    ok: bool
        Whether this step succeeded.
    details: Optional[str]
        Extra context (values seen, expectations, etc.).
    substeps: List[ProofStep]
        Child steps that justify this step. The parent step's `ok` is AND-ed
        with every child added via `add(...)`.
    """
    claim: str
    ok: bool
    details: Optional[str] = None
    substeps: List["ProofStep"] = field(default_factory=list)

    def add(self, step: "ProofStep"):
        """Append a substep and combine its result into this step's ok flag."""
        self.substeps.append(step)
        self.ok = self.ok and step.ok

    def render(self, indent: int = 0) -> str:
        """
        Return a multi-line string representing this node and its subtree.
        Uses ✔︎/✘ for quick visual scan.
        """
        pad = "  " * indent
        status = "✔︎" if self.ok else "✘"
        s = f"{pad}{status} {self.claim}"
        if self.details:
            s += f" — {self.details}"
        lines = [s]
        for st in self.substeps:
            lines.append(st.render(indent + 1))
        return "\n".join(lines)


class ProofEngine:
    """
    A minimal rule/check engine tailored to the specific goal:
      "Person is asserted by EXPECTED_ISSUER at TIME using a supported proof,
       and optionally that proof is itself covered by a meta-proof."

    Policy knobs:
      - SUPPORTED_TYPE, SUPPORTED_SUITES, SUPPORTED_PURPOSE
      - Whether to require meta-proof (`require_meta_proof` flag)

    Note:
      Replace the "proofValue is present" check with real cryptographic
      verification if/when you integrate a crypto library and the canonical
      signing inputs.
    """
    SUPPORTED_SUITES = {"ecdsa-rdfc-2019"}
    SUPPORTED_TYPE = "DataIntegrityProof"
    SUPPORTED_PURPOSE = "assertionMethod"

    def __init__(self, ds: Dataset):
        self.ds = ds

    # ------------------ public goal ------------------

    def prove_person_asserted_by(
        self,
        expected_issuer: str,
        at: datetime,
        require_meta_proof: bool = True,
    ) -> ProofStep:
        """
        Evaluate the goal and return a ProofStep tree explaining the result.

        Parameters
        ----------
        expected_issuer : str
            The issuer URI that must match the person-covering proof.
        at : datetime
            The evaluation instant; must be timezone-aware.
        require_meta_proof : bool
            If True, also require a meta-proof (a proof whose `about` equals
            the id of the first proof).

        Returns
        -------
        ProofStep
            Root of the proof tree with ✔︎/✘ per subgoal and human-readable notes.
        """
        if at.tzinfo is None:
            raise ValueError("`at` must be timezone-aware (UTC recommended)")

        root = ProofStep(
            claim=f"Person({self.ds.person.id}) is asserted by {expected_issuer} at {at.isoformat()}",
            ok=True,
        )

        # 1) Find a proof that covers the person data (about == "person")
        step_find = ProofStep(claim="Find proof about 'person'", ok=False)
        p1 = next(self.ds.proofs_about("person"), None)
        if p1:
            step_find.ok = True
            step_find.details = f"Using proof id={p1.id}"
        else:
            step_find.details = "No proof with about='person' found"
        root.add(step_find)
        if not step_find.ok:
            return root  # Nothing else to justify

        # 2) Check structural fields and supported suite/type/purpose
        root.add(self._check_supported(p1))

        # 3) Check issuer matches expected
        root.add(self._check_issuer(p1, expected_issuer))

        # 4) Check validity window at `at` (if provided)
        root.add(self._check_validity(p1, at))

        # 5) Check created timestamp is not in the future
        root.add(self._check_created_before(p1, at))

        # 6) (Optional) Require a meta-proof whose `about` equals p1.id
        if require_meta_proof:
            root.add(self._check_meta_proof(p1, at))

        return root

    # ------------------ helper subgoals ------------------

    def _check_supported(self, p: Proof) -> ProofStep:
        """
        Verify type/suite/purpose and presence of a proof value.
        This is where you would integrate real signature verification.
        """
        step = ProofStep(claim="Proof uses supported type/suite/purpose", ok=True)

        # type
        if p.type != self.SUPPORTED_TYPE:
            step.add(ProofStep(f"type == {self.SUPPORTED_TYPE}", ok=False, details=f"found {p.type}"))
        else:
            step.add(ProofStep(f"type == {self.SUPPORTED_TYPE}", ok=True))

        # suite
        if p.suite not in self.SUPPORTED_SUITES:
            step.add(ProofStep(f"suite in {sorted(self.SUPPORTED_SUITES)}", ok=False, details=f"found {p.suite}"))
        else:
            step.add(ProofStep(f"suite == {p.suite}", ok=True))

        # purpose
        if p.purpose != self.SUPPORTED_PURPOSE:
            step.add(ProofStep(f"purpose == {self.SUPPORTED_PURPOSE}", ok=False, details=f"found {p.purpose}"))
        else:
            step.add(ProofStep(f"purpose == {p.purpose}", ok=True))

        # Placeholder for real crypto check:
        has_value = bool(p.value)
        step.add(ProofStep("proofValue is present (crypto verification not performed)", ok=has_value))

        return step

    def _check_issuer(self, p: Proof, expected_issuer: str) -> ProofStep:
        """Ensure the person-covering proof's issuer equals the expected issuer."""
        ok = (p.issuer == expected_issuer)
        return ProofStep(
            claim="Issuer matches expected",
            ok=ok,
            details=f"found {p.issuer!r}, expected {expected_issuer!r}",
        )

    def _check_validity(self, p: Proof, at: datetime) -> ProofStep:
        """
        If a validity window is provided, ensure `at` falls inside [start, end].
        If absent, treat as unconstrained.
        """
        if p.valid is None:
            return ProofStep("No validity window provided (treat as unconstrained)", ok=True)
        ok = p.valid.contains(at)
        detail = f"window [{p.valid.start}, {p.valid.end}] contains {at}"
        return ProofStep("Within validity window", ok=ok, details=detail)

    def _check_created_before(self, p: Proof, at: datetime) -> ProofStep:
        """Ensure the proof was not created after the evaluation instant."""
        ok = p.created <= at
        return ProofStep("Proof 'created' is not in the future", ok=ok, details=f"created={p.created}, at={at}")

    def _check_meta_proof(self, p: Proof, at: datetime) -> ProofStep:
        """
        Require a meta-proof whose `about` equals `p.id`. Re-use supported checks
        and the "created not in the future" constraint. We deliberately do not
        constrain the meta-proof's issuer here, but you can add that policy.
        """
        step = ProofStep(claim=f"Meta-proof exists covering proof {p.id}", ok=False)

        # Find first meta-proof about p.id
        p2 = next(self.ds.proofs_about(p.id), None)
        if not p2:
            step.details = "No proof found with about == first proof's id"
            return step

        step.ok = True
        step.details = f"Using meta-proof id={p2.id}"

        # Structural checks for the meta-proof (same support constraints)
        step.add(self._check_supported(p2))

        # Creation time must not be in the future at evaluation instant
        step.add(self._check_created_before(p2, at))
        return step


# -----------------------------------------------------------------------------
# Example dataset (mirrors the original TriG semantics)
# -----------------------------------------------------------------------------

def example_dataset() -> Dataset:
    """
    Construct the dataset corresponding to the original example:
      - Person(Bob) with name "Bob".
      - signature1: proof about "person", with issuer and validity window.
      - signature2: meta-proof about "signature1".
    """
    person = Person(id="Bob", name="Bob")

    signature1 = Proof(
        id="signature1",
        about="person",
        type="DataIntegrityProof",
        suite="ecdsa-rdfc-2019",
        created=datetime(2021, 11, 13, 18, 19, 39, tzinfo=timezone.utc),
        verification_method="https://university.example/issuers/14#key-1",
        purpose="assertionMethod",
        value="z58DAdFfa9SkqZMVPxAQp...jQCrfFPP2oumHKtz",
        issuer="https://university.example/issuers/14",
        valid=Validity(
            start=datetime(2024, 4, 3, 0, 0, 0, tzinfo=timezone.utc),
            end=datetime(2025, 4, 3, 0, 0, 0, tzinfo=timezone.utc),
        ),
    )

    signature2 = Proof(
        id="signature2",
        about="signature1",  # signs the first proof's graph/metadata
        type="DataIntegrityProof",
        suite="ecdsa-rdfc-2019",
        created=datetime(2021, 11, 13, 18, 19, 39, tzinfo=timezone.utc),
        verification_method="https://university.example/issuers/14#key-1",
        purpose="assertionMethod",
        value="adad123efv434r5200...dqed2t44v43das",
        issuer=None,
        valid=None,
    )

    return Dataset(person=person, proofs=[signature1, signature2])


# -----------------------------------------------------------------------------
# Demo / simple tests
# -----------------------------------------------------------------------------

def run_demo():
    """
    Build the example dataset and evaluate three goals:

    A) Valid time, correct issuer, meta-proof required -> expect success.
    B) Time outside the validity window                -> validity fails.
    C) Wrong expected issuer                           -> issuer check fails.

    Output is an indented proof tree; each line shows a subgoal with ✔︎/✘ and
    optional details that explain the result.
    """
    ds = example_dataset()
    engine = ProofEngine(ds)
    issuer = "https://university.example/issuers/14"

    print("\n=== Goal A: valid time, correct issuer, require meta-proof ===")
    at = datetime(2024, 6, 1, 12, 0, tzinfo=timezone.utc)
    proof = engine.prove_person_asserted_by(issuer, at, require_meta_proof=True)
    print(proof.render())

    print("\n=== Goal B: time outside validity (should fail validity) ===")
    at2 = datetime(2025, 5, 1, 12, 0, tzinfo=timezone.utc)  # after validUntil
    proof2 = engine.prove_person_asserted_by(issuer, at2, require_meta_proof=True)
    print(proof2.render())

    print("\n=== Goal C: wrong issuer (should fail issuer check) ===")
    wrong_issuer = "https://evil.example/issuer"
    proof3 = engine.prove_person_asserted_by(wrong_issuer, at, require_meta_proof=True)
    print(proof3.render())


if __name__ == "__main__":
    run_demo()

