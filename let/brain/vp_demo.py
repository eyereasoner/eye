#!/usr/bin/env python3
"""
vp_demo.py  –  Goal‑directed authorisation from a Verifiable Presentation
============================================================================

This single file shows **the entire life‑cycle** of a request that is proven
or denied **purely from data supplied in a W3C Verifiable Presentation** (VP).

The code is split into four logical layers – each heavily commented so you can
follow *where* each part of the VC/VP spec is reflected in the Python.

──────────────────────────────────────────────────────────────────────────────
1.  DATA MODEL (≈ spec §2)
──────────────────────────────────────────────────────────────────────────────
    •  ``VerifiableCredential``   –  a self‑contained, *tamper‑evident* claim
       such as  "CA says *Alice can read file1*".
    •  ``VerifiablePresentation`` –  an *envelope* that bundles one or more
       VCs and is itself signed by the *holder* (e.g. Alice).  Cryptographic
       verification of the VP proves **origin** and **integrity** of the
       bundle before any business logic is evaluated.

2.  CRYPTOGRAPHIC VERIFICATION (≈ spec §4)
──────────────────────────────────────────────────────────────────────────────
    The real VC spec prescribes JSON‑LD Data Integrity proofs or JWTs.  For a
    zero‑dependency demo we use ordinary HMAC‑SHA‑256: every principal has a
    secret key, the signature is the HMAC over a deterministic JSON encoding
    of the credential/presentation *without* its proof.

3.  BUSINESS LOGIC / AUTHORISATION ENGINE
──────────────────────────────────────────────────────────────────────────────
    Once *both* signatures (VP + embedded VCs) are valid, the VCs are fed into
    a goal‑directed backward‑chaining prover:

        "Does *holder* have permission *X*?"
            └── search for a Permission VC that grants it
                └── recursively prove each issuer is trusted for *X*
                    via Delegation VCs, terminating in a configured root.

4.  END‑TO‑END DEMO (run the file)
──────────────────────────────────────────────────────────────────────────────
    •  Root  delegates  "read:file1"  to  CA
    •  CA    grants     "read:file1"  to  Alice
    •  Alice wraps the two VCs in a VP and sends it to the verifier.
    •  The verifier prints the decision (True/False) **and the exact chain of
       credentials** that justify it.

Many real‑world concerns (key‑distribution, revocation, timestamps, JSON‑LD
contexts, DID Resolution…) are stripped away so the *control‑flow* is crystal
clear and hackable.
"""

# ────────────────────────────────────────────────────────────────────────────
# 0.  Imports – only the standard library is needed
# ────────────────────────────────────────────────────────────────────────────
from __future__ import annotations
import json
import hmac
import hashlib
from dataclasses import dataclass, asdict
from typing import List, Dict, Tuple, Set

# ────────────────────────────────────────────────────────────────────────────
# 1.  DATA MODEL – minimal, but mirrors VC Data Model terminology
# ────────────────────────────────────────────────────────────────────────────

@dataclass
class Proof:
    """A *very* slimmed‑down Proof structure (spec §4)."""

    type: str               # "HMAC" in this demo
    creator: str            # identifier/key owner (e.g. "Root")
    signatureValue: str     # hex‑encoded HMAC digest


@dataclass
class VerifiableCredential:
    """A single, signed claim that will survive copy‑paste and travel."""

    context: str            # normally an @context URL – kept for realism
    type: str               # "DelegationCredential" | "PermissionCredential"
    issuer: str             # who signed / is attesting the claim
    credentialSubject: Dict # holds at least "id" (subject) + "action"
    proof: Proof | None     # filled by sign_vc()


@dataclass
class VerifiablePresentation:
    """An envelope that a *holder* signs (proving they controlled the VCs)."""

    context: str
    type: str               # "VerifiablePresentation"
    holder: str             # normally a DID of the presenter (e.g. "Alice")
    verifiableCredential: List[VerifiableCredential]
    proof: Proof | None     # filled by sign_vp()

# ────────────────────────────────────────────────────────────────────────────
# 2.  CRYPTOGRAPHIC HELPERS – sign / verify using HMAC‑SHA‑256
# ────────────────────────────────────────────────────────────────────────────

# NOTE:  In production you would *never* use shared‑secret HMACs between all
# parties.  This is deliberately primitive so you don't need external libs.

def _canonical(obj) -> bytes:
    """Deterministic JSON encoding (keys sorted, no whitespace)."""
    return json.dumps(obj, sort_keys=True, separators=(",", ":")).encode()


def _sign_bytes(message: bytes, secret: bytes) -> str:
    """Return hex‑encoded HMAC‑SHA‑256(message, secret)."""
    return hmac.new(secret, message, hashlib.sha256).hexdigest()


# ── VC helpers ────────────────────────────────────────────────────────────

def sign_vc(vc: VerifiableCredential, secret: bytes) -> None:
    """Attach a proof to *vc* in‑place."""
    payload = asdict(vc)
    payload.pop("proof", None)           # exclude existing proof (if any)
    vc.proof = Proof(
        type="HMAC",
        creator=vc.issuer,
        signatureValue=_sign_bytes(_canonical(payload), secret),
    )


def verify_vc(vc: VerifiableCredential, keys: Dict[str, bytes]) -> bool:
    """Return True iff the VC's proof validates with *issuer*'s secret."""
    if vc.proof is None:
        return False
    key = keys.get(vc.proof.creator)
    if key is None:
        return False
    payload = asdict(vc)
    proof_dict = payload.pop("proof")     # remove proof field for hashing
    expected = _sign_bytes(_canonical(payload), key)
    return hmac.compare_digest(expected, proof_dict["signatureValue"])


# ── VP helpers ────────────────────────────────────────────────────────────

def sign_vp(vp: VerifiablePresentation, secret: bytes) -> None:
    """Attach a proof to *vp* in‑place."""
    payload = asdict(vp)
    payload.pop("proof", None)
    vp.proof = Proof(
        type="HMAC",
        creator=vp.holder,
        signatureValue=_sign_bytes(_canonical(payload), secret),
    )


def verify_vp(vp: VerifiablePresentation, keys: Dict[str, bytes]) -> bool:
    """Return True iff the VP's proof validates with *holder*'s secret."""
    if vp.proof is None:
        return False
    key = keys.get(vp.proof.creator)
    if key is None:
        return False
    payload = asdict(vp)
    proof_dict = payload.pop("proof")
    expected = _sign_bytes(_canonical(payload), key)
    return hmac.compare_digest(expected, proof_dict["signatureValue"])


# ────────────────────────────────────────────────────────────────────────────
# 3.  GOAL‑DIRECTED PROVER – business logic after crypto succeeds
# ────────────────────────────────────────────────────────────────────────────


def prove_permission_from_vp(
    vp: VerifiablePresentation,
    action: str,
    keys: Dict[str, bytes],
    trusted_roots: Set[str],
) -> Tuple[bool, List[VerifiableCredential]]:
    """High‑level API used by the verifier.

    Parameters
    ----------
    vp             : The received Verifiable Presentation.
    action         : Action the holder wants to perform (e.g. "read:file1").
    keys           : Mapping *principal → secret‑key* for signature checks.
    trusted_roots  : Principals you accept as ultimate authorities.

    Returns (bool, proof_chain)
    ---------------------------
    *bool* – True iff permission is proven.
    *proof_chain* – list of VCs (root→leaf) that justify the decision.
    """

    # 1️⃣  Cryptographic gate – bail immediately if the VP signature fails.
    if not verify_vp(vp, keys):
        return False, []

    # 2️⃣  Keep only the VCs whose own signatures validate.
    valid_creds: List[VerifiableCredential] = [
        vc for vc in vp.verifiableCredential if verify_vc(vc, keys)
    ]

    # 3️⃣  Classic backward‑chaining proof search.
    visited: Set[str] = set()   # avoids infinite loops in delegation graphs
    chain: List[VerifiableCredential] = []

    def trusted(principal: str) -> bool:
        """Meta‑goal: `principal` is trusted on *action*."""
        if principal in trusted_roots:
            return True
        if principal in visited:
            return False
        visited.add(principal)

        # Look for a Delegation VC of the form  issuer ⇒ principal.
        for cred in valid_creds:
            subj = cred.credentialSubject
            if (
                cred.type == "DelegationCredential"
                and subj["id"] == principal
                and subj["action"] == action
            ):
                if trusted(cred.issuer):
                    chain.append(cred)
                    return True
        return False

    # MAIN goal – a Permission VC for (holder, action) whose issuer is trusted.
    for cred in valid_creds:
        subj = cred.credentialSubject
        if (
            cred.type == "PermissionCredential"
            and subj["id"] == vp.holder
            and subj["action"] == action
            and trusted(cred.issuer)
        ):
            chain.append(cred)
            chain.reverse()      # root → leaf for nicer reading
            return True, chain

    return False, []


# ────────────────────────────────────────────────────────────────────────────
# 4.  END‑TO‑END DEMO – build keys, VCs, VP and ask the prover
# ────────────────────────────────────────────────────────────────────────────

def _demo() -> None:
    """Run `python vp_demo.py` to see the proof succeed."""

    # 4.1  In‑memory key store – REAL systems would use DIDs + KIDs.
    keys: Dict[str, bytes] = {
        "Root":  b"root-secret",
        "CA":    b"ca-secret",
        "Alice": b"alice-secret",
    }

    # 4.2  Build VCs (signed immediately after creation) ────────────────

    # Root  ⇒  CA   (delegation of *read:file1*)
    vc_deleg = VerifiableCredential(
        context="https://www.w3.org/2018/credentials/v1",
        type="DelegationCredential",
        issuer="Root",
        credentialSubject={
            "id": "CA",
            "action": "read:file1",
        },
        proof=None,
    )
    sign_vc(vc_deleg, keys["Root"])

    # CA  ⇒  Alice (permission to actually perform *read:file1*)
    vc_perm = VerifiableCredential(
        context="https://www.w3.org/2018/credentials/v1",
        type="PermissionCredential",
        issuer="CA",
        credentialSubject={
            "id": "Alice",
            "action": "read:file1",
        },
        proof=None,
    )
    sign_vc(vc_perm, keys["CA"])

    # 4.3  Alice collects both VCs into a VP and signs it ────────────────

    vp = VerifiablePresentation(
        context="https://www.w3.org/2018/credentials/v1",
        type="VerifiablePresentation",
        holder="Alice",
        verifiableCredential=[vc_deleg, vc_perm],
        proof=None,
    )
    sign_vp(vp, keys["Alice"])

    # 4.4  Verifier asks: "May Alice read file1?" ───────────────────────

    ok, chain = prove_permission_from_vp(
        vp=vp,
        action="read:file1",
        keys=keys,
        trusted_roots={"Root"},
    )

    # Display the decision and its justification chain.
    print("VP verified:", ok)
    if ok:
        print("Proof chain (root → leaf):")
        for c in chain:
            subj = c.credentialSubject["id"]
            print(f"  {c.issuer} ⇒ {subj}   ({c.type})")


# ────────────────────────────────────────────────────────────────────────────
# Entrypoint – allow `python vp_demo.py` execution
# ────────────────────────────────────────────────────────────────────────────

if __name__ == "__main__":
    _demo()

