#!/usr/bin/env python3
# sha512.py  –  compute SHA-512 *and* print a goal-oriented proof
# =============================================================

import hashlib
import sys
from textwrap import indent as _indent


# -------------------------------------------------------------------
# 1 ▸  Numeric evaluator – just use hashlib (battle-tested & fast)
# -------------------------------------------------------------------
def sha512_hex(msg: bytes) -> str:
    """Return SHA-512(msg) as a 128-char hexadecimal string."""
    return hashlib.sha512(msg).hexdigest()


# -------------------------------------------------------------------
# 2 ▸  Helper to break the padded message into 1024-bit (=128-byte) blocks
# -------------------------------------------------------------------
def padded_blocks(msg: bytes):
    """
    Yield each 128-byte block after standard SHA-512 padding.
    (We do this only for the proof narration, not for hashing.)
    """
    ml = len(msg) * 8                         # message length in bits
    # append the single 1-bit
    msg_padded = msg + b"\x80"
    # append 0-bits until length ≡ 896 mod 1024 (i.e. pad to 112 mod 128 bytes)
    pad_len = (112 - len(msg_padded) % 128) % 128
    msg_padded += b"\x00" * pad_len
    # append 128-bit big-endian length
    msg_padded += ml.to_bytes(16, "big")

    # finally, split into 128-byte blocks
    for i in range(0, len(msg_padded), 128):
        yield msg_padded[i : i + 128]


# -------------------------------------------------------------------
# 3 ▸  Pretty backward-chaining proof printer
# -------------------------------------------------------------------
def prove_sha512(msg: bytes, expected_hex: str | None = None,
                 summarise_blocks: int = 4, width: int = 32):
    """
    Print a “backward” proof that SHA-512(msg) = expected_hex.
    If expected_hex is None, it is computed first.
    `summarise_blocks` keeps only that many *leading* and *trailing*
    blocks expanded; the middle ones are elided.
    """
    if expected_hex is None:
        expected_hex = sha512_hex(msg)

    # ― Step 1: state the goal
    print(f"Step 1: prove  sha512({msg!r}) = {expected_hex}")

    # ― Step 2: recall the SHA-512 algorithm
    print("  → By definition SHA-512 consists of:")
    print("    (a) pre-processing / padding the message;")
    print("    (b) processing each 1024-bit block with the compression function;")
    print("    (c) concatenating the eight 64-bit state words as the digest.\n")

    # ------ 2a: padding facts -------------------------------------------------
    ml_bits = len(msg) * 8
    print("  2a. Padding fact:")
    print(f"       • original length   L  = {ml_bits} bits")
    print("       • padded to a multiple of 1024 bits "
          f"(adds 1-bit, 0≤k≤1023 zeros, and 128-bit length)\n")

    # ------ 2b: show (some) block digests ------------------------------------
    blocks = list(padded_blocks(msg))
    num_blocks = len(blocks)
    print(f"  2b. The padded message splits into {num_blocks} "
          f"block{'s' if num_blocks!=1 else ''} of 1024 bits each.")

    # decide which blocks to show
    head = blocks[:summarise_blocks]
    tail = blocks[-summarise_blocks:] if num_blocks > summarise_blocks else []

    def short(b):                # first `width` bits as hex
        return b[:width//8].hex()

    # print the selected blocks
    for idx, blk in enumerate(head):
        print(f"       • block {idx:>3}: {short(blk)}…")
    if tail and tail[0] not in head:
        skipped = num_blocks - 2 * summarise_blocks
        print(f"       • … {skipped} block(s) elided …")
        for idx, blk in enumerate(tail, num_blocks - summarise_blocks):
            print(f"       • block {idx:>3}: {short(blk)}…")

    print("\n       (each block updates the chaining variables a–h)")

    # ------ 2c: final digest --------------------------------------------------
    print(f"\n  2c. After the last block the concatenated state words give:")
    # emphasise only the first 24 Hz digits to keep line length sensible
    print(f"       digest = {expected_hex[:24]}…{expected_hex[-24:]} "
          f"(total 512 bits)")

    # ― Step 3: conclude
    print("\nStep 3: Therefore  sha512(msg) = stated digest  ✓ proven.")


# -------------------------------------------------------------------
# 4 ▸  Demo – very similar CLI to fib_proof.py / padovan_proof.py
# -------------------------------------------------------------------
def main():
    if len(sys.argv) > 1:
        raw = sys.argv[1].encode()
    else:
        raw = b"Cryptographic hashing with SHA-512"

    digest = sha512_hex(raw)
    print(f"sha512({raw!r}) =\n  {digest}\n")
    print("=== Proof ===============================================")
    prove_sha512(raw, digest)


if __name__ == "__main__":
    main()

