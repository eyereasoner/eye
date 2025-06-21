/*
 * sdcoding.c
 *
 * A **stand‑alone** C translation of the Prolog program that implements
 * super‑dense coding in *discrete quantum computing* (DQC).
 *
 * ------------------------------------------------------------
 * DQC background  (very short!)
 * ------------------------------------------------------------
 * Replace the complex amplitudes of conventional quantum theory with the
 * finite field GF(2) = {0,1} and addition ≡ XOR.  Quantum states now become
 * **sets** of classical basis vectors instead of complex superpositions; if a
 * basis vector occurs twice it annihilates ( 1 ⊕ 1 = 0 ), yielding the famous
 * “interference” of DQC.  The original Prolog code exploits Prolog’s choice
 * points plus an odd‑count filter (assert/retract) to realise this XOR logic.
 *
 * This C program reproduces the same semantics purely with deterministic data
 * structures—no external quantum SDKs, no libraries—so it compiles and runs
 * with a one‑line `gcc` command.
 *
 * ------------------------------------------------------------
 * Build & Run
 * ------------------------------------------------------------
 *     gcc sdcoding.c -o sdcoding && ./sdcoding
 *
 * On completion it prints the (N,M) pairs that survive an *odd* number of
 * computational paths.  Correct super‑dense coding means only (0,0), (1,1),
 * (2,2) and (3,3) remain, proving Bob always recovers Alice’s two‑bit message
 * with a single qubit of communication.
 * ------------------------------------------------------------
 */

#include <stdio.h>
#include <stdbool.h>

/* ----------------------------------------------------------
 * Single‑qubit gates (as relations X ↦ Y).  Each returns the
 * set of possible outputs for the given `x` and its size `n`.
 * ---------------------------------------------------------- */
static void gate_id  (int x, int *outs, int *n) { outs[0] = x;      *n = 1; }
static void gate_g   (int x, int *outs, int *n) { outs[0] = 1 - x;  *n = 1; }
static void gate_k   (int x, int *outs, int *n) {
    if (x == 0) { outs[0] = 0;      *n = 1; }
    else        { outs[0] = 0; outs[1] = 1; *n = 2; }
}
/* kg = k ∘ g : apply g then k */
static void gate_kg  (int x, int *outs, int *n) {
    int z = 1 - x;   /* g */
    gate_k(z, outs, n);
}

/* ----------------------------------------------------------
 * Composite gate gk = g ∘ k used in Bob’s decoder predicate
 * ---------------------------------------------------------- */
static bool holds_gk(int x, int y) {
    int zs[2]; int n;
    gate_k(x, zs, &n);          /* k */
    for (int i = 0; i < n; ++i)
        if ((1 - zs[i]) == y)   /* g */
            return true;
    return false;
}
static bool holds_k (int x, int y) {
    int outs[2]; int n; gate_k(x, outs, &n);
    for (int i = 0; i < n; ++i) if (outs[i] == y) return true;
    return false;
}
static bool holds_g (int x, int y) { return (1 - x) == y; }
static bool holds_id(int x, int y) { return x == y; }

/* ----------------------------------------------------------
 * Main enumeration exactly mirroring the Prolog rules.
 * present[N][M] toggles every time the pair (N,M) is produced,
 * thereby realising addition mod‑2 (⊕) over the path counts.
 * ---------------------------------------------------------- */
int main(void) {
    bool present[4][4] = {false};          /* toggling parity table */

    /* Shared Bell pair |R⟩ = {|00⟩, |11⟩} */
    int r_x[2] = {0, 1};
    int r_y[2] = {0, 1};

    for (int n = 0; n < 4; ++n) {          /* Alice’s 2‑bit message N */
        for (int idx = 0; idx < 2; ++idx) {/* iterate over |R⟩ branches */
            int X = r_x[idx];
            int Y = r_y[idx];

            /* ---------- Alice encodes her bit‑pair ---------- */
            int B_out[2]; int B_n = 0;
            switch (n) {
                case 0: gate_id (X, B_out, &B_n); break;
                case 1: gate_g  (X, B_out, &B_n); break;
                case 2: gate_k  (X, B_out, &B_n); break;
                case 3: gate_kg (X, B_out, &B_n); break;
            }

            /* ---------- Bob decodes ---------- */
            for (int b_i = 0; b_i < B_n; ++b_i) {
                int B = B_out[b_i];

                /* Test which operator maps (B,Y).  Each true predicate
                 * yields a candidate M; order follows the Prolog file. */
                if (holds_gk(B, Y)) {
                    present[n][0] = !present[n][0];
                }
                if (holds_k(B, Y)) {
                    present[n][1] = !present[n][1];
                }
                if (holds_g(B, Y)) {
                    present[n][2] = !present[n][2];
                }
                if (holds_id(B, Y)) {
                    present[n][3] = !present[n][3];
                }
            }
        }
    }

    /* ------------------------------------------------------
     * Report pairs that survived an *odd* number of paths.
     * Expected output: (0,0) (1,1) (2,2) (3,3)
     * ------------------------------------------------------ */
    printf("Pairs (N,M) appearing an odd number of times:\n");
    for (int N = 0; N < 4; ++N)
        for (int M = 0; M < 4; ++M)
            if (present[N][M])
                printf("  N = %d,  M = %d\n", N, M);

    return 0;
}

