# ==========================================================
# Bell’s Spaceship Paradox — EXPLAIN & CHECK (standalone, no imports)
# ==========================================================
# Goal:
#   Give a clear “reason why” the connecting string MUST BREAK in Bell’s
#   spaceship setup, and back it up with a small numeric check.
#
# Classical setup (c = 1 units):
#   • Two identical ships A (rear) and B (front) start at rest in a lab frame S.
#   • Their initial positions are x_A(0) = 0 and x_B(0) = L (so lab separation L).
#   • Engines fire in lockstep per the lab clock (identical thrust program in S),
#     so the lab-frame separation stays EXACTLY L at all lab times t.
#   • A light, inelastic string of rest length L connects A to B at t = 0.
#
# What people sometimes (mis)state:
#   You sometimes see “L′(t) = γ(t)·L” quoted as the separation in the instantaneous
#   rest frame S′. That equality holds for the *inertial* case (constant v).
#   With ACCELERATION, the B-event that is simultaneous with A(t_A) in S′ occurs at a
#   *different lab time* t_B ≠ t_A, and the comoving separation is actually STRICTLY
#   LARGER than γ(t_A)·L once v_A>0.
#
# What IS true (and sufficient to break the string):
#   In A’s instantaneous inertial frame S′ at lab time t_A (speed v_A>0),
#   the simultaneity condition t′_B = t′_A implies
#       Δt  =  v_A · Δx,                      [Lorentz simultaneity]
#   where Δx = x_B(t_B) − x_A(t_A) > 0 and Δt = t_B − t_A.
#   Therefore the measured separation in S′ is
#       L′  =  γ_A(Δx − v_A Δt) = γ_A(1 − v_A^2)Δx = Δx / γ_A.
#   For identical proper accelerations, convexity of x_A(t) gives
#       x_A(t_B) ≥ x_A(t_A) + v_A (t_B − t_A),
#   hence  Δx = x_B(t_B) − x_A(t_A) = L + x_A(t_B) − x_A(t_A) ≥ L + v_A Δt.
#   Using Δt = v_A Δx gives
#       (1 − v_A^2) Δx ≥ L   ⇒   Δx ≥ γ_A^2 L   ⇒   L′ = Δx/γ_A ≥ γ_A L,
#   with strict “>” once v_A>0. Since the string’s proper length is fixed at L,
#   it cannot reach γ_A L or more; tension grows and the string must break.  □
#
# What this program does:
#   1) Prints the “reason why” in words (as above).
#   2) Uses standard constant PROPER acceleration worldlines (nice closed forms)
#      to compute:
#        • γ(t), v(t), x_A(t), x_B(t) with lab separation fixed at L,
#        • the comoving separation L′(t_A) by explicitly solving the
#          simultaneity condition in S′ via a Lorentz transform,
#        • and it confirms numerically that L′(t_A) ≥ γ(t_A)·L (strict for v>0),
#          and that indeed t_B > t_A (later lab time) for the simultaneous event.
#   3) A harness checks monotonicity of γ, constancy of lab separation, and
#      the inequalities above at several times.
#
# Notes on numerics:
#   • No imports used; we hand-roll sqrt and a hybrid Newton–bisection solver.
#   • Iterations/tolerances are chosen to be robust with plain Python floats.
# ==========================================================


# -----------------------------
# Minimal numeric helpers (no imports)
# -----------------------------

def sqrt(x):
    """Newton–Raphson sqrt with many iterations for high accuracy; assumes x ≥ 0."""
    if x == 0.0:
        return 0.0
    g = x if x > 1.0 else 1.0
    for _ in range(50):  # generous for robustness
        g = 0.5*(g + x/g)
    return g

def absf(x):
    """Absolute value for floats (no imports)."""
    return -x if x < 0.0 else x


# -----------------------------
# Kinematics for constant proper acceleration α (c = 1)
# -----------------------------
# Starting from rest at t = 0, the worldline at LAB coordinate time t is:
#   γ(t)   = sqrt(1 + (α t)^2)
#   v(t)   = α t / γ(t)
#   x_A(t) = (γ(t) − 1)/α
#   x_B(t) = L + x_A(t)
# These maintain lab separation L at ALL lab times by construction.

def gamma_of_t(alpha, t):
    return sqrt(1.0 + (alpha*t)*(alpha*t))

def v_of_t(alpha, t):
    g = gamma_of_t(alpha, t)
    return (alpha*t) / g

def xA_of_t(alpha, t):
    return (gamma_of_t(alpha, t) - 1.0)/alpha

def xB_of_t(alpha, t, L):
    return L + xA_of_t(alpha, t)


# -----------------------------
# Lorentz transform S → S′ with speed v (c = 1)
# x′ = γ (x − v t),   t′ = γ (t − v x)
# -----------------------------

def lorentz_xprime(x, t, v):
    g = 1.0 / sqrt(1.0 - v*v)
    return g*(x - v*t)

def lorentz_tprime(x, t, v):
    g = 1.0 / sqrt(1.0 - v*v)
    return g*(t - v*x)


# -----------------------------
# Simultaneity solver (hybrid Newton + bisection)
# Given A’s lab event (tA, xA(tA)) and its instantaneous speed vA,
# find tB such that A(tA) and B(tB) are simultaneous in S′ (A’s frame):
#   t′_B  =  t′_A.
# Then compute L′ = x′_B − x′_A in that frame.
# -----------------------------

def find_tB_simultaneous_in_Sprime(alpha, L, tA):
    """
    Returns (tB, Lprime), where tB is the LAB time on B's worldline that is
    simultaneous with A(tA) in A’s instantaneous rest frame, and
    Lprime = x′_B − x′_A is the comoving separation in that frame.
    """
    vA = v_of_t(alpha, tA)
    gA = 1.0 / sqrt(1.0 - vA*vA)  # gamma(vA)

    xA = xA_of_t(alpha, tA)
    # Simultaneity condition t′_B = t′_A boils down to:
    #   (tB − vA xB(tB))  =  (tA − vA xA(tA))  =: const
    const = tA - vA * xA

    def g(tB):
        xB = xB_of_t(alpha, tB, L)
        return (tB - vA * xB) - const

    def gp(tB):
        # derivative wrt tB: 1 − vA * (dxB/dtB) = 1 − vA * v(tB)
        return 1.0 - vA * v_of_t(alpha, tB)

    # Bracket a root: at tB = tA, g = −vA L < 0 (for vA > 0); increase tB until g > 0
    lo = tA
    hi = tA + 1.0
    while g(hi) <= 0.0:
        hi += 1.0

    # Hybrid Newton–bisection
    tB = 0.5 * (lo + hi)
    for _ in range(60):
        gt = g(tB)
        if gt == 0.0:
            break
        d = gp(tB)
        took_newton = False
        if d != 0.0:
            cand = tB - gt/d
            if lo < cand < hi:
                tB = cand
                took_newton = True
        if not took_newton:
            mid = 0.5*(lo + hi)
            if g(mid) > 0.0:
                hi = mid
            else:
                lo = mid
            tB = 0.5*(lo + hi)
        # tighten bracket according to sign at new tB
        if g(tB) > 0.0:
            hi = tB
        else:
            lo = tB

    # Comoving separation via Lorentz x′ with speed vA
    xB = xB_of_t(alpha, tB, L)
    xA_p = gA * (xA - vA * tA)
    xB_p = gA * (xB - vA * tB)
    return tB, (xB_p - xA_p)


# -----------------------------
# EXPLAIN — the “reason why” (printed)
# -----------------------------

print("============================================")
print("Bell’s Spaceship Paradox — explain and check")
print("============================================\n")

print("Setup:")
print("  • Two ships A (rear) and B (front) start at rest, separated by lab distance L.")
print("  • Engines are synchronized in the lab; lab separation stays exactly L.")
print("  • A thin, inelastic string of rest length L connects A to B.\n")

print("Reason why the string breaks:")
print("  • Consider A’s instantaneous rest frame S′ at some lab time t_A (speed v_A>0).")
print("  • In S′, simultaneity slices tilt; the B-event simultaneous with A(t_A)")
print("    occurs at a later lab time t_B > t_A. Using simultaneity (Δt = v_A Δx)")
print("    and the convexity of accelerating worldlines, one shows L′(t_A) ≥ γ(t_A)·L,")
print("    with strict '>' once v_A>0.")
print("  • Since the string’s rest length is fixed at L, it cannot reach γ(t_A)L or more;")
print("    tension grows and the string must break.  □\n")


# -----------------------------
# Parameters & numeric demonstration
# -----------------------------

alpha = 0.002   # proper acceleration (c = 1 units)
L     = 100.0   # initial lab separation = initial string rest length

print(f"Parameters:  α={alpha},  L={L}\n")

times = [0.0, 2000.0, 5000.0, 10000.0, 20000.0]

print("Table: lab time t, lab speed v, gamma γ, lower bound γ·L, and computed L′ in S′ (A’s frame)")
print("Note: With acceleration we expect   L′(t)  ≥  γ(t)·L   (strict for v>0).\n")
print("    t        v(t)         γ(t)        γ·L (lower bound)       L′(t) (computed)       gap L′−γL")
for t in times:
    vA = v_of_t(alpha, t)
    gA = gamma_of_t(alpha, t)
    gL = gA * L
    if t == 0.0:
        # Trivial simultaneous event is the same time; equality at rest
        tB, Lprime = t, L
    else:
        tB, Lprime = find_tB_simultaneous_in_Sprime(alpha, L, t)
    gap = Lprime - gL
    print(f"{t:8.1f}  {vA:10.6f}  {gA:10.6f}  {gL:18.9f}  {Lprime:20.9f}  {gap:14.9f}")
print()


# -----------------------------
# CHECK — harness (silent on success, prints summary)
# -----------------------------

def harness():
    # 1) γ(t) nondecreasing; >1 for t>0
    g_prev = gamma_of_t(alpha, 0.0)
    for k in range(1, 21):
        t = k * 1500.0
        g = gamma_of_t(alpha, t)
        assert g >= g_prev - 1e-14, "γ(t) should be nondecreasing"
        if t > 0.0:
            assert g > 1.0, "γ(t)>1 for t>0"
        g_prev = g

    # 2) Lab separation stays exactly L
    for t in (0.0, 1000.0, 5000.0, 12000.0, 24000.0):
        sep = xB_of_t(alpha, t, L) - xA_of_t(alpha, t)
        assert absf(sep - L) <= 1e-12, "Lab separation must remain L"

    # 3) L′(t_A) ≥ γ(t_A)·L and t_B(simul) > t_A for v_A>0
    for tA in (2000.0, 8000.0, 16000.0, 32000.0):
        vA = v_of_t(alpha, tA)
        assert vA > 0.0
        tB, Lp = find_tB_simultaneous_in_Sprime(alpha, L, tA)
        gL = gamma_of_t(alpha, tA) * L
        # Allow a tiny epsilon slack for arithmetic
        assert Lp + 1e-10 >= gL, f"L′(t) < γ(t)L at t={tA} (Lp={Lp}, γL={gL})"
        assert tB > tA, f"Simultaneous B-event should be at later lab time (tB>{tA}), got tB={tB}"

    return True

if harness():
    print("Harness:")
    print("  • γ(t) is nondecreasing and > 1 for t > 0.")
    print("  • Lab-frame separation is exactly L at all times (by construction).")
    print("  • Computed comoving separation obeys  L′(t) ≥ γ(t)·L  (strict once v>0), and the")
    print("    simultaneous B-event indeed occurs at later lab time t_B > t_A. ✓")

