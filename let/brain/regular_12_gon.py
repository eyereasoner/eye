# Compute the side length of a regular 12-gon inscribed in a circle of radius 1,
# WITHOUT using imports. We also print a clear explanation and show both
# an approximate computation (via a small Taylor series for sin) and the exact
# closed form ( (sqrt(6) - sqrt(2)) / 2 ).
#
# Why the formula works (short version for the program output):
# - A side of a regular n-gon inscribed in a circle is a chord.
# - Two radii to adjacent vertices and the chord form an isosceles triangle.
# - Bisecting the apex (central) angle yields a right triangle:
#     sin(central_angle/2) = (half_chord) / radius
#   So chord length s = 2 * r * sin(central_angle / 2).
# - For n = 12 and r = 1, central_angle = 2*pi/12 = pi/6, so s = 2*sin(pi/12) = 2*sin(15°).
# - Using angle subtraction (45° - 30°), sin(15°) = (sqrt(6) - sqrt(2)) / 4,
#   hence s = (sqrt(6) - sqrt(2)) / 2.

# -----------------------------
# Minimal numeric "toolbox":
# -----------------------------

# Numerical value for pi (sufficiently precise for our needs)
pi = 3.141592653589793

def sqrt(x):
    """
    Compute square root via Newton-Raphson iteration.
    - No imports allowed, so we implement it ourselves.
    - For positive x, Newton's method converges quadratically with a decent initial guess.
    - We use a fixed number of iterations which is plenty for double precision-like accuracy
      in this context.
    """
    if x == 0:
        return 0.0
    # A simple initial guess: half the value (works fine for positive x)
    g = x / 2.0
    # Do ~20 iterations; more than enough for our small needs
    for _ in range(20):
        g = 0.5 * (g + x / g)
    return g

def sin(x):
    """
    Compute sin(x) using a Taylor series around 0:
        sin x = x - x^3/3! + x^5/5! - ...
    For our use case, x = pi/12 ≈ 0.261799..., which is small, so convergence is fast.
    NOTE:
    - We do a handful of terms for a good approximation.
    - No imports, so we hand-roll the factorial pattern by updating 'term' iteratively.
    """
    # Optional: wrap x to [-pi, pi] for better numerical behavior (not strictly needed here)
    # but harmless and illustrative.
    two_pi = 2.0 * pi
    while x > pi:
        x -= two_pi
    while x < -pi:
        x += two_pi

    term = x            # first term x
    result = 0.0
    sign = 1.0
    k = 1               # current power index for the factorial pattern
    # We'll sum enough terms to be very accurate for |x| <= pi
    # For x ~ 0.26, ~9 terms is already overkill; we do more to be safe.
    for _ in range(10):
        result += sign * term
        # Update term to next odd power / factorial piece:
        # if current term is x^k / k!, next should be x^(k+2) / (k+2)!,
        # which equals (current_term) * x^2 / ((k+1)*(k+2))
        term = term * x * x / ((k + 1) * (k + 2))
        sign = -sign
        k += 2
    return result

# -----------------------------
# Geometry-specific computation
# -----------------------------

# Radius r = 1 for the problem
r = 1.0

# n = 12 (regular dodecagon)
n = 12

# Central angle (in radians) between two adjacent vertices:
theta = 2.0 * pi / n           # = pi/6

# By the chord formula: side s = 2 * r * sin(theta/2)
half_theta = theta / 2.0       # = pi/12 (i.e., 15 degrees)
side_approx = 2.0 * r * sin(half_theta)

# Closed-form exact value via sin(15°) = (sqrt(6) - sqrt(2))/4
# Therefore s = 2 * sin(15°) = (sqrt(6) - sqrt(2)) / 2
exact_side = (sqrt(6.0) - sqrt(2.0)) / 2.0

# -----------------------------
# Print a clear explanation + results
# -----------------------------

print("WHY THE FORMULA IS TRUE (concise geometric reasoning):")
print("1) In a regular n-gon inscribed in a circle, each side is a chord.")
print("2) Two radii to adjacent vertices + the side form an isosceles triangle with central angle θ = 2π/n.")
print("3) Bisect the triangle: you get a right triangle with hypotenuse r, opposite leg s/2, angle θ/2.")
print("   Hence sin(θ/2) = (s/2)/r  =>  s = 2 r sin(θ/2).")
print()
print(f"For n = {n}, r = {r}:  θ = 2π/n = {theta:.12f} rad, so s = 2 * {r} * sin(θ/2) with θ/2 = {half_theta:.12f} rad.")
print()

print("NUMERICAL EVALUATION (Taylor series sine, no imports):")
print(f"  s ≈ {side_approx:.12f}")
print()

print("CLOSED-FORM EVALUATION (angle subtraction: sin(45°-30°)):")
print("  sin(15°) = (√6 - √2) / 4  ⇒  s = 2 sin(15°) = (√6 - √2) / 2")
print(f"  s = (sqrt(6) - sqrt(2)) / 2 ≈ {exact_side:.12f}")
print()

print("Both approaches agree (within tiny numerical error).")

