"""
Polygon area with "explain-and-check"
=====================================

What this file does
-------------------
• Accepts inputs written as strings like:
      "Polygon((x1, y1), (x2, y2), ..., (xN, yN)).area"
• Parses the coordinate tuples, then computes the polygon area using the
  shoelace (Gauss / Green's-theorem) formula.
• Prints a compact derivation for each case and runs a small proof harness:
    1) Orientation check: reversing vertex order flips the sign but preserves |area|.
    2) Translation invariance: shifting all points by a vector leaves the area unchanged.
    3) Independent check: anchor-fan decomposition via cross products equals the same area.

Mathematical summary
--------------------
Given an ordered simple polygon P with vertices (x_i, y_i), i = 0..n-1 (in CW or CCW order),
its oriented area is:
    A_oriented = (1/2) * Σ_{i=0}^{n-1} (x_i * y_{i+1} - x_{i+1} * y_i),
where indices wrap (x_n, y_n) := (x_0, y_0).  The unsigned area is |A_oriented|.
If the input repeats the first vertex at the end, we drop the duplicate.

Notes
-----
• Works for convex and concave simple polygons (no self-crossings), in any orientation.
• Uses only built-in Python; no imports, no eval.
• Numbers are handled as Python ints/floats; printed cleanly (integers shown without .0).
"""

# --------------------------- Helpers ---------------------------

def parse_polygon_case(case_str):
    """
    Extract list of (x, y) pairs from a string like:
      "Polygon((0, 0), (2, 0), (0, 1), (0, 0)).area"
    No eval, no regex, just a small hand-rolled parser.
    """
    s = case_str.strip()
    # Find the substring inside Polygon( ... ) before ".area"
    head = "Polygon("
    tail = ").area"
    a = s.find(head)
    b = s.rfind(tail)
    if a == -1 or b == -1 or b <= a:
        raise ValueError("Bad case string: " + case_str)
    inside = s[a + len(head): b]

    # Scan for parenthesized pairs "(x, y)"
    pts = []
    i = 0
    n = len(inside)
    while i < n:
        if inside[i] == '(':
            j = i + 1
            depth = 1
            # find matching ')'
            while j < n and depth > 0:
                if inside[j] == '(':
                    depth += 1
                elif inside[j] == ')':
                    depth -= 1
                j += 1
            if depth != 0:
                raise ValueError("Unbalanced parentheses in: " + case_str)
            pair = inside[i+1:j-1].strip()
            # split on comma
            comma = pair.find(',')
            if comma == -1:
                raise ValueError("Not a pair in: " + pair)
            x_str = pair[:comma].strip()
            y_str = pair[comma+1:].strip()
            x = float(x_str) if ('.' in x_str or 'e' in x_str or 'E' in x_str) else int(x_str)
            y = float(y_str) if ('.' in y_str or 'e' in y_str or 'E' in y_str) else int(y_str)
            pts.append((x, y))
            i = j
        else:
            i += 1

    if len(pts) < 3:
        raise ValueError("Polygon must have at least 3 vertices.")
    return pts

def drop_duplicate_closure(points):
    """If last point equals first, drop the last."""
    if points[0] == points[-1]:
        return points[:-1]
    return points

def shoelace_oriented(points):
    """
    Oriented area = 0.5 * sum (x_i*y_{i+1} - x_{i+1}*y_i),
    wrapping indices. Points must NOT repeat the first at the end.
    """
    n = len(points)
    s = 0
    for i in range(n):
        x1, y1 = points[i]
        x2, y2 = points[(i + 1) % n]
        s += x1 * y2 - x2 * y1
    return 0.5 * s  # signed

def area_anchor_fan(points):
    """
    Independent check: sum cross((p_i - p0), (p_{i+1} - p0)) / 2, i=1..n-2.
    Equal to oriented area for a simple polygon with ordered vertices
    (convex or concave). We return the unsigned area for comparison.
    """
    p0x, p0y = points[0]
    acc = 0
    for i in range(1, len(points) - 1):
        x1 = points[i][0] - p0x
        y1 = points[i][1] - p0y
        x2 = points[i + 1][0] - p0x
        y2 = points[i + 1][1] - p0y
        acc += x1 * y2 - x2 * y1
    return abs(acc) * 0.5

def translate(points, dx, dy):
    return [(p[0] + dx, p[1] + dy) for p in points]

def fmt_num(v):
    """Pretty number: show integers without .0."""
    if isinstance(v, int):
        return str(v)
    # treat float very close to int as int
    r = int(round(v))
    if abs(v - r) < 1e-12:
        return str(r)
    # general float
    s = f"{v:.12g}"
    return s

def print_shoelace_terms(points):
    """Show the Σ x_i*y_{i+1} and Σ x_{i+1}*y_i tables and their difference."""
    n = len(points)
    xy = []
    yx = []
    for i in range(n):
        x1, y1 = points[i]
        x2, y2 = points[(i + 1) % n]
        xy.append(x1 * y2)
        yx.append(x2 * y1)
    sum_xy = sum(xy)
    sum_yx = sum(yx)
    print("  Shoelace detail:")
    print("    Σ x_i*y_{i+1} =", " + ".join(fmt_num(t) for t in xy), "=", fmt_num(sum_xy))
    print("    Σ y_i*x_{i+1} =", " + ".join(fmt_num(t) for t in yx), "=", fmt_num(sum_yx))
    oriented = 0.5 * (sum_xy - sum_yx)
    print("    Oriented area  = 1/2 * (Σ x_i*y_{i+1} - Σ y_i*x_{i+1}) =", fmt_num(oriented))
    print("    Unsigned area  =", fmt_num(abs(oriented)))

# --------------------------- Driver ---------------------------

if __name__ == "__main__":
    cases = [
        "Polygon((0, 0), (2, 0), (0, 1), (0, 0)).area",
        "Polygon((3, 2), (6, 2), (7, 6), (4, 6), (5, 5), (5, 3), (3, 2)).area",
    ]

    for c in cases:
        # Parse
        raw_pts = parse_polygon_case(c)
        pts = drop_duplicate_closure(raw_pts)
        n = len(pts)

        print("=" * 72)
        print("Case:", c)
        print("  Parsed vertices (after dropping duplicate closure if present):")
        print("  ", pts)

        # Compute oriented + unsigned areas
        A_oriented = shoelace_oriented(pts)
        A = abs(A_oriented)

        # Explain (show the actual shoelace sums)
        print_shoelace_terms(pts)

        # Checks / proof harness
        print("\n  Checks:")
        # 1) Orientation reversal flips sign
        pts_rev = list(reversed(pts))
        A_oriented_rev = shoelace_oriented(pts_rev)
        print("    Orientation check:  A_oriented =", fmt_num(A_oriented),
              ",  reversed =", fmt_num(A_oriented_rev),
              " -> |areas| equal?", abs(abs(A_oriented) - abs(A_oriented_rev)) < 1e-12)

        # 2) Translation invariance
        dx, dy = 123, -77
        A_shift = abs(shoelace_oriented(translate(pts, dx, dy)))
        print("    Translation invariance (+123, -77): area =", fmt_num(A_shift),
              " -> unchanged?", abs(A - A_shift) < 1e-12)

        # 3) Independent anchor-fan decomposition
        A_fan = area_anchor_fan(pts)
        print("    Anchor-fan check: area =", fmt_num(A_fan),
              " -> matches?", abs(A - A_fan) < 1e-12)

        # Final result line matching the original style
        print("\n", c, "=", fmt_num(A))

