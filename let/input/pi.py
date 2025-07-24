import decimal
from decimal import Decimal, getcontext

def compute_pi(digits=10000):
    """
    Calculate π to a specified number of digits using the Chudnovsky algorithm.

    Args:
        digits (int): Number of decimal digits to compute (default: 10000)

    Returns:
        str: π formatted as a string with the requested digits
    """
    # Set precision: digits + 10 extra for intermediate calculation safety
    getcontext().prec = digits + 10

    # === Constants for Chudnovsky formula ===
    # A = 640320 (base constant)
    A = Decimal(640320)
    # A³ = 640320^3 (used in denominator)
    A3 = A ** 3
    # Precomputed constant: 426880 * sqrt(10005)
    C = Decimal(426880) * Decimal(10005).sqrt()

    # === Initialize series variables ===
    total_sum = Decimal(13591409)  # k=0 term: (545140134*0 + 13591409)
    u = Decimal(1)                 # Multiplicative factor for k=0

    # Estimate iterations needed: each adds ~14 digits
    max_iter = digits // 14 + 2    # +2 ensures convergence at desired precision

    # === Main computation loop ===
    for k in range(1, max_iter + 1):
        # Calculate numerator: (-24) * (6k-5)(2k-1)(6k-1)
        num = Decimal(-24) * (6*k - 5) * (2*k - 1) * (6*k - 1)

        # Calculate denominator: k^3 * (640320^3)
        denom = (Decimal(k) ** 3) * A3

        # Update multiplicative factor using recurrence relation
        # This avoids computing large factorials directly
        u *= num / denom

        # Compute current term: u * (545140134*k + 13591409)
        term = u * (Decimal(545140134) * k + Decimal(13591409))

        # Add term to series total
        total_sum += term

    # === Final π calculation ===
    # π = C / total_sum (Chudnovsky formula rearrangement)
    pi = C / total_sum

    # Format π to requested digits (convert to string)
    return format(pi, f'.{digits}f')

# === Example usage ===
if __name__ == "__main__":
    digits = 10000  # Number of digits to compute
    pi_str = compute_pi(digits)
    print(pi_str)
