import decimal
from decimal import Decimal, getcontext

def compute_pi(digits=10000):
    """
    Calculate π to a specified number of digits using the Chudnovsky algorithm.
    
    Args:
        digits (int): Number of decimal digits to compute (default: 10000).
    
    Returns:
        str: π formatted as a string with the requested digits.
    """
    # Set precision (digits + 10 extra for intermediate calculations)
    getcontext().prec = digits + 10
    
    # Constants for the Chudnovsky formula
    A = Decimal(640320)
    A3 = A ** 3  # 640320^3
    C = Decimal(426880) * Decimal(10005).sqrt()
    
    # Initialize series sum and first term (k=0)
    total_sum = Decimal(13591409)  # Term for k=0
    u = Decimal(1)  # Multiplicative factor for k=0
    
    # Iterations needed (each adds ~14 digits)
    max_iter = digits // 14 + 2
    
    # Compute series terms for k=1 to max_iter
    for k in range(1, max_iter + 1):
        # Update the multiplicative factor 'u' using recurrence
        num = Decimal(-24) * (6 * k - 5) * (6 * k - 1) * (2 * k - 1)
        denom = (Decimal(k) ** 3) * A3
        u *= num / denom
        
        # Compute current term: u * (545140134*k + 13591409)
        term = u * (Decimal(545140134) * k + Decimal(13591409))
        total_sum += term
    
    # π = C / total_sum
    pi = C / total_sum
    
    # Format π to the requested number of digits
    return format(pi, f'.{digits}f')

# Example: Calculate π to 10000 digits and print
if __name__ == "__main__":
    digits = 10000
    pi_str = compute_pi(digits)
    print(pi_str)
