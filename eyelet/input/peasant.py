def peasant_multiply(a: int, b: int) -> int:
    """
    Multiply two integers using the ancient Egyptian (peasant) multiplication method.

    Args:
        a: The first integer (multiplicand).
        b: The second integer (multiplier).

    Returns:
        The product of a and b.

    Example:
        >>> peasant_multiply(18, 23)
        414
    """
    # Handle sign
    sign = -1 if (a < 0) ^ (b < 0) else 1
    a, b = abs(a), abs(b)
    result = 0
    while b > 0:
        if b & 1:
            result += a
        a <<= 1  # double a
        b >>= 1  # halve b
    return sign * result


def peasant_power(base: int, exponent: int) -> int:
    """
    Compute integer exponentiation using the peasant (binary exponentiation) method,
    leveraging peasant_multiply for multiplication.

    Args:
        base: The base integer.
        exponent: The exponent (must be a non-negative integer).

    Returns:
        base raised to the power of exponent.

    Example:
        >>> peasant_power(3, 5)
        243
    """
    if exponent < 0:
        raise ValueError("Exponent must be non-negative")
    result = 1
    b = base
    e = exponent
    while e > 0:
        if e & 1:
            result = peasant_multiply(result, b)
        b = peasant_multiply(b, b)
        e >>= 1
    return result

# If run as a script, demonstrate usage
if __name__ == "__main__":
    print("Peasant Multiply 18 x 23 =", peasant_multiply(18, 23))
    print("Peasant Power 3^5 =", peasant_power(3, 5))

