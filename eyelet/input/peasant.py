import sys

if hasattr(sys, 'set_int_max_str_digits'):
    # adjust this limit as needed
    sys.set_int_max_str_digits(10000000)

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
    print("3 x 0 =", peasant_multiply(3, 0))
    print("5 x 6 =", peasant_multiply(5, 6))
    print("238 x 13 =", peasant_multiply(238, 13))
    print("8367238 x 27133 =", peasant_multiply(8367238, 27133))
    print("62713345408367238 x 40836723862713345 =", peasant_multiply(62713345408367238, 40836723862713345))
    print("4083672386271334562713345408367238 x 4083672386271334562713345408367238 =", peasant_multiply(4083672386271334562713345408367238, 4083672386271334562713345408367238))

    print("3^0 =", peasant_power(3, 0))
    print("5^6 =", peasant_power(5, 6))
    print("238^13 =", peasant_power(238, 13))
    print("8367238^2713 =", peasant_power(8367238, 2713))

