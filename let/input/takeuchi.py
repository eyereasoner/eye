def tak(x: int, y: int, z: int) -> int:
    """
    Takeuchi’s triple-recursive benchmark function.

    tak(x, y, z) =
        tak(tak(x-1, y, z),
            tak(y-1, z, x),
            tak(z-1, x, y))   if y < x
        z                      otherwise
    """
    if y < x:
        return tak(
            tak(x - 1, y, z),
            tak(y - 1, z, x),
            tak(z - 1, x, y)
        )
    return z


if __name__ == "__main__":
    # Quick test cases
    cases = [
        (10, 5, 0),
        (5, 3, 1),
        (15, 15, 1),
        (7, 10, 12),
        (18, 13, 8)
    ]

    for x, y, z in cases:
        print(f"tak({x}, {y}, {z}) = {tak(x, y, z)}")
