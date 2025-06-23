import sympy  # pip install sympy

def perfect_numbers(count):
    found = 0
    for p in sympy.primerange(2, 200_000):    # search prime exponents
        mersenne = 2**p - 1
        if sympy.isprime(mersenne):
            # p is prime and 2**p-1 is prime ⇒ perfect
            yield 2**(p-1) * mersenne
            found += 1
            if found >= count:
                break

if __name__ == '__main__':
    for idx, perf in enumerate(perfect_numbers(15), start=1):
        print(f"{idx}: {perf}")

