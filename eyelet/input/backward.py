"""
Python rewrite of the N3 backward-rule example
from https://www.w3.org/2000/10/swap/doc/tutorial-1.pdf (p. 17).

Rule in N3:
    { ?X :moreInterestingThan ?Y. } <= { ?X math:greaterThan ?Y. }.

Informally:
    X is :moreInterestingThan Y  ⇐  X math:greaterThan Y
    (i.e. X is more interesting if X > Y)
"""

# --- Rule layer -------------------------------------------------------------
def more_interesting_than(x: int, y: int) -> bool:
    """
    Equivalent of the N3 backward rule:
        ?X :moreInterestingThan ?Y  ⇐  ?X math:greaterThan ?Y.
    In Python that just means “return True if x > y”.
    """
    return x > y


# --- “Query” layer ----------------------------------------------------------
if __name__ == "__main__":
    x, y = 5, 3                        # mirrors the original query
    result = more_interesting_than(x, y)

    print(f"{x} :moreInterestingThan {y}  →  {result}")
    # →  5 :moreInterestingThan 3  →  True

