#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
===============================================================================
diagonalization_demo.py
===============================================================================

A *live* Python illustration of **Turing’s diagonalization paradox** for the
Halting Problem.

What the script does at runtime
-------------------------------
1. Prints a short banner and explains the set‑up.
2. Defines a hypothetical, *perfect* halting oracle ``HALTS(program, input)``
   that simply raises ``NotImplementedError`` (to remind us it cannot exist).
3. Defines the self‑referential program ``PARADOX`` that *does the opposite* of
   whatever ``HALTS`` predicts when asked about the call ``program(program)``.
4. Calls ``PARADOX`` on itself inside a ``try/except``.  The attempt to invoke
   the impossible oracle triggers the exception, and the program prints an
   explanation of why *any* real implementation would lead to contradiction.
===============================================================================
"""

import inspect
import textwrap
from typing import Callable, Any

# ---------------------------------------------------------------------------
# 1) Hypothetical halting oracle (impossible to implement perfectly)
# ---------------------------------------------------------------------------

def HALTS(program: Callable[[Any], Any], arg: Any) -> bool:  # pragma: no cover
    """Hypothetical perfect halting decider.

    If such a function *really* existed, it would return ``True`` when
    ``program(arg)`` halts and ``False`` when it loops forever.  In reality no
    total, always‑correct implementation can exist.  We raise an exception to
    keep the demo honest.
    """
    raise NotImplementedError(
        "A perfect halting oracle cannot exist — this is the whole point!"
    )


# ---------------------------------------------------------------------------
# 2) The diagonal construction: a program that contradicts HALTS
# ---------------------------------------------------------------------------

def PARADOX(prog: Callable[[Callable[..., Any]], Any]):
    """Do the *opposite* of what HALTS predicts for (prog, prog)."""
    if HALTS(prog, prog):
        # HALTS predicted that ``prog(prog)`` halts → we now LOOP forever
        while True:
            pass  # infinite loop to contradict the prediction
    else:
        # HALTS predicted that ``prog(prog)`` loops → we HALT immediately
        return "Halted!"  # returning terminates the function


# ---------------------------------------------------------------------------
# 3) Runtime demonstration & explanation
# ---------------------------------------------------------------------------

def demo():
    banner = "DIAGONALIZATION DEMO — The Halting Problem Paradox\n" + "=" * 72
    print(banner)

    # Show the source of PARADOX for clarity (optional nicety)
    print("\nSource of PARADOX(prog):\n" + "-" * 72)
    print(textwrap.indent(inspect.getsource(PARADOX), "    "))

    print("\nAttempting to evaluate PARADOX(PARADOX) …\n")
    try:
        PARADOX(PARADOX)
    except NotImplementedError as exc:
        print("Caught expected exception from HALTS():")
        print("    ", exc)
        print("\nWhy we *must* fail:")
        explanation = """
        1. Assume HALTS were perfect.
        2. By definition, PARADOX does the opposite of HALTS' prediction on itself.
        3. Evaluate PARADOX(PARADOX):
             • If HALTS says it *halts*, PARADOX loops forever → contradiction.
             • If HALTS says it *loops*, PARADOX halts immediately → contradiction.
        4. Both cases refute the assumption that HALTS is infallible.

        Result: a perfect halting decider cannot exist.  Q.E.D. (Turing, 1936)
        """
        print(textwrap.indent(explanation.strip(), "    "))
    else:
        # This branch should never execute with the stub HALTS.
        print("Unexpectedly finished PARADOX(PARADOX) without exception —\n"
              "something is wrong with the HALTS stub!")


# ---------------------------------------------------------------------------
if __name__ == "__main__":
    demo()

