"""
A very small, self‑contained Python program that *mimics* the high‑level
behavior of a modern "large reasoning model" (LRM) using deterministic
rules and tool‑use. It is obviously **not** intelligent in the same sense
as GPT, but the architecture below illustrates key ideas:

1. **Chain‑of‑Thought (CoT)** — every intermediate decision is stored in a
   `ReasoningStep` so you can inspect how the system reached its answer.
2. **Tools** — the model can call a tiny set of external functions
   (`calculate`, `lookup`) to solve sub‑tasks, imitating how LRMs invoke
   calculators, search APIs, or other specialist modules.
3. **Planning Loop** — in `answer()` the model iteratively decides *what to
   do next* until it can produce a final answer or gives up.

Run it as a script and it will execute a short, **non‑interactive** demo,
printing each question, the answer, and the reasoning trace. Uncomment the
interactive block at the bottom if you still want a REPL afterwards.

Python ≥3.8, no third‑party dependencies.  License: MIT.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Callable, Dict, List, Optional, Tuple


@dataclass
class ReasoningStep:
    """A single chain‑of‑thought step."""

    thought: str
    action: Optional[str] = None  # name of the tool invoked, if any
    observation: Optional[str] = None  # result returned by the tool

    def __str__(self) -> str:  # pretty print
        lines = [f"Thought: {self.thought}"]
        if self.action:
            lines.append(f"Action:  {self.action}")
        if self.observation is not None:
            lines.append(f"Result:  {self.observation}")
        return "\n".join(lines)


class SimpleReasoningModel:
    """Toy reasoning model supporting basic math & fact lookup."""

    def __init__(self) -> None:
        # ――― minimal static knowledge base ―――
        self._facts: Dict[str, str] = {
            "capital of france": "Paris",
            "capital of belgium": "Brussels",
            "largest planet": "Jupiter",
            "speed of light": "299,792,458 m/s",
        }

        # ――― register available tools ―――
        self._tools: Dict[str, Callable[[str], str]] = {
            "calculate": self._calculate,
            "lookup": self._lookup_fact,
        }

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------
    def answer(self, prompt: str, *, max_steps: int = 6) -> Tuple[str, List[ReasoningStep]]:
        """Return *(answer, reasoning_trace)* for *prompt*."""

        trace: List[ReasoningStep] = []
        clean = prompt.strip()

        for _ in range(max_steps):
            # 1) Arithmetic?
            if self._is_arithmetic(clean):
                step = ReasoningStep("I think this is a math problem, I'll calculate it.", "calculate")
                result = self._tools["calculate"](clean)
                step.observation = result
                trace.append(step)

                trace.append(ReasoningStep("Having computed the result, I'm done."))
                return result, trace

            # 2) Exact fact lookup?
            key = self._canonical_fact_key(clean)
            if key in self._facts:
                step = ReasoningStep(f"Looks like a factual query about '{key}'. I'll look it up.", "lookup")
                result = self._tools["lookup"](key)
                step.observation = result
                trace.append(step)

                trace.append(ReasoningStep("Fact found. Returning it as the answer."))
                return result, trace

            # 3) Give up — a real LRM would now call more advanced tools.
            trace.append(ReasoningStep("I'm not sure how to proceed; I'll admit uncertainty."))
            return "I'm not confident how to answer that.", trace

        # Fallback (loop exhausted)
        trace.append(ReasoningStep("Reached step limit without an answer."))
        return "No answer in allotted steps.", trace

    # ------------------------------------------------------------------
    # Internal helpers / tools
    # ------------------------------------------------------------------
    _ARITH_RE = re.compile(r"^[\d\s()+\-*/.%]+$")

    def _is_arithmetic(self, s: str) -> bool:
        return bool(self._ARITH_RE.match(s))

    def _calculate(self, expr: str) -> str:
        """Safely evaluate a purely arithmetic expression."""
        try:
            # `eval` is dangerous; use it *only* with empty globals/locals!
            value = eval(expr, {}, {})
            return str(value)
        except Exception as exc:  # noqa: BLE001
            return f"[error: {exc}]"

    def _canonical_fact_key(self, s: str) -> str:
        return s.lower().rstrip("? .")

    def _lookup_fact(self, key: str) -> str:
        return self._facts[key]


# ----------------------------------------------------------------------
# Demo run (non‑interactive)
# ----------------------------------------------------------------------
if __name__ == "__main__":
    model = SimpleReasoningModel()

    # ――― predefined demo questions ―――
    demo_questions = [
        "3 * (4 + 5)",
        "capital of France?",
        "largest planet",
        "speed of light",
    ]

    for q in demo_questions:
        print(f"Q: {q}")
        answer, steps = model.answer(q)
        print(f"A: {answer}\n")
        print("--- Chain of thought -------------")
        for s in steps:
            print(s)
            print("----------------------------------")
        print()

    # Uncomment the block below to enter an interactive REPL after the demo.
    #
    # print("Simple Reasoning Model — interactive mode; blank line to quit\n")
    # while True:
    #     q = input("Q: ")
    #     if not q.strip():
    #         break
    #     answer, steps = model.answer(q)
    #     print(f"A: {answer}\n")
    #     print("--- Chain of thought -------------")
    #     for s in steps:
    #         print(s)
    #         print("----------------------------------")
    #     print()

