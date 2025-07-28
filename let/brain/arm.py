"""
Advanced Reasoning Model (ARM)
=============================

This *didactic* script sketches how a **more capable** “Large Reasoning
Model” could be wired together in pure Python *without* any ML weights.
It layers a handful of architectural ideas that modern agents use:

* **Tools as Plug‑ins** – each with name/description/run interface.
* **Vector‑ish Memory** – naive token‑overlap scoring stands in for real
  embeddings so the agent can recall past answers.
* **Dynamic Planning Loop** – the agent chooses and chains tools until it
  feels confident (or hits a step budget).
* **Self‑Reflection** – a primitive confidence check triggers retries
  using extra tools when the first plan looks weak.
* **Transparent Chain‑of‑Thought** – every action is logged so you can
  watch the reasoning unfold.

Run the file to see a *non‑interactive* demo on startup.  To play with it
manually, flip `INTERACTIVE = True` below.

Python ≥3.8, no third‑party deps.  License: MIT.
"""

from __future__ import annotations

import math
import random
import re
from dataclasses import dataclass, field
from typing import Callable, Dict, List, Optional, Tuple

# ---------------------------------------------------------------------------
# Utility classes
# ---------------------------------------------------------------------------


@dataclass
class ReasoningStep:
    """A single line in the chain‑of‑thought trace."""

    thought: str
    tool: Optional[str] = None  # tool name if one is called
    tool_input: Optional[str] = None
    observation: Optional[str] = None

    def __str__(self) -> str:
        parts = [f"Thought: {self.thought}"]
        if self.tool:
            parts.append(f'Action:  {self.tool}("{self.tool_input}")')
        if self.observation is not None:
            parts.append(f"Result:  {self.observation}")
        return "\n".join(parts)


class Tool:
    """Abstract base for tools."""

    name: str = "tool"
    description: str = "(no description)"

    def run(self, query: str) -> str:  # pragma: no cover
        raise NotImplementedError


# ---------------------------------------------------------------------------
# Built‑in tools
# ---------------------------------------------------------------------------


class CalculatorTool(Tool):
    name = "calculator"
    description = "Evaluate arithmetic expressions (support + - * / % **)."

    _SAFE_CHARS = re.compile(r"^[\d\s()+\-*/.%**]+$")

    def run(self, expr: str) -> str:
        if not self._SAFE_CHARS.match(expr):
            return "[calculator refused: unsafe expression]"
        try:
            result = eval(expr, {"__builtins__": {}}, {})  # guard eval
            return str(result)
        except Exception as exc:  # noqa: BLE001
            return f"[calculator error: {exc}]"


class FactLookupTool(Tool):
    name = "fact_lookup"
    description = "Return simple factual answers from an internal KB."

    _KB: Dict[str, str] = {
        "capital of france": "Paris",
        "capital of belgium": "Brussels",
        "largest planet": "Jupiter",
        "speed of light": "299,792,458 m/s",
        "author of hamlet": "William Shakespeare",
    }

    def run(self, query: str) -> str:
        key = query.lower().rstrip("? .")
        return self._KB.get(key, "[fact not found]")


class WebSearchTool(Tool):
    name = "web_search"
    description = "Return top mocked web snippets for open‑ended queries."

    _MOCK_SNIPPETS = {
        "ai definition": "Artificial intelligence (AI) refers to ...",
        "openai": "OpenAI is an AI research and deployment company ...",
        "mount everest height": "Mount Everest is 8,848.86 m above sea level ...",
    }

    def run(self, query: str) -> str:
        key = query.lower().rstrip("? .")
        return self._MOCK_SNIPPETS.get(key, "[no search results]")


# ---------------------------------------------------------------------------
# Memory store (toy vector retrieval)
# ---------------------------------------------------------------------------


@dataclass
class MemoryItem:
    question: str
    answer: str
    tokens: set[str] = field(init=False)

    def __post_init__(self) -> None:
        self.tokens = set(re.findall(r"\w+", self.question.lower()))


class MemoryStore:
    """Stores past Q/A pairs and retrieves by naive token overlap."""

    def __init__(self) -> None:
        self._items: List[MemoryItem] = []

    def add(self, q: str, a: str) -> None:
        self._items.append(MemoryItem(q, a))

    def recall(self, q: str, top_k: int = 1) -> List[Tuple[str, str]]:
        q_tokens = set(re.findall(r"\w+", q.lower()))
        scored = [
            (len(q_tokens & item.tokens), item.answer)
            for item in self._items
        ]
        scored.sort(key=lambda t: t[0], reverse=True)
        return [(score, ans) for score, ans in scored[:top_k] if score > 0]


# ---------------------------------------------------------------------------
# The Agent
# ---------------------------------------------------------------------------


class AdvancedReasoningModel:
    """A *small‑but‑lawful* reasoning agent with planning, memory, tools."""

    def __init__(self) -> None:
        self.memory = MemoryStore()
        self.tools: Dict[str, Tool] = {t.name: t for t in [
            CalculatorTool(),
            FactLookupTool(),
            WebSearchTool(),
        ]}

    # ---------------------- public API ----------------------------------

    def answer(self, question: str, *, max_steps: int = 8) -> Tuple[str, List[ReasoningStep]]:
        trace: List[ReasoningStep] = []
        q_clean = question.strip()

        # 1) Check memory first
        mem_hits = self.memory.recall(q_clean)
        if mem_hits:
            score, ans = mem_hits[0]
            if score >= 2:  # fairly confident
                trace.append(ReasoningStep("I recall answering this before; I'll reuse it."))
                return ans, trace

        # 2) Planning loop
        working: Optional[str] = q_clean
        answer: Optional[str] = None

        for step_idx in range(1, max_steps + 1):
            # Heuristic tool selection
            tool, tool_in = self._choose_tool(working)
            if tool is None:
                trace.append(ReasoningStep("No suitable tool – giving up."))
                answer = "I'm not confident how to answer that."
                break

            thought = f"Step {step_idx}: I should use {tool.name} on '{tool_in}'."
            result = tool.run(tool_in)
            trace.append(ReasoningStep(thought, tool.name, tool_in, result))

            # Check result quality
            if not result.startswith("["):  # we treat bracket‑wrapped strings as errors
                answer = result
                break
            else:
                # Failed – try web search fallback once
                if tool.name != "web_search":
                    working = q_clean  # re‑plan using original query
                else:
                    answer = result  # even search failed
                    break

        if answer is None:
            answer = "No answer in allotted steps."

        # 3) Store in memory
        self.memory.add(q_clean, answer)
        return answer, trace

    # ---------------------- helpers -------------------------------------

    _ARITH_RE = re.compile(r"^[\d\s()+\-*/.%**]+$")

    def _choose_tool(self, query: str) -> Tuple[Optional[Tool], str]:
        """Very crude intent recognition → tool selection."""

        if self._ARITH_RE.match(query):
            return self.tools["calculator"], query

        # Simple fact patterns
        if any(k in query.lower() for k in ["capital", "largest", "speed", "author"]):
            return self.tools["fact_lookup"], query

        # Fallback: search
        return self.tools["web_search"], query


# ---------------------------------------------------------------------------
# Demo runner
# ---------------------------------------------------------------------------


INTERACTIVE = False  # set True for a tiny REPL after demo


if __name__ == "__main__":
    agent = AdvancedReasoningModel()

    demo_questions = [
        "3 * (12 - 4) / 2",
        "capital of France?",
        "author of Hamlet",
        "Mount Everest height",
        "openai",
    ]

    for q in demo_questions:
        print(f"Q: {q}")
        ans, steps = agent.answer(q)
        print(f"A: {ans}\n")
        print("--- Chain of thought ----------------------------------")
        for s in steps:
            print(s)
            print("-------------------------------------------------------")
        print()

    if INTERACTIVE:
        print("ARM interactive mode; blank line to quit\n")
        while True:
            q = input("Q: ")
            if not q.strip():
                break
            a, st = agent.answer(q)
            print(f"A: {a}\n")
            print("--- Chain of thought ----------------------------------")
            for s in st:
                print(s)
                print("-------------------------------------------------------")
            print()

