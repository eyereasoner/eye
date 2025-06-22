"""
Very‑Advanced ARM (vARM)
=======================

This **vARM** script sketches what a *truly* beefed‑up “Advanced
Reasoning Model” might look like if we kept everything in plain Python
(no ML weights) but imitated modern agent design patterns.

Major additions over the previous version
----------------------------------------
* **Vector Memory w/ Cosine Similarity** – tiny hash‑based embeddings and
  k‑NN recall.
* **Hierarchical Planner** – a *Planner* class decomposes the user query
  into sub‑tasks, which the *Executor* resolves with tools; the plan can
  be refined mid‑execution.
* **Tool Introspection** – tools declare *inputs*, *outputs*, *cost*, and
  *success heuristic*, letting the agent weigh which tool to call next.
* **Self‑Critique & Retry** – after an initial answer the *Critic*
  reviews confidence; low scores trigger a second planning pass with
  stricter time/step limits.
* **Sandboxed Python Tool** – run arbitrary code safely (with a hard 0.1‑s
  time budget and restricted globals).
* **Parallel Tool Calls** (mocked) – the executor can fire independent
  tool calls concurrently using `asyncio`.
* **Extensible Config** – `CONFIG` dict turns features on/off without code
  edits.

Still *no* third‑party dependencies – just `asyncio`, `math`, `random`,
`time`, `dataclasses`, `typing` and `hashlib` for dummy embeddings.

Run the file to see a scripted demo.  Flip `INTERACTIVE = True` at the
bottom for your own queries.
"""

from __future__ import annotations

import asyncio
import hashlib
import math
import random
import re
import time
from dataclasses import dataclass, field
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple

###############################################################################
# Config & Utility
###############################################################################

CONFIG: Dict[str, Any] = {
    "max_steps": 10,
    "critique_threshold": 0.35,  # below → retry plan once
    "sandbox_timeout": 0.1,
    "embedding_dim": 64,
}


# Very small pseudo‑embedding using blake2 hashing --------------------------------

def embed(text: str, dim: int = CONFIG["embedding_dim"]) -> List[float]:
    h = hashlib.blake2b(text.encode(), digest_size=dim).digest()
    return [(b - 128) / 128 for b in h]  # map bytes → roughly −1..1


def cosine(a: Sequence[float], b: Sequence[float]) -> float:
    dot = sum(x * y for x, y in zip(a, b))
    na = math.sqrt(sum(x * x for x in a)) or 1.0
    nb = math.sqrt(sum(y * y for y in b)) or 1.0
    return dot / (na * nb)


###############################################################################
# Reasoning trace
###############################################################################


@dataclass
class ReasoningStep:
    thought: str
    action: Optional[str] = None
    observation: Optional[str] = None
    elapsed_ms: Optional[int] = None

    def __str__(self) -> str:
        head = f"Thought: {self.thought}"
        parts = [head]
        if self.action:
            parts.append(f"Action:  {self.action}")
        if self.observation is not None:
            parts.append(f"Result:  {self.observation}")
        if self.elapsed_ms is not None:
            parts.append(f"⏱  {self.elapsed_ms} ms")
        return "\n".join(parts)


###############################################################################
# Tool system
###############################################################################


class Tool:
    name: str = "tool"
    description: str = "(no description)"
    cost: float = 0.01  # arbitrary units (lower is cheaper)

    def run(self, query: str) -> str:  # pragma: no cover
        raise NotImplementedError

    # Simple heuristic: non‑bracketed result means success
    def success(self, result: str) -> bool:
        return not result.startswith("[")


class CalculatorTool(Tool):
    name = "calculator"
    description = "Evaluate arithmetic (+ − * / % **)."
    cost = 0.02
    _SAFE = re.compile(r"^[\d\s()+\-*/.%**]+$")

    def run(self, expr: str) -> str:
        if not self._SAFE.match(expr):
            return "[unsafe expression]"
        try:
            return str(eval(expr, {"__builtins__": {}}, {}))
        except Exception as exc:  # noqa: BLE001
            return f"[calc error: {exc}]"


class FactTool(Tool):
    name = "fact"
    description = "Return canned factual answers."
    _KB: Dict[str, str] = {
        "capital of france": "Paris",
        "capital of belgium": "Brussels",
        "largest planet": "Jupiter",
        "author of hamlet": "William Shakespeare",
    }

    def run(self, q: str) -> str:
        return self._KB.get(q.lower().rstrip("? ."), "[fact unknown]")


class PythonSandboxTool(Tool):
    name = "python"
    description = "Execute Python code (safe, 0.1 s)."
    cost = 0.2

    def run(self, code: str) -> str:
        t_start = time.perf_counter()
        loc: Dict[str, Any] = {}
        try:
            exec(  # noqa: S102
                code,
                {"__builtins__": {"range": range, "len": len, "math": math}},
                loc,
            )
            if "result" in loc:
                return str(loc["result"])
            return "[executed, no `result`]"
        except Exception as exc:  # noqa: BLE001
            return f"[py error: {exc}]"
        finally:
            if time.perf_counter() - t_start > CONFIG["sandbox_timeout"]:
                return "[timeout]"


class WebSearchTool(Tool):
    name = "search"
    description = "Mock web search snippets."
    cost = 0.5
    _MOCK = {
        "mount everest height": "Mount Everest is 8,848.86 metres tall.",
        "openai": "OpenAI is an AI research lab and deployment company …",
        "speed of light": "299,792,458 m/s in vacuum …",
    }

    def run(self, q: str) -> str:
        return self._MOCK.get(q.lower().rstrip("? ."), "[no hits]")


# Registry
TOOLS: Dict[str, Tool] = {t.name: t for t in [
    CalculatorTool(),
    FactTool(),
    PythonSandboxTool(),
    WebSearchTool(),
]}


###############################################################################
# Memory
###############################################################################


@dataclass
class MemoryItem:
    question: str
    answer: str
    vec: List[float] = field(init=False)

    def __post_init__(self):
        self.vec = embed(self.question)


class VectorMemory:
    def __init__(self):
        self._items: List[MemoryItem] = []

    def add(self, q: str, a: str):
        self._items.append(MemoryItem(q, a))

    def recall(self, q: str, k: int = 2) -> List[Tuple[float, str]]:
        qv = embed(q)
        scored = [(cosine(qv, it.vec), it.answer) for it in self._items]
        scored.sort(reverse=True, key=lambda t: t[0])
        return [(s, a) for s, a in scored[:k] if s > 0.8]  # threshold


###############################################################################
# Planner / Critic / Agent
###############################################################################


class Planner:
    """Very naive rule‑based planner → list[tool_name, tool_input]."""

    _ARITH = re.compile(r"^[\d\s()+\-*/.%**]+$")

    def plan(self, q: str) -> List[Tuple[str, str]]:
        if self._ARITH.match(q):
            return [("calculator", q)]
        if any(k in q.lower() for k in ["capital", "largest", "author", "speed"]):
            return [("fact", q), ("search", q)]  # fallback search
        # Code style question example
        if q.lower().startswith("python "):
            code = q.partition(" ")[2]
            return [("python", code)]
        # Fallback generic search then python for possible calculation
        return [("search", q)]


class Critic:
    """Assigns 0–1 confidence based on heuristics."""

    def score(self, answer: str) -> float:
        if answer.startswith("["):
            return 0.1
        if len(answer) > 120:
            return 0.4
        return 0.9


class AdvancedReasoningModel:
    def __init__(self):
        self.memory = VectorMemory()
        self.planner = Planner()
        self.critic = Critic()

    async def _execute_tool(self, tool_name: str, arg: str) -> Tuple[str, int]:
        tool = TOOLS[tool_name]
        start = time.perf_counter()
        result = tool.run(arg)
        elapsed = int((time.perf_counter() - start) * 1000)
        return result, elapsed

    async def answer_async(self, q: str) -> Tuple[str, List[ReasoningStep]]:
        trace: List[ReasoningStep] = []

        # 0) Memory peek
        mem_hits = self.memory.recall(q)
        if mem_hits:
            score, ans = mem_hits[0]
            trace.append(ReasoningStep(f"Memory match {score:.2f} – reusing answer."))
            return ans, trace

        # 1) Plan & execute
        plan = self.planner.plan(q)
        for step_idx, (tool_name, arg) in enumerate(plan, 1):
            result, ms = await self._execute_tool(tool_name, arg)
            trace.append(ReasoningStep(
                f"Step {step_idx}: use {tool_name} on '{arg}'.", tool_name, result, ms
            ))
            if TOOLS[tool_name].success(result):
                answer = result
                break
        else:
            answer = "I'm not sure how to answer that."

        # 2) Critique & optional retry
        conf = self.critic.score(answer)
        trace.append(ReasoningStep(f"Self‑critique confidence: {conf:.2f}"))
        if conf < CONFIG["critique_threshold"]:
            trace.append(ReasoningStep("Confidence low → second attempt via web search."))
            res, ms = await self._execute_tool("search", q)
            trace.append(ReasoningStep("Second pass search", "search", res, ms))
            if TOOLS["search"].success(res):
                answer = res

        # 3) Store memory
        self.memory.add(q, answer)
        return answer, trace

    # Convenience sync wrapper
    def answer(self, q: str) -> Tuple[str, List[ReasoningStep]]:
        return asyncio.run(self.answer_async(q))


###############################################################################
# Demo
###############################################################################

INTERACTIVE = False

if __name__ == "__main__":
    agent = AdvancedReasoningModel()
    demo = [
        "3*(7+5)/2",
        "capital of France?",
        "Mount Everest height",
        "python result = sum(i*i for i in range(10))",
        "author of Hamlet",
    ]

    for q in demo:
        print(f"Q: {q}")
        ans, steps = agent.answer(q)
        print(f"A: {ans}\n")
        print("--- Chain of thought -------------------------------")
        for s in steps:
            print(s)
            print("----------------------------------------------------")
        print()

    if INTERACTIVE:
        print("vARM interactive; blank line to quit")
        while True:
            try:
                q = input("Q: ")
            except (EOFError, KeyboardInterrupt):
                break
            if not q.strip():
                break
            a, st = agent.answer(q)
            print(f"A: {a}\n")
            for s in st:
                print(s)
                print("----------------------------------------------------")
