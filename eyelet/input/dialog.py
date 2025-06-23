"""
Socratic Dialogue Emulator (deterministic)
========================================
This script *simulates* a Socratic conversation between a teacher and a student
**without** any external LLM.  All variability now comes from Python’s `random`
module, but we expose a `--seed` flag (default **42**) so that every run with
the same arguments produces identical output.

Usage
-----
```bash
python socratic.py --topic "Pythagorean theorem" --steps 6 --seed 42
```
Change the seed (or omit the flag) if you ever *do* want different dialogue.
Only the Python standard library is required.
"""
from __future__ import annotations

import argparse
import random
import re
from collections import defaultdict
from typing import List

# ----------------------------------------------------------------------------
# Minimal stop‑word list for quick "keyword" extraction
# ----------------------------------------------------------------------------
_STOPWORDS = {
    "the","a","an","and","or","but","if","while","as","of","to","in","on","for","with",
    "about","is","are","was","were","be","being","been","it","this","that","those","these",
    "i","you","he","she","we","they","them","us","his","her","their","my","our",
}

_WORD_RE = re.compile(r"[A-Za-z']+")


def extract_keywords(text: str, n: int = 3) -> List[str]:
    """Return up to *n* non‑stopwords occurring in *text*, most frequent first."""
    counts: defaultdict[str, int] = defaultdict(int)
    for w in _WORD_RE.findall(text.lower()):
        if w not in _STOPWORDS:
            counts[w] += 1
    # most common first, but preserve original order as tie‑breaker
    sorted_words = sorted(counts.items(), key=lambda t: (-t[1], text.lower().find(t[0])))
    return [w for w, _ in sorted_words[:n]] or ["it"]  # fallback

# ----------------------------------------------------------------------------
# Tiny knowledge base (feel free to extend!)
# ----------------------------------------------------------------------------
_FACTS = {
    "pythagorean theorem": [
        "It states that in a right‑angled triangle, the square of the hypotenuse equals the sum of the squares of the other two sides.",
        "The theorem can be written as *c² = a² + b²* where c is the length of the hypotenuse.",
        "It only holds for Euclidean geometry; on curved surfaces the relationship changes.",
        "One intuitive proof uses shear transformations to show that area is preserved.",
        "There are reputedly hundreds of proofs, including one attributed to President Garfield.",
    ],
    "justice": [
        "One classical view sees justice as giving each person what they are due.",
        "Plato describes justice in the individual as harmony between parts of the soul.",
        "Modern theories, like Rawls’, emphasise fairness under a veil of ignorance.",
        "Utilitarians might equate justice with actions that maximise aggregate welfare.",
        "Debates arise when collective welfare conflicts with individual rights.",
    ],
}

_GENERIC_FACTS = [
    "That topic involves multiple perspectives across history and cultures.",
    "Empirical evidence as well as logical reasoning both play roles in understanding it.",
    "There are open questions that scholars continue to debate today.",
    "Practical applications influence how society values the concept.",
]

# ----------------------------------------------------------------------------
# Dialogue agents
# ----------------------------------------------------------------------------
class Teacher:
    """Generates Socratic questions based on the student’s latest answer."""

    _TEMPLATES_SINGLE = [
        "Why do you think {kw}?",
        "Could you clarify what you mean by {kw}?",
        "What underlying assumptions lead you to focus on {kw}?",
        "How might someone challenge your view on {kw}?",
        "What evidence supports your statement about {kw}?",
    ]

    _TEMPLATES_DOUBLE = [
        "How does {kw1} relate to {kw2}?",
        "If {kw1} holds, what does that imply for {kw2}?",
        "Can you reconcile {kw1} with {kw2}?",
    ]

    def next_question(self, student_reply: str) -> str:
        kws = extract_keywords(student_reply, 2)
        if len(kws) >= 2 and random.random() < 0.5:
            kw1, kw2 = kws[0], kws[1]
            template = random.choice(self._TEMPLATES_DOUBLE)
            return template.format(kw1=kw1, kw2=kw2)
        else:
            kw = kws[0]
            template = random.choice(self._TEMPLATES_SINGLE)
            return template.format(kw=kw)

class Student:
    """Responds with sequential facts plus a pinch of reflection."""

    def __init__(self, topic: str):
        canonical = topic.lower().strip()
        self.facts = _FACTS.get(canonical, _GENERIC_FACTS.copy())
        self.index = 0

    def reply(self, teacher_question: str) -> str:
        kw = extract_keywords(teacher_question, 1)[0]
        fact = self.facts[self.index] if self.index < len(self.facts) else random.choice(_GENERIC_FACTS)
        self.index += 1
        return f"I believe {kw} is important because {fact}"

# ----------------------------------------------------------------------------
# Main loop
# ----------------------------------------------------------------------------

def socratic_dialog(topic: str, steps: int):
    teacher, student = Teacher(), Student(topic)

    opening = f"Let us discuss {topic}. To begin, what do you already know about it?"
    print(f"Teacher: {opening}")

    # Student opens the floor responding to the initial prompt
    student_answer = student.reply(opening)
    print(f"Student: {student_answer}\n")

    conversation_log: List[str] = [student_answer]

    for _ in range(steps):
        question = teacher.next_question(conversation_log[-1])
        print(f"Teacher: {question}")

        answer = student.reply(question)
        print(f"Student: {answer}\n")

        conversation_log.append(answer)

    # Simple closing summary
    print("Teacher (closing): You have reflected on several aspects of the topic.")
    print("                 Consider how these ideas might apply in practice as your next inquiry.")

# ----------------------------------------------------------------------------
# CLI entry‑point
# ----------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Run a deterministic Socratic dialogue emulator.")
    parser.add_argument("--topic", default="Pythagorean theorem", help="Subject to discuss")
    parser.add_argument("--steps", type=int, default=5, help="Number of Q&A cycles after the opening exchange")
    parser.add_argument("--seed", type=int, default=42, help="Random seed for reproducibility")
    args = parser.parse_args()

    # One call is enough because the script only uses functions from the global RNG
    random.seed(args.seed)

    socratic_dialog(args.topic, args.steps)

if __name__ == "__main__":
    main()

