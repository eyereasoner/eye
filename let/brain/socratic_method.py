#!/usr/bin/env python3
"""
socratic_method.py — A self-running, deterministic illustration of the
Socratic method in the context of planning a bike ride in Flanders.

Run:  python3 socratic_method.py
Outputs a fixed transcript that demonstrates guided inquiry using canonical
Socratic question families. No randomness, no user input, no time-based elements.
"""

import re
import textwrap
from dataclasses import dataclass
from typing import List, Callable

# ---------- Formatting helpers (deterministic) ----------

def wrap(s: str, width: int = 88) -> str:
    return textwrap.fill(s, width=width)

def say(lines: List[str], speaker: str, text: str, width: int = 88):
    lines.append(wrap(f"{speaker}: {text}", width))

# ---------- Tiny deterministic "key phrase" reflection ----------

STOPWORDS = {
    "the","a","an","and","or","but","so","to","of","in","on","at","for","with",
    "that","this","these","those","it","is","are","am","be","was","were","as",
    "by","from","about","into","over","after","before","under","between","your",
    "my","our","their","his","her","they","we","i","you","he","she","them","us",
    "me","do","does","did","have","has","had","if","then","than","because","just",
    "really","very","also","not","no","yes","more","less","most","least","some",
    "any","many","much","there","here","out","up","down","how","what","why","when",
}

def extract_key_phrase(text: str, max_words: int = 6) -> str:
    """
    Deterministic key-phrase heuristic: pick longer, non-stopword tokens,
    keep their first occurrence order. No randomness or time.
    """
    tokens = re.findall(r"[A-Za-z0-9'-]+", text.lower())
    candidates = [t for t in tokens if t not in STOPWORDS and len(t) > 2]
    if not candidates:
        return text.strip()[:60]
    unique = list(dict.fromkeys(candidates))  # preserve first occurrence order
    ranked = sorted(unique, key=lambda w: (-len(w), unique.index(w)))
    top = set(ranked[:max_words])
    ordered = [w for w in unique if w in top]
    return " ".join(ordered[:max_words])

# ---------- Socratic question families (no randomness) ----------

@dataclass
class Prompt:
    family: str
    text: str

Question = Callable[[str, List[str]], Prompt]

def q_clarification(last: str, history: List[str]) -> Prompt:
    key = extract_key_phrase(last)
    return Prompt("Clarification", f'When you say "{key}", what exactly do you mean?')

def q_reasons(last: str, history: List[str]) -> Prompt:
    return Prompt("Reasons/Evidence", "What reasons or evidence lead you to that view?")

def q_assumptions(last: str, history: List[str]) -> Prompt:
    key = extract_key_phrase(last)
    return Prompt("Assumptions", f'What assumptions are you making about "{key}"?')

def q_examples(last: str, history: List[str]) -> Prompt:
    return Prompt("Examples/Counterexamples", "Can you give a concrete example that illustrates your point?")

def q_counterview(last: str, history: List[str]) -> Prompt:
    key = extract_key_phrase(history[0] if history else last)
    return Prompt("Alternative Viewpoints", f'How might someone who disagrees with you about "{key}" respond?')

def q_implications(last: str, history: List[str]) -> Prompt:
    return Prompt("Implications/Consequences", "If your view is correct, what follows from that?")

def q_synthesis(last: str, history: List[str]) -> Prompt:
    return Prompt("Synthesis", "Given our discussion, how would you now summarize your view in one sentence?")

SCRIPT: List[Question] = [
    q_clarification,
    q_reasons,
    q_assumptions,
    q_examples,
    q_counterview,
    q_implications,
    q_synthesis,
]

# ---------- Deterministic “user” replies for a Flanders ride ----------

PLACE_NAME = "Leie & Scheldt Rivers Loop"  # You can change this and keep determinism.

DEMO_USER_REPLIES = [
    # Initial claim
    f"I think a bike trip in Flanders along the {PLACE_NAME} would be a great idea.",
    # Replies, one per question family in SCRIPT order:
    ("By interesting I mean quiet jaagpaden (towpaths), car-light fietsstraten, a flat route with a few "
     "short bridges, a scenic polder viewpoint, and a café stop for a waffle near the end. "
     "I’m thinking of a loop of roughly 45 km using the knooppunten (node) network."),
    ("Flanders is well set up for cycling: the knooppunten signs make navigation easy, the terrain is mostly "
     "flat, and there are long stretches of separated paths along the rivers. On similar rides I felt safe, "
     "got a solid workout, and enjoyed the scenery."),
    ("I'm assuming the weather is dry with manageable wind, no major works block the towpaths, any small ferry "
     "(veer) on the loop is operating, and my bike is in good condition. I’ll bring lights, a compact rain "
     "jacket, a spare tube, and a mini-pump."),
    ("For example: start near a train station, follow the Leie towpath out through green areas, cross to the "
     "Scheldt via a bridge, ride back along the Scheldt jaagpad, then finish in a village square for coffee. "
     "I’d note a few key knooppunten so the turns are easy to follow."),
    ("A critic might say wind across the polders can feel relentless, cobbles in historic centers can be jarring, "
     "popular paths get busy on sunny weekends, and if a ferry or path is closed the detour could be long."),
    ("If my view is correct, I should plan the loop so the headwind is on the outbound leg and the tailwind helps "
     "me home, check for any planned works and ferry schedules beforehand, download an offline map, and have a "
     "bridge alternative marked. If using the train, I’ll check the bike rules and timing for a quieter carriage."),
    ("My updated view: the ride is worthwhile with a dry forecast and moderate wind; I’ll use the knooppunten to "
     "link Leie and Scheldt towpaths, start early, keep a steady pace, and leave time for a café stop. If wind or "
     "closures look bad, I’ll switch to a shorter, more sheltered loop."),
]

# ---------- Demo runner ----------

def run_demo():
    width = 88
    coach = "Coach"
    user = "You"
    transcript: List[str] = []

    say(transcript, coach, "Socratic Coach (deterministic Flanders bike-trip demo).", width)
    say(transcript, coach, "We’ll explore your idea using structured questions.", width)

    # Initial move
    first = DEMO_USER_REPLIES[0]
    say(transcript, coach, "What question, claim, or plan would you like to examine?", width)
    say(transcript, user, first, width)
    history = [first]
    last = first

    # Scripted question families with predetermined answers
    for i, gen in enumerate(SCRIPT, start=1):
        prompt = gen(last, history)
        say(transcript, f"{coach} [{prompt.family}]", prompt.text, width)
        answer = DEMO_USER_REPLIES[i]  # deterministic pick
        say(transcript, user, answer, width)
        history.append(answer)
        last = answer

    say(transcript, coach, "Thank you for exploring that thoughtfully.", width)

    # Print the transcript
    print("\n".join(transcript))

if __name__ == "__main__":
    run_demo()

