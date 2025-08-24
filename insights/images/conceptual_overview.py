#!/usr/bin/env python3
'''
Generate an SVG infographic for the LLM → Python pipeline.

Defaults match the "compact height + bigger fonts + teal middle box" version.

Usage:
    python generate_llm_infographic.py --out llm_flow_infographic.svg
    python generate_llm_infographic.py --variant orange --out llm_flow_orange.svg
    python generate_llm_infographic.py --variant purple --out llm_flow_purple.svg

Variants change only the MIDDLE box header + bullet accent color.
'''
from pathlib import Path
import argparse

def build_svg(variant: str = "teal") -> str:
    # --- Layout constants (compact height) ---
    MARGIN = 40
    BOX_W = 720
    SPACING = 40

    HEADER_H = 110
    TOP_PAD = 34
    LINE_GAP = 42     # distance between bullet baselines
    BOTTOM_PAD = 32
    BOX_H = HEADER_H + TOP_PAD + 2*LINE_GAP + BOTTOM_PAD  # 3 bullets -> 2 gaps

    CANVAS_W = MARGIN*2 + 3*BOX_W + 2*SPACING
    CANVAS_H = MARGIN*2 + BOX_H

    x1 = MARGIN
    x2 = MARGIN + BOX_W + SPACING
    x3 = MARGIN + 2*(BOX_W + SPACING)
    y = MARGIN

    # --- Color variants for the middle box ---
    variants = {
        "teal":   {"grad_id": "hdrTeal",   "bullet": "#06b6d4", "grad_stops": ("#99f6e4", "#67e8f9")},
        "orange": {"grad_id": "hdrOrange", "bullet": "#d97706", "grad_stops": ("#ffedd5", "#fed7aa")},
        "purple": {"grad_id": "hdrPurple", "bullet": "#7c3aed", "grad_stops": ("#ede9fe", "#ddd6fe")},
    }
    if variant not in variants:
        raise SystemExit(f"Unknown variant '{variant}'. Choose from: {', '.join(variants)}")

    mid = variants[variant]

    # --- SVG assembly helpers ---
    svg = []
    svg.append(f'''<svg xmlns="http://www.w3.org/2000/svg" width="{CANVAS_W}" height="{CANVAS_H}" viewBox="0 0 {CANVAS_W} {CANVAS_H}" role="img" aria-label="LLM to Python infographic">
  <defs>
    <style>
      .box {{ fill: #ffffff; stroke: #0f172a; stroke-width: 3.2; rx: 26; ry: 26; filter: drop-shadow(0px 2px 2px rgba(0,0,0,0.05)); }}
      .title {{ font: 800 42px "Inter", "Segoe UI", Arial, Helvetica, sans-serif; fill: #0f172a; }}
      .bulletText {{ font: 600 32px "Inter", "Segoe UI", Arial, Helvetica, sans-serif; fill: #0b1220; }}
      .arrow {{ stroke: #0f172a; stroke-width: 5; fill: none; }}
    </style>
    <!-- Left header gradient (blue) -->
    <linearGradient id="hdrBlue" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#dbeafe"/>
      <stop offset="100%" stop-color="#bfdbfe"/>
    </linearGradient>
    <!-- Right header gradient (green) -->
    <linearGradient id="hdrGreen" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#dcfce7"/>
      <stop offset="100%" stop-color="#bbf7d0"/>
    </linearGradient>

    <!-- Middle header gradient (variant) -->
    <linearGradient id="{mid['grad_id']}" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="{mid['grad_stops'][0]}"/>
      <stop offset="100%" stop-color="{mid['grad_stops'][1]}"/>
    </linearGradient>
  </defs>
  <rect x="0" y="0" width="{CANVAS_W}" height="{CANVAS_H}" fill="#ffffff"/>
''')

    def add_box(x, y, w, h, header_title, header_fill, accent, bullets):
        inner_pad_x = 34
        start_y = y + HEADER_H + TOP_PAD
        svg.append(f'<rect class="box" x="{x}" y="{y}" width="{w}" height="{h}" rx="26" ry="26"/>')
        svg.append(f'<rect x="{x}" y="{y}" width="{w}" height="{HEADER_H}" rx="26" ry="26" fill="url(#{header_fill})"/>')
        svg.append(f'<text class="title" x="{x+inner_pad_x}" y="{y+74}">{header_title}</text>')
        cy = start_y
        for text in bullets:
            cx = x + inner_pad_x + 6
            svg.append(f'<circle cx="{cx}" cy="{cy-7}" r="9" fill="{accent}"/>')
            svg.append(f'<text class="bulletText" x="{cx+28}" y="{cy}">{text}</text>')
            cy += LINE_GAP

    def add_arrow(from_right_x, to_left_x, y_top, h):
        mid_y = y_top + h/2
        svg.append(f'<line class="arrow" x1="{from_right_x}" y1="{mid_y}" x2="{to_left_x-32}" y2="{mid_y}"/>')
        svg.append(f'<path d="M {to_left_x-32} {mid_y-12} L {to_left_x} {mid_y} L {to_left_x-32} {mid_y+12} Z" fill="#0f172a"/>')

    # Left box (blue)
    add_box(
        x1, y, BOX_W, BOX_H,
        "LLM Input", "hdrBlue", "#2563eb",
        ["Data (e.g., RDF)", "Logic (e.g., N3)", "Goal"]
    )

    # Middle box (variant color)
    add_box(
        x2, y, BOX_W, BOX_H,
        "LLM (e.g., GPT-5 Thinking)", mid["grad_id"], mid["bullet"],
        ["Translates Data + Logic + Goal", "Constructs proof strategy", "Synthesizes Python code"]
    )

    # Right box (green)
    add_box(
        x3, y, BOX_W, BOX_H,
        "Python Output", "hdrGreen", "#16a34a",
        ["Answer", "Reason why", "Check (harness)"]
    )

    add_arrow(x1+BOX_W, x2, y, BOX_H)
    add_arrow(x2+BOX_W, x3, y, BOX_H)

    svg.append('</svg>')
    return "\n".join(svg)

def main():
    parser = argparse.ArgumentParser(description="Generate LLM → Python infographic SVG.")
    parser.add_argument("--out", type=Path, default=Path("conceptual_overview.svg"), help="Output SVG path")
    parser.add_argument("--variant", choices=["teal", "orange", "purple"], default="teal",
                        help="Color variant for the middle box")
    args = parser.parse_args()

    svg = build_svg(args.variant)
    args.out.write_text(svg, encoding="utf-8")
    print(f"Wrote {args.out.resolve()}")

if __name__ == "__main__":
    main()

