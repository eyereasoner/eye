# create svg for Insights - Conceptual Overview

MARGIN = 40
BOX_W = 720
SPACING = 40

# Layout (unchanged height)
HEADER_H = 110
TOP_PAD = 34
LINE_GAP = 42
BOTTOM_PAD = 32
BOX_H = HEADER_H + TOP_PAD + 2*LINE_GAP + BOTTOM_PAD

CANVAS_W = MARGIN*2 + 3*BOX_W + 2*SPACING
CANVAS_H = MARGIN*2 + BOX_H

x1 = MARGIN
x2 = MARGIN + BOX_W + SPACING
x3 = MARGIN + 2*(BOX_W + SPACING)
y = MARGIN

svg = []

svg.append(f'''<svg xmlns="http://www.w3.org/2000/svg" width="{CANVAS_W}" height="{CANVAS_H}" viewBox="0 0 {CANVAS_W} {CANVAS_H}" role="img" aria-label="LLM to Python infographic — larger fonts">
  <defs>
    <style>
      .box {{ fill: #ffffff; stroke: #111827; stroke-width: 3.2; rx: 26; ry: 26; filter: drop-shadow(0px 2px 2px rgba(0,0,0,0.05)); }}
      .title {{ font: 800 42px "Inter", "Segoe UI", Arial, Helvetica, sans-serif; fill: #0f172a; }}
      .bulletText {{ font: 600 32px "Inter", "Segoe UI", Arial, Helvetica, sans-serif; fill: #0b1220; }}
      .arrow {{ stroke: #111827; stroke-width: 5; fill: none; }}
    </style>
    <linearGradient id="hdrBlue" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#dbeafe"/>
      <stop offset="100%" stop-color="#bfdbfe"/>
    </linearGradient>
    <linearGradient id="hdrOrange" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#ffedd5"/>
      <stop offset="100%" stop-color="#fed7aa"/>
    </linearGradient>
    <linearGradient id="hdrGreen" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#dcfce7"/>
      <stop offset="100%" stop-color="#bbf7d0"/>
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
    svg.append(f'<path d="M {to_left_x-32} {mid_y-12} L {to_left_x} {mid_y} L {to_left_x-32} {mid_y+12} Z" fill="#111827"/>')

# Left box (blue)
add_box(
    x1, y, BOX_W, BOX_H,
    "LLM Input", "hdrBlue", "#2563eb",
    ["Data (e.g., RDF)", "Logic (e.g., N3 and RDF Surfaces)", "Goal"]
)

# Middle box (orange)
add_box(
    x2, y, BOX_W, BOX_H,
    "LLM (e.g., GPT-5 Thinking)", "hdrOrange", "#d97706",
    ["Translates Data + Logic + Goal", "Constructs proof strategy", "Synthesizes self-contained Python code"]
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

out = "conceptual_overview.svg"
with open(out, "w", encoding="utf-8") as f:
    f.write("\n".join(svg))

out

