# ----------------------------------
# Leg Length Discrepancy Measurement
# ----------------------------------
#
# See http://www.agfa.com/w3c/2002/10/medicad/op/
#

from dataclasses import dataclass, field
from math import sqrt


@dataclass
class Measurement:
    # ──────────  “input triples”  ──────────
    p1xCm: float
    p1yCm: float
    p2xCm: float
    p2yCm: float
    p3xCm: float
    p3yCm: float
    p4xCm: float
    p4yCm: float

    # ────────── derived values (populated in .compute) ──────────
    # you can inspect these after construction
    # e.g.   m = Measurement(...);  print(m.dCm, m.LLDAlarm)

    #   scalar results
    dx12Cm: float = field(init=False)
    dy12Cm: float = field(init=False)
    dy13Cm: float = field(init=False)
    dy24Cm: float = field(init=False)
    cL1: float = field(init=False)
    dL3m: float = field(init=False)
    cL3: float = field(init=False)
    pL1x1Cm: float = field(init=False)
    pL1x2Cm: float = field(init=False)
    pL3x3Cm: float = field(init=False)
    pL3x4Cm: float = field(init=False)
    dd13Cm: float = field(init=False)
    ddy13Cm: float = field(init=False)
    dd24Cm: float = field(init=False)
    ddy24Cm: float = field(init=False)
    ddL13: float = field(init=False)
    p5xCm: float = field(init=False)
    p5yCm: float = field(init=False)
    p6xCm: float = field(init=False)
    p6yCm: float = field(init=False)
    dx51Cm: float = field(init=False)
    dx53Cm: float = field(init=False)
    dx62Cm: float = field(init=False)
    dx64Cm: float = field(init=False)
    dy53Cm: float = field(init=False)
    dy64Cm: float = field(init=False)
    d53Cm: float = field(init=False)
    d64Cm: float = field(init=False)
    dCm: float = field(init=False)

    #   alarms
    LLDAlarm: bool = field(init=False)

    # ────────── crunch all rules ──────────
    def compute(self) -> None:
        # basic differences
        self.dx12Cm = self.p1xCm - self.p2xCm
        self.dy12Cm = self.p1yCm - self.p2yCm
        self.dy13Cm = self.p1yCm - self.p3yCm
        self.dy24Cm = self.p2yCm - self.p4yCm

        # slope of L1 and its “negative reciprocal”
        self.cL1  = self.dy12Cm / self.dx12Cm
        self.dL3m = 1 / self.cL1
        self.cL3  = 0 - self.dL3m

        # intercept-like helpers
        self.pL1x1Cm = self.cL1 * self.p1xCm
        self.pL1x2Cm = self.cL1 * self.p2xCm
        self.pL3x3Cm = self.cL3 * self.p3xCm
        self.pL3x4Cm = self.cL3 * self.p4xCm

        # diagonal deltas
        self.dd13Cm  = self.pL1x1Cm - self.pL3x3Cm
        self.ddy13Cm = self.dd13Cm  - self.dy13Cm
        self.dd24Cm  = self.pL1x2Cm - self.pL3x4Cm
        self.ddy24Cm = self.dd24Cm  - self.dy24Cm

        # line-slope difference
        self.ddL13 = self.cL1 - self.cL3

        # unknown points p5 & p6 (intersection projections)
        self.p5xCm = self.ddy13Cm / self.ddL13
        self.dx51Cm = self.p5xCm - self.p1xCm
        self.p5yCm = self.cL1 * self.dx51Cm + self.p1yCm

        self.p6xCm = self.ddy24Cm / self.ddL13
        self.dx62Cm = self.p6xCm - self.p2xCm
        self.p6yCm = self.cL1 * self.dx62Cm + self.p2yCm

        # more deltas
        self.dx53Cm = self.p5xCm - self.p3xCm
        self.dx64Cm = self.p6xCm - self.p4xCm
        self.dy53Cm = self.p5yCm - self.p3yCm
        self.dy64Cm = self.p6yCm - self.p4yCm

        # squared sums (Pythagoras)
        sdx53Cm2 = self.dx53Cm ** 2
        sdx64Cm2 = self.dx64Cm ** 2
        sdy53Cm2 = self.dy53Cm ** 2
        sdy64Cm2 = self.dy64Cm ** 2
        ssd53Cm2 = sdx53Cm2 + sdy53Cm2
        ssd64Cm2 = sdx64Cm2 + sdy64Cm2

        # Euclidean distances
        self.d53Cm = sqrt(ssd53Cm2)
        self.d64Cm = sqrt(ssd64Cm2)

        # final diagnostic delta
        self.dCm = self.d53Cm - self.d64Cm

        # alarm test (|dCm| > 1.25 cm)
        self.LLDAlarm = self.dCm < -1.25 or self.dCm > 1.25

    # auto-run on construction
    def __post_init__(self):
        self.compute()


# ────────── example run using the numbers in the N3 file ──────────
if __name__ == "__main__":
    m = Measurement(
        p1xCm=10.1, p1yCm=7.8,
        p2xCm=45.1, p2yCm=5.6,
        p3xCm=3.6,  p3yCm=29.8,
        p4xCm=54.7, p4yCm=28.5
    )

    print(f"dCm      = {m.dCm:+.3f} cm")
    print(f"LLDAlarm = {m.LLDAlarm}")
    # → dCm = -1.908 cm  →  alarm raised (outside ±1.25 cm)

