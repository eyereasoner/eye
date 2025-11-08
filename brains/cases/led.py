#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
LED (Light Emitting Diode) Emulator + Circuit Solver + Harness
==============================================================

This program emulates a basic LED using a simple but instructive physical model:

  • Electrical: Shockley diode equation with ideality factor n, saturation current Is,
                thermal voltage Vt = kT/q, and an optional series resistance Rs
                inside the LED package.
  • Optical:    Electroluminescent photon rate ≈ (I / q) × EQE, with photon energy
                set by the LED bandgap Eg. Radiant flux Φe = photons/s × hν.
                Luminous flux Φv ≈ 683 lm/W × V(λ) × Φe using a simple photopic
                sensitivity approximation V(λ).

Given a supply V_s and a series resistor R, we solve for the operating point:

    V_s = I·R + ( LED forward voltage ) + I·R_s

where the LED forward voltage follows Shockley:

    I = Is * (exp(Vd / (n*Vt)) - 1),   so Vd = n*Vt * ln(1 + I/Is)

We answer a concrete “bench” question, then print:
  Answer / Reason / Check

Notes:
- This is an educational model (not a full SPICE). It’s surprisingly good for
  ballpark behavior and trade-off intuition.
- Units are SI unless stated otherwise.
"""

import math
from dataclasses import dataclass

# -----------------
# Physical constants
# -----------------
q = 1.602176634e-19    # Coulomb
k = 1.380649e-23       # J/K
h = 6.62607015e-34     # J·s
c = 299_792_458.0      # m/s

# -------------------------------------------------------
# A tiny photopic sensitivity approximation V(λ) (unitless)
# -------------------------------------------------------
def photopic_V_lambda_nm(wavelength_nm: float) -> float:
    """
    Approximate the CIE 1931 photopic luminous efficiency V(λ) for humans.
    We use a smooth, compact approximation with two log-Gaussians that
    reproduces the general shape (peak ~ 555 nm) without external data.
    Accuracy is fine for demo purposes.

    Returns:
        V (unitless, 0..1)
    """
    wl = max(380.0, min(780.0, float(wavelength_nm)))  # clamp to visible band
    # Two bumps: one around 555 nm, a smaller blue-side shoulder
    # Parameters tuned for qualitative agreement (not standards-grade).
    def gauss(x, mu, sigma):
        return math.exp(-0.5 * ((x - mu) / sigma) ** 2)

    V = 0.999 * gauss(wl, 555.0, 60.0) + 0.26 * gauss(wl, 460.0, 40.0)
    return max(0.0, min(1.0, V))

# -----------------------
# LED device model (electrical + optical)
# -----------------------
@dataclass
class LED:
    """
    Simple LED model.
    Eg_eV : bandgap energy in eV (sets nominal photon energy and color)
    Is    : saturation current (A)
    n     : ideality factor (typ. 1.6..2.2 for many visible LEDs)
    Rs    : series resistance internal to LED (Ω)
    EQE   : external quantum efficiency (0..1): fraction of electrons that
            result in an emitted photon escaping the package.
    T     : junction temperature (K)
    """
    Eg_eV: float = 1.9         # Red LED ~ 1.8–2.0 eV
    Is: float = 1e-12          # A
    n: float = 2.0             # dimensionless
    Rs: float = 10.0           # Ω
    EQE: float = 0.30          # 30% — generous for didactic purposes
    T: float = 300.0           # K (≈ 27 °C)

    @property
    def Vt(self) -> float:
        """Thermal voltage natively used with Shockley (kT/q)."""
        return k * self.T / q

    @property
    def photon_energy_J(self) -> float:
        """Photon energy ~ Eg in Joules."""
        return self.Eg_eV * q

    @property
    def peak_wavelength_nm(self) -> float:
        """Peak wavelength from Eg (hc/E)."""
        lam_m = (h * c) / self.photon_energy_J
        return lam_m * 1e9  # nm

    def diode_voltage_from_current(self, I: float) -> float:
        """
        Electrical forward drop inside the junction (Shockley).
        Does NOT include series resistance; caller may add I*Rs separately.
        """
        if I <= 0:
            return 0.0
        return self.n * self.Vt * math.log(1.0 + I / self.Is)

    def current_from_diode_voltage(self, Vd: float) -> float:
        """Inverse Shockley (junction only, no Rs)."""
        if Vd <= 0:
            return 0.0
        return self.Is * (math.exp(Vd / (self.n * self.Vt)) - 1.0)

    def operating_point(self, Vs: float, R: float) -> dict:
        """
        Solve the series circuit: Vs -> R -> (LED junction + Rs).
        Unknown is current I. We root-find on:
            f(I) = I*(R + Rs) + n*Vt*ln(1 + I/Is) - Vs = 0
        using a safe monotonic bracketing + Newton refinement.
        """
        # Helper: f(I) and derivative
        def f(I):
            if I < 0:
                return -Vs  # keep it monotone
            return I * (R + self.Rs) + self.n * self.Vt * math.log1p(I / self.Is) - Vs

        def df(I):
            # d/dI of ln(1+I/Is) is 1/(Is + I) * 1
            return (R + self.Rs) + (self.n * self.Vt) / (self.Is + max(I, 0.0) + 1e-30)

        # Bracket: start at 0 and a conservative upper bound
        I_low = 0.0
        I_high = max(1e-12, Vs / max(1e-3, (R + self.Rs))) * 5.0  # generous
        while f(I_high) < 0:
            I_high *= 2.0
            if I_high > 5.0:  # >5 A in this tiny demo? nope — bail out
                break

        I = min(max((Vs - 2.0) / max(R + self.Rs, 1e-9), 1e-9), I_high)  # seed near “LED ~2V”
        # Newton with fallback to bisection
        for _ in range(80):
            Fi = f(I)
            if abs(Fi) < 1e-12:
                break
            dFi = df(I)
            if dFi <= 0:
                # fallback to bisection if derivative goes goofy
                mid = 0.5 * (I_low + I_high)
                I = max(0.0, mid)
                continue
            step = Fi / dFi
            I_new = I - step
            # Keep the iterate in [I_low, I_high]
            if I_new <= I_low or I_new >= I_high or not math.isfinite(I_new):
                # Bisection
                if Fi > 0:
                    I_high = I
                else:
                    I_low = I
                I = 0.5 * (I_low + I_high)
            else:
                # Accept Newton step and update bracket
                if Fi > 0:
                    I_high = I
                else:
                    I_low = I
                I = I_new

        # Final electrical quantities
        Vj = self.diode_voltage_from_current(I)      # junction drop
        Vled = Vj + I * self.Rs                      # LED terminals drop incl. Rs
        Vr = Vs - Vled                               # resistor drop
        # Optical quantities
        photon_rate = (I / q) * self.EQE             # photons/s to the outside
        radiant_flux_W = photon_rate * self.photon_energy_J
        lam_nm = self.peak_wavelength_nm
        Vlambda = photopic_V_lambda_nm(lam_nm)
        luminous_flux_lm = 683.0 * Vlambda * radiant_flux_W

        # Residuals for a quick sanity check
        kcl_residual = Vs - (I * R + Vled)
        return {
            "I_A": I,
            "V_led_V": Vled,
            "V_junction_V": Vj,
            "V_resistor_V": Vr,
            "radiant_W": radiant_flux_W,
            "luminous_lm": luminous_flux_lm,
            "lambda_nm": lam_nm,
            "Vlambda": Vlambda,
            "photon_rate_s": photon_rate,
            "kcl_residual_V": kcl_residual,
        }

    # A second, independent solver (solve for Vd, then get I) for cross-check
    def operating_point_vsolve(self, Vs: float, R: float) -> dict:
        """
        Solve by bracketing on diode *junction* voltage Vj and include Rs afterwards:
            I(Vj) = Is*(exp(Vj/(nVt)) - 1)
            Equation in Vj:  Vj + I(Vj)*(R + Rs) - Vs = 0
        """
        def g(Vj):
            I = self.current_from_diode_voltage(max(0.0, Vj))
            return Vj + I * (R + self.Rs) - Vs

        # Bracket Vj in [0, Vs]
        low, high = 0.0, max(0.0, Vs)
        glow, ghigh = g(low), g(high)
        # If g(high) < 0 (unlikely for sane params), stretch
        tries = 0
        while ghigh < 0 and tries < 50:
            high *= 2.0
            ghigh = g(high)
            tries += 1

        # Bisection + a bit of secant polish
        for _ in range(100):
            mid = 0.5 * (low + high)
            gmid = g(mid)
            if abs(gmid) < 1e-12 or (high - low) < 1e-12:
                Vj = mid
                break
            if gmid > 0:
                high = mid
            else:
                low = mid
        else:
            Vj = 0.5 * (low + high)

        I = self.current_from_diode_voltage(max(0.0, Vj))
        Vled = Vj + I * self.Rs
        photon_rate = (I / q) * self.EQE
        radiant_flux_W = photon_rate * self.photon_energy_J
        lam_nm = self.peak_wavelength_nm
        Vlambda = photopic_V_lambda_nm(lam_nm)
        luminous_flux_lm = 683.0 * Vlambda * radiant_flux_W
        kcl_residual = Vs - (I * R + Vled)
        return {
            "I_A": I,
            "V_led_V": Vled,
            "V_junction_V": Vj,
            "radiant_W": radiant_flux_W,
            "luminous_lm": luminous_flux_lm,
            "lambda_nm": lam_nm,
            "Vlambda": Vlambda,
            "kcl_residual_V": kcl_residual,
        }

# -------------------------
# Demo / Harness / Question
# -------------------------
def run_demo(trace: bool = False):
    """
    Question:
      With a 5.0 V supply and a 330 Ω resistor, what is the operating point of a
      red LED (Eg=1.9 eV, Is=1e-12 A, n=2.0, Rs=10 Ω, EQE=30%) at 300 K?
      Report current, forward voltage, radiant power, and (approx) luminous flux.

    We compute an Answer using the current-solver, explain the Reason, and
    Check by solving the same circuit a different way (voltage-solver) and
    ensuring KCL residuals are tiny and both solvers agree.
    """
    # Circuit
    Vs = 5.0     # V
    R = 330.0    # Ω

    # LED parameters (tweak these to try different colors/efficiencies)
    led = LED(
        Eg_eV=2.7,   # blue
        Is=1e-12,
        n=2.2,
        Rs=10.0,
        EQE=0.30,
        T=300.0
    )

    # Solve two ways
    op_I = led.operating_point(Vs, R)
    op_V = led.operating_point_vsolve(Vs, R)

    # Format a compact Answer
    I_mA = op_I["I_A"] * 1e3
    Vled = op_I["V_led_V"]
    P_rad_mW = op_I["radiant_W"] * 1e3
    lum_lm = op_I["luminous_lm"]
    lam = op_I["lambda_nm"]

    answer = (
        f"I ≈ {I_mA:.2f} mA, V_LED ≈ {Vled:.3f} V, "
        f"radiant ≈ {P_rad_mW:.3f} mW, luminous ≈ {lum_lm:.3f} lm "
        f"(λ≈{lam:.0f} nm)."
    )

    # Reason (what the emulator did)
    reason = (
        "We modeled the LED with the Shockley equation I = Is·(e^{Vj/(n·Vt)}−1) "
        "plus an internal series resistance Rs. In series with a 330 Ω resistor, "
        "we solved Vs = I·R + (Vj + I·Rs) for the current. Optical output assumes "
        "EQE photons per electron, each at energy ≈ Eg, so radiant power is "
        "Φe = (I/q)·EQE·(h·c/λ). Luminous flux is 683·V(λ)·Φe using an approximate "
        "photopic sensitivity V(λ)."
    )

    # Checks:
    #  1) Compare the two independent solvers (current-based vs voltage-based).
    #  2) Ensure KCL residuals are tiny.
    agree_I = abs(op_I["I_A"] - op_V["I_A"])
    agree_V = abs(op_I["V_led_V"] - op_V["V_led_V"])
    res_I = abs(op_I["kcl_residual_V"])
    res_V = abs(op_V["kcl_residual_V"])

    checks_ok = (agree_I < 1e-9) and (agree_V < 1e-9) and (res_I < 1e-9) and (res_V < 1e-9)
    check = (
        f"PASS — solvers agree (ΔI={agree_I:.2e} A, ΔV={agree_V:.2e} V), "
        f"KCL residuals {res_I:.1e} V / {res_V:.1e} V."
        if checks_ok else
        f"CHECK WARN — ΔI={agree_I:.2e} A, ΔV={agree_V:.2e} V, "
        f"residuals {res_I:.1e}/{res_V:.1e} V."
    )

    # Print final result
    print("Answer:", answer)
    print("\nReason:", reason)
    print("\nCheck: ", check)

    if trace:
        # Optional detailed dump
        print("\n-- Operating point (current solver) --")
        for k, v in op_I.items():
            print(f"{k:>16}: {v}")
        print("\n-- Operating point (voltage solver) --")
        for k, v in op_V.items():
            print(f"{k:>16}: {v}")

if __name__ == "__main__":
    run_demo(trace=False)

