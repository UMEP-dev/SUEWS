"""Coupled supy impact of the FAIBldg scheme change (gh#1594).

Single-grid density sweep comparing FAImethod=1 (grid-dependent #192) against the
resolution-independent scheme. The new scheme is exercised by injecting
FAIBldg = lambda_p * H / b0 through FAImethod=0 (OBSERVED), so the comparison runs
on an unmodified build; once the FAImethod=2 branch is built, set faimethod=2 and
drop the injection to confirm identical results.

Diagnostic only: keep to a single grid and a short period (local-run policy).
"""

import warnings
import logging

warnings.filterwarnings("ignore")
logging.disable(logging.INFO)

import numpy as np
import supy

H = 22.0
B0 = 15.0


def setup(df0, lp, faimethod, rslm):
    d = df0.copy()
    # Isolate buildings: zero trees/bare soil; trade building fraction with paved.
    fr = {0: 1 - 0.14 - 0.03 - lp, 1: lp, 2: 0.0, 3: 0.0, 4: 0.03, 5: 0.0, 6: 0.14}
    for i, v in fr.items():
        d[("sfr_surf", f"({i},)")] = v
    d[("bldgh", "0")] = H
    d[("roughlenmommethod", "0")] = 3  # MacDonald: FAI drives z0m
    d[("rslmethod", "0")] = rslm
    d[("faimethod", "0")] = faimethod
    if faimethod == 0:
        d[("faibldg", "0")] = lp * H / B0  # new scheme injected
        d[("faievetree", "0")] = 0.0
        d[("faidectree", "0")] = 0.0
    return d


def u_at(out, z_target):
    zc = [c for c in out.columns if c[0] == "RSL" and str(c[1]).startswith("z_")]
    uc = [c for c in out.columns if c[0] == "RSL" and str(c[1]).startswith("U_")]
    if not uc:
        return np.nan
    z = out[zc].mean().values
    u = out[uc].mean().values
    o = np.argsort(z)
    return float(np.interp(z_target, z[o], u[o]))


def main():
    df0, dff = supy.load_sample_data()
    f = dff.iloc[: 288 * 14]  # 2 weeks, single grid
    print(f"H={H} m, MacDonald roughness, trees zeroed, b0={B0} m. 2-week mean.\n")
    for rslm, lbl in [(0, "MOST (RSLMethod=0)"), (1, "RSL (RSLMethod=1)")]:
        print(f"=== {lbl} ===")
        print(" lp  | z0m(new) z0m(#192) | U10(new) U10(#192) | U3(new) U3(#192)")
        for lp in [0.1, 0.2, 0.3, 0.4, 0.5]:
            r = {}
            for tag, fm in [("new", 0), ("192", 1)]:
                out, _ = supy.run_supy(f, setup(df0, lp, fm, rslm))
                z0 = float(out[("SUEWS", "z0m")].mean())
                u10 = float(out[("SUEWS", "U10")].mean())
                u3 = u_at(out, 3.0) if rslm == 1 else np.nan
                r[tag] = (z0, u10, u3)
            print(
                f" {lp:.1f} |  {r['new'][0]:6.3f}  {r['192'][0]:7.4f}  |  "
                f"{r['new'][1]:5.2f}   {r['192'][1]:5.2f}   | "
                f"{r['new'][2]:5.2f}  {r['192'][2]:5.2f}"
            )


if __name__ == "__main__":
    main()
