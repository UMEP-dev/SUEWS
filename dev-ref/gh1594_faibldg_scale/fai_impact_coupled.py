"""Coupled supy impact of the FAIBldg scheme change (gh#1594).

Single-grid density sweep comparing the revised FAImethod=1 branch against the
legacy grid-dependent #192 formula. The legacy branch is emulated by injecting
FAIBldg = sqrt(lambda_p / A) * H through FAImethod=0 (OBSERVED); the revised
branch is exercised directly with FAImethod=1.

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


def setup(df0, lp, scheme, rslm):
    d = df0.copy()
    # Isolate buildings: zero trees/bare soil; trade building fraction with paved.
    fr = {0: 1 - 0.14 - 0.03 - lp, 1: lp, 2: 0.0, 3: 0.0, 4: 0.03, 5: 0.0, 6: 0.14}
    for i, v in fr.items():
        d[("sfr_surf", f"({i},)")] = v
    d[("bldgh", "0")] = H
    d[("roughlenmommethod", "0")] = 3  # MacDonald: FAI drives z0m
    d[("rslmethod", "0")] = rslm
    if scheme == "revised":
        d[("faimethod", "0")] = 1
    elif scheme == "legacy_192":
        d[("faimethod", "0")] = 0
        area = float(d[("surfacearea", "0")].iloc[0])
        d[("faibldg", "0")] = np.sqrt(lp / area) * H
        d[("faievetree", "0")] = 0.0
        d[("faidectree", "0")] = 0.0
    else:
        raise ValueError(f"Unknown FAI scheme: {scheme}")
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
        print(" lp  | z0m(rev) z0m(#192) | U10(rev) U10(#192) | U3(rev) U3(#192)")
        for lp in [0.1, 0.2, 0.3, 0.4, 0.5]:
            r = {}
            for tag, scheme in [("revised", "revised"), ("192", "legacy_192")]:
                out, _ = supy.run_supy(f, setup(df0, lp, scheme, rslm))
                z0 = float(out[("SUEWS", "z0m")].mean())
                u10 = float(out[("SUEWS", "U10")].mean())
                u3 = u_at(out, 3.0) if rslm == 1 else np.nan
                r[tag] = (z0, u10, u3)
            print(
                f" {lp:.1f} |  {r['revised'][0]:6.3f}  {r['192'][0]:7.4f}  |  "
                f"{r['revised'][1]:5.2f}   {r['192'][1]:5.2f}   | "
                f"{r['revised'][2]:5.2f}  {r['192'][2]:5.2f}"
            )


if __name__ == "__main__":
    main()
