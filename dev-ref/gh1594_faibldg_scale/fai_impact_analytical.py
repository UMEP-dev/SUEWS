"""Closed-form impact of the FAIBldg scheme change (gh#1594).

Compares FAImethod=1 (grid-dependent, sqrt(lambda_p/A)*H) with the proposed
resolution-independent scheme (lambda_p*H/b0) for FAIBldg, the MacDonald z0m,
and the RSL length scale Lc / attenuation coefficient beta. Formulas mirror
suews_phys_resist.f95 and suews_phys_rslprof.f95. No supy build required.
"""

import numpy as np

K_CD = 0.5 * 1.0 * 1.2 / 0.4**2  # = 3.75, MacDonald z0m drag group


def fai_192(lp, H, A):
    return np.sqrt(lp / A) * H


def fai_new(lp, H, b0=15.0):
    return lp * H / b0


def zd_macdonald(lp, Zh):
    return (1 + 4.43 ** (-lp) * (lp - 1)) * Zh


def z0m_macdonald(lp, Zh, fai):
    zd = zd_macdonald(lp, Zh)
    r = 1 - zd / Zh
    fai = max(fai, 1e-5)
    return r * np.exp(-((K_CD * r * fai) ** (-0.5))) * Zh, zd


def lc_rsl(lp, Zh, fai):
    return (1 - lp) / max(fai, 1e-5) * Zh


def beta_rsl(fai):
    b = (3.444 * fai**0.971) / (1 + 10.487 * fai**0.971)
    return min(max(b, 0.15), 0.50)


def main():
    H = 15.0
    print(f"Building-only grid, H={H} m, MacDonald roughness, b0=15 m\n")
    print("=== FAIBldg: #192 scale-dependence vs new (scale-free) ===")
    for lp in [0.1, 0.3, 0.5]:
        print(
            f"  lp={lp}: new={fai_new(lp, H):.3f} | "
            f"#192 @100m={fai_192(lp, H, 1e4):.3f} "
            f"@500m={fai_192(lp, H, 2.5e5):.4f} @1km={fai_192(lp, H, 1e6):.4f}"
        )
    print("\n=== z0m (m) MacDonald: #192(1km) vs new ===")
    for lp in [0.1, 0.3, 0.5]:
        z0_new, zd = z0m_macdonald(lp, H, fai_new(lp, H))
        z0_192, _ = z0m_macdonald(lp, H, fai_192(lp, H, 1e6))
        print(
            f"  lp={lp}: zd={zd:.1f}  z0m_new={z0_new:.3f}  "
            f"z0m_#192={z0_192:.2e}  ratio={z0_new / max(z0_192, 1e-12):.0f}x"
        )
    print("\n=== RSL Lc (m) and beta: #192(1km) vs new ===")
    for lp in [0.1, 0.3, 0.5]:
        print(
            f"  lp={lp}: Lc_new={lc_rsl(lp, H, fai_new(lp, H)):.0f}  "
            f"Lc_#192={lc_rsl(lp, H, fai_192(lp, H, 1e6)):.0f}   "
            f"beta_new={beta_rsl(fai_new(lp, H)):.3f}  "
            f"beta_#192={beta_rsl(fai_192(lp, H, 1e6)):.3f}"
        )


if __name__ == "__main__":
    main()
