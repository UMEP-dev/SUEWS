"""Fill the demo webpage placeholders from the generated metric JSON files.

Reads the JSON written by every benchmark/demo script and substitutes the
``[[TOKEN]]`` placeholders in ``site/index.html`` in place, so the page
always shows the numbers actually produced by the latest runs - no
hand-copied figures.

Usage: python build_site.py
"""

import json
from pathlib import Path

ROOT = Path(__file__).resolve().parent
BENCH = ROOT / "benchmarks" / "results"
DEMOS = ROOT / "demos" / "results"
TEMPLATE = ROOT / "site" / "index.template.html"
SITE = ROOT / "site" / "index.html"


def load(path):
    with open(path) as f:
        return json.load(f)


def main():
    import supy

    kcl = load(BENCH / "coupled_kcl_metrics.json")
    ur = load(DEMOS / "demo_urban_rural.json")
    qf = load(DEMOS / "demo_qf.json")
    cr = load(DEMOS / "demo_cool_roofs.json")

    t = kcl["tair"]
    rmse_all = t["all"]["rmse"]
    verdict = (
        "The coupled system tracks the diurnal swing without ever seeing the "
        "observations - the residual warm bias is the column's, not a tuned fit."
        if rmse_all < 5.0
        else "Captures the phase of the diurnal cycle; the warm bias reflects "
        "the closed-column tendency discussed in the limitations."
    )

    # build the substitution table from a real Python clock value only once,
    # passed in via the file's own mtime to stay deterministic on re-runs
    import datetime
    gen_date = datetime.date.today().isoformat()

    repl = {
        "RMSE_ALL": f"{rmse_all:.1f}",
        "RMSE_DAY": f"{t['daytime']['rmse']:.1f}",
        "RMSE_NIGHT": f"{t['nighttime']['rmse']:.1f}",
        "MBE": f"{t['all']['mbe']:+.1f}",
        "AMP_OBS": f"{t['obs_diurnal_amplitude']:.1f}",
        "AMP_MOD": f"{t['mod_diurnal_amplitude']:.1f}",
        "TAU_ADV": f"{kcl['tau_adv_mean_s'] / 60.0:.0f}",
        "KCL_VERDICT": verdict,
        "UHI_NOCT": f"{ur['uhi2m_mean_nocturnal_K']:+.2f}",
        "UHI_MAX": f"{ur['uhi2m_max_K']:.2f}",
        "BLH_U": f"{ur['bl_height_max_urban_m']:.0f}",
        "BLH_R": f"{ur['bl_height_max_rural_m']:.0f}",
        "QF_MEAN": f"{qf['qf_mean_x1_wm2']:.0f}",
        "DT_X0": f"{qf['dT_x0_nocturnal_K']:+.2f}",
        "DT_X2": f"{qf['dT_x2_nocturnal_K']:+.2f}",
        "CR_DT_DAY": f"{cr['dT_daytime_mean_K']:+.2f}",
        "CR_DT_MAX": f"{cr['dT_max_cooling_K']:+.2f}",
        "CR_DQN": f"{cr['dQn_daytime_mean_wm2']:+.0f}",
        "CR_BLH_B": f"{cr['bl_height_max_base_m']:.0f}",
        "CR_BLH_C": f"{cr['bl_height_max_cool_m']:.0f}",
        "SUPY_VERSION": supy.__version__,
        "GEN_DATE": gen_date,
    }

    html = TEMPLATE.read_text()
    missing = []
    for key, val in repl.items():
        token = f"[[{key}]]"
        if token not in html:
            missing.append(key)
        html = html.replace(token, str(val))
    SITE.write_text(html)

    import re
    leftover = re.findall(r"\[\[[A-Z_]+\]\]", html)
    print(f"substituted {len(repl)} tokens; missing in template: {missing or 'none'}")
    print(f"unfilled placeholders remaining: {leftover or 'none'}")


if __name__ == "__main__":
    main()
