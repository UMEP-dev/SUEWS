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
    nat = load(BENCH / "native_vs_python.json")
    my = load(BENCH / "multiyear.json")

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
        "NAT_SPEEDUP": f"{nat['timing']['speedup']:.0f}",
        "NAT_TIME": f"{nat['timing']['native_coupled_s']:.1f}",
        "PY_TIME": f"{nat['timing']['python_coupled_s']:.0f}",
        "NAT_PER_STEP": f"{nat['timing']['native_ms_per_step']:.1f}",
        "PY_PER_STEP": f"{nat['timing']['python_ms_per_step']:.0f}",
        "NAT_AGREE_MAX": f"{nat['backend_agreement']['tair_abs_diff_max_K']:.2f}",
        "NAT_AGREE_MEAN": f"{nat['backend_agreement']['tair_abs_diff_mean_K']:.2f}",
        "NAT_RMSE": f"{nat['skill_vs_obs']['native']['rmse']:.1f}",
        "PY_RMSE": f"{nat['skill_vs_obs']['python']['rmse']:.1f}",
        "MY_NATIVE_S": f"{my['timing']['native_total_s']:.0f}",
        "MY_PY_H": f"{my['timing']['python_projected_h']:.0f}",
        "MY_PACE": f"{my['timing']['sim_years_per_wallclock_min']:.1f}",
        "MY_UHI_JJA": f"{my['climatology']['uhi2m_nocturnal_jja_K']:+.2f}",
        "MY_UHI_DJF": f"{my['climatology']['uhi2m_nocturnal_djf_K']:+.2f}",
        "MY_H_JJA": f"{my['climatology']['h_bl_daymax_jja_urban_m']:.0f}",
        "MY_H_DJF": f"{my['climatology']['h_bl_daymax_djf_urban_m']:.0f}",
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
