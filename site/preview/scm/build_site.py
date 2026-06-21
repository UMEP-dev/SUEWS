"""Generate the SCM preview page from the archived validation metrics.

Substitutes every ``[[TOKEN]]`` placeholder in ``index.template.html``
with values read from the version-controlled fixtures under
``test/fixtures/scm/`` and writes ``index.html``. The build is strict
and deterministic:

- the template token set and the substitution table must match exactly
  (missing, unknown or leftover tokens fail the build with exit code 1);
- no wall-clock or environment values enter the output - the footer
  provenance (generation date, SuPy version) comes from the committed
  ``provenance.json`` fixture, so regenerating the page on any machine
  on any day yields byte-identical output.

Usage:
    python build_site.py          # regenerate index.html
    python build_site.py --check  # verify the committed index.html is
                                  # fresh (exit 2 on drift); used in tests
"""

import argparse
import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]
METRICS = REPO / "test" / "fixtures" / "scm"
TEMPLATE = HERE / "index.template.html"
SITE = HERE / "index.html"

TOKEN_RE = re.compile(r"\[\[([A-Z0-9_]+)\]\]")


def load(path):
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def build_replacements():
    """Substitution table, built solely from committed fixtures."""
    kcl = load(METRICS / "coupled_kcl_metrics.json")
    ur = load(METRICS / "demo_urban_rural.json")
    qf = load(METRICS / "demo_qf.json")
    cr = load(METRICS / "demo_cool_roofs.json")
    nat = load(METRICS / "native_vs_python.json")
    my = load(METRICS / "multiyear.json")
    prov = load(METRICS / "provenance.json")

    t = kcl["tair"]
    rmse_all = t["all"]["rmse"]
    verdict = (
        "The coupled system tracks the diurnal swing with the air-temperature "
        "observations withheld after initialisation - the residual warm bias "
        "is the column's, not a tuned fit."
        if rmse_all < 5.0
        else "Captures the phase of the diurnal cycle; the warm bias reflects "
        "the closed-column tendency discussed in the limitations."
    )

    return {
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
        "SUPY_VERSION": prov["supy_version"],
        "GEN_DATE": prov["evidence_generated"],
    }


def render():
    """Render the page; raise SystemExit(1) on any token contract breach."""
    template = TEMPLATE.read_text(encoding="utf-8")
    repl = build_replacements()

    template_tokens = set(TOKEN_RE.findall(template))
    table_tokens = set(repl)
    problems = []
    if table_tokens - template_tokens:
        problems.append(
            f"tokens in table but not in template: {sorted(table_tokens - template_tokens)}"
        )
    if template_tokens - table_tokens:
        problems.append(
            f"tokens in template but not in table: {sorted(template_tokens - table_tokens)}"
        )
    if problems:
        for p in problems:
            print(f"ERROR: {p}", file=sys.stderr)
        raise SystemExit(1)

    html = template
    for key, val in repl.items():
        html = html.replace(f"[[{key}]]", str(val))

    leftover = TOKEN_RE.findall(html)
    if leftover:
        print(f"ERROR: unfilled placeholders after substitution: {leftover}", file=sys.stderr)
        raise SystemExit(1)
    return html


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="verify the committed index.html matches the fixtures (exit 2 on drift)",
    )
    args = parser.parse_args(argv)

    html = render()
    if args.check:
        committed = SITE.read_text(encoding="utf-8") if SITE.exists() else ""
        if committed != html:
            print(
                "ERROR: committed index.html is stale - regenerate with "
                "`python build_site.py`",
                file=sys.stderr,
            )
            raise SystemExit(2)
        print("index.html is fresh")
        return
    SITE.write_text(html, encoding="utf-8")
    print(f"rendered index.html ({len(TOKEN_RE.findall(TEMPLATE.read_text(encoding='utf-8')))} tokens)")


if __name__ == "__main__":
    main()
