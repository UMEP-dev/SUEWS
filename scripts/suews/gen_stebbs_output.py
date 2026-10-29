"""Regenerate the STEBBS reference using the regression test's two-day run.

Run ``python scripts/suews/gen_stebbs_output.py`` from a built checkout.
The first day is spin-up; only the second day is stored, matching
``TestSTEBBSOutput.test_stebbs_building_energy_outputs``.
"""

from pathlib import Path

import supy as sp

repo_root = Path(__file__).resolve().parents[2]
fixture_dir = repo_root / "test" / "fixtures" / "data_test" / "stebbs_test"
simulation = sp.SUEWSSimulation(fixture_dir / "sample_config.yml")
forcing = simulation.forcing.df.loc["2017-08-26":"2017-08-27"].copy()
# The fixture uses -999 for dry periods; the supported API requires zero.
forcing["rain"] = forcing["rain"].clip(lower=0)
simulation.update_forcing(forcing)
output = simulation.run(start_date=forcing.index[0], end_date=forcing.index[-1])
reference = output.df.STEBBS.iloc[288:].droplevel("grid")
reference.to_csv(fixture_dir / "sample_output_stebbs.csv")
print(f"[OK] Saved {len(reference)} STEBBS validation timesteps")
