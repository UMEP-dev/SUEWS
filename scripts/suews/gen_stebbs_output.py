"""Regenerate the STEBBS reference using the regression test's two-day run.

Run ``python scripts/suews/gen_stebbs_output.py`` from a built checkout.
The first day is spin-up; only the second day is stored, matching
``TestSTEBBSOutput.test_stebbs_building_energy_outputs``.
"""

from datetime import UTC, datetime
import json
from pathlib import Path
import platform
import subprocess
import sys

import supy as sp

repo_root = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(repo_root / "test" / "fixtures" / "data_test"))
from sample_output_io import reference_file_digest, reference_source  # noqa: E402

fixture_dir = repo_root / "test" / "fixtures" / "data_test" / "stebbs_test"
simulation = sp.SUEWSSimulation(fixture_dir / "sample_config.yml")
forcing = simulation.forcing.df.loc["2017-08-26":"2017-08-27"].copy()
# The fixture uses -999 for dry periods; the supported API requires zero.
forcing["rain"] = forcing["rain"].clip(lower=0)
simulation.update_forcing(forcing)
output = simulation.run(start_date=forcing.index[0], end_date=forcing.index[-1])
reference = output.df.STEBBS.iloc[288:].droplevel("grid")
reference.to_csv(fixture_dir / "sample_output_stebbs.csv")
files = ("sample_config.yml", "test_2017_data_15_mp.txt", "sample_output_stebbs.csv")
provenance = {
    **reference_source(repo_root),
    "generator": "scripts/suews/gen_stebbs_output.py",
    "generated_date": datetime.now(UTC).date().isoformat(),
    "platform": f"{platform.system()} {platform.machine()}",
    "compiler": subprocess.check_output(
        ["gfortran", "--version"], text=True
    ).splitlines()[0],
    "python": platform.python_version(),
    "build_command": "make dev",
    "spin_up": str(forcing.index[0].date()),
    "validation": str(reference.index[0].date()),
    "rows": len(reference),
    "columns": len(reference.columns),
    "hash_format": "SHA-256 with CRLF folded to LF",
    "files": {
        name: reference_file_digest(fixture_dir / name)[0]
        for name in files
    },
}
(fixture_dir / "provenance.json").write_text(
    json.dumps(provenance, indent=2) + "\n", encoding="utf-8"
)
print(f"[OK] Saved {len(reference)} STEBBS validation timesteps")
