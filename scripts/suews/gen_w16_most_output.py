"""Regenerate the W16/MOST fixture using its recorded execution procedure.

Run ``python scripts/suews/gen_w16_most_output.py`` from a built checkout.
Writes the CSV and provenance together; the numerical-noise tolerance stays
unchanged in ``test_stability_profile_combinations``.
"""

from datetime import UTC, datetime
import hashlib
import json
from pathlib import Path
import platform
import subprocess

import supy as sp

repo_root = Path(__file__).resolve().parents[2]
fixture_dir = repo_root / "test" / "fixtures"
sample_dir = repo_root / "src" / "supy" / "sample_data"
sample = sp.SUEWSSimulation.from_sample_data()
config = sample.config.model_copy(deep=True)
config.model.physics.stability = 2
config.model.physics.roughness_sublayer = 0
forcing = sample.forcing.df.iloc[:288].copy()
simulation = sp.SUEWSSimulation()
simulation.update_config(config, auto_load_forcing=False)
simulation.update_forcing(forcing)
output = simulation.run(start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1)
reference = output.df["SUEWS"][["QH", "QE", "T2", "U10"]]
csv_path = fixture_dir / "w16_most.csv"
reference.to_csv(csv_path, float_format="%.17g")

provenance_path = fixture_dir / "w16_most.provenance.json"
provenance = json.loads(provenance_path.read_text())
provenance.update(
    description="W16/MOST regression baseline refreshed for the shared physical constants in #1953.",
    source_ref=subprocess.check_output(["git", "branch", "--show-current"], cwd=repo_root, text=True).strip(),
    source_commit=subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo_root, text=True).strip(),
    generated_date=datetime.now(UTC).date().isoformat(),
    platform=f"{platform.system()} {platform.machine()}",
    compiler=subprocess.check_output(["gfortran", "--version"], text=True).splitlines()[0],
    python=platform.python_version(),
    sample_config_sha256=hashlib.sha256((sample_dir / "sample_config.yml").read_bytes()).hexdigest(),
    hourly_forcing_sha256=hashlib.sha256((sample_dir / "Kc_2012_data_60.txt").read_bytes()).hexdigest(),
    csv_sha256=hashlib.sha256(csv_path.read_bytes()).hexdigest(),
)
provenance_path.write_text(json.dumps(provenance, indent=2) + "\n")
print(f"[OK] Saved {len(reference)} W16/MOST timesteps and provenance")
