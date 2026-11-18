"""Generate the resolved YAML consumed by Rust bridge tests.

Run from the repository root with the project venv active:
    python scripts/suews/gen_bridge_fixtures.py
"""

from pathlib import Path

import yaml

# Repository tooling deliberately reuses the private engine hand-off.
from supy._run_rust import config_to_engine_yaml  # noqa: PLC2701
from supy.data_model import SUEWSConfig

REPO_ROOT = Path(__file__).resolve().parents[2]
FIXTURES = {
    "sample": REPO_ROOT / "src/supy/sample_data/sample_config.yml",
    "stebbs": REPO_ROOT / "test/fixtures/data_test/stebbs_test/sample_config.yml",
}
OUTPUT_DIR = REPO_ROOT / "test/fixtures/bridge"


def resolved_fixture(name: str) -> bytes:
    """Use the sample-output check's loader and the engine's shared dumper."""
    # As in test_sample_output._write_run_inputs, load the mapping without a
    # yaml_path: from_yaml embeds that checkout-specific path in the dump.
    with FIXTURES[name].open(encoding="utf-8") as handle:
        config = SUEWSConfig.from_dict(yaml.safe_load(handle))
    return config_to_engine_yaml(config).encode("utf-8")


def main() -> None:
    """Write both resolved fixtures from the current data model."""
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    for name in FIXTURES:
        path = OUTPUT_DIR / f"{name}_resolved.yml"
        path.write_bytes(resolved_fixture(name))
        print(f"Wrote {path.relative_to(REPO_ROOT)}")


if __name__ == "__main__":
    main()
