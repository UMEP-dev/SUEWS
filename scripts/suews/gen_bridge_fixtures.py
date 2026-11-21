"""Generate portable input for the buffer-only Rust bridge tests.

Run from the repository root with the project venv active:
    python scripts/suews/gen_bridge_fixtures.py
"""

from pathlib import Path

import yaml

from supy._run_rust import config_to_engine_yaml  # ruff: ignore[import-private-name]
from supy.data_model import SUEWSConfig

REPO_ROOT = Path(__file__).resolve().parents[2]
FIXTURES = {
    "sample": REPO_ROOT / "src/supy/sample_data/sample_config.yml",
    "stebbs": REPO_ROOT / "test/fixtures/data_test/stebbs_test/sample_config.yml",
}
OUTPUT_DIR = REPO_ROOT / "test/fixtures/bridge"


def resolved_fixture(name: str) -> bytes:
    """Resolve model values without capturing a file-output destination."""
    # As in test_sample_output._write_run_inputs, load the mapping without a
    # yaml_path: from_yaml embeds that checkout-specific path in the dump.
    with FIXTURES[name].open(encoding="utf-8") as handle:
        config = SUEWSConfig.from_dict(yaml.safe_load(handle))
    return config_to_engine_yaml(config, for_file_output=False).encode("utf-8")


def main() -> None:
    """Write both resolved fixtures from the current data model."""
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    for name in FIXTURES:
        path = OUTPUT_DIR / f"{name}_resolved.yml"
        path.write_bytes(resolved_fixture(name))
        print(f"Wrote {path.relative_to(REPO_ROOT)}")


if __name__ == "__main__":
    main()
