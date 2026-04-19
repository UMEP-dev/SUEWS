"""
Release-compatibility tests that guard against YAML config drift.

For each vendored last-release sample YAML and each documented example YAML,
`SUEWSConfig.from_yaml` must parse successfully under the current validator.
Failures indicate structural drift that either needs a matching handler in
`src/supy/util/converter/yaml_upgrade.py` or a justified fixture refresh.

See #1301 (meta) and #1302 (this guard).
"""

from pathlib import Path

import pytest

from supy.data_model.core.config import SUEWSConfig

pytestmark = pytest.mark.api

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
RELEASE_FIXTURES = REPO_ROOT / "test" / "fixtures" / "release_configs"
DOCS_EXAMPLES = REPO_ROOT / "docs" / "source" / "inputs" / "yaml" / "examples"

_REMEDIATION_HINT = (
    "If this fixture predates a structural change on master, add a matching "
    "handler in src/supy/util/converter/yaml_upgrade.py (see #1304) or "
    "refresh the fixture with a justifying commit. See #1301 for the overall "
    "policy."
)


def _collect_yaml(directory: Path) -> list[Path]:
    """Return every *.yml file in `directory`, sorted for deterministic ids."""
    if not directory.is_dir():
        return []
    return sorted(p for p in directory.iterdir() if p.suffix in {".yml", ".yaml"})


_RELEASE_YAMLS = _collect_yaml(RELEASE_FIXTURES)
_DOCS_YAMLS = _collect_yaml(DOCS_EXAMPLES)


@pytest.mark.cfg
class TestReleaseCompat:
    """Parse-without-error guard for vendored release + docs YAMLs."""

    @pytest.mark.parametrize(
        "yaml_path",
        _RELEASE_YAMLS,
        ids=[p.stem for p in _RELEASE_YAMLS] or ["no-release-fixtures"],
    )
    def test_last_release_sample_parses(self, yaml_path: Path):
        """Each vendored release YAML must parse under the current validator."""
        # ARRANGE
        if not _RELEASE_YAMLS:
            pytest.skip("No release fixtures vendored under test/fixtures/release_configs")

        # ACT / ASSERT
        try:
            SUEWSConfig.from_yaml(str(yaml_path))
        except Exception as exc:
            pytest.fail(
                f"Release fixture {yaml_path.name} no longer parses under current "
                f"schema: {type(exc).__name__}: {exc}\n\n{_REMEDIATION_HINT}"
            )

    @pytest.mark.parametrize(
        "yaml_path",
        _DOCS_YAMLS,
        ids=[p.stem for p in _DOCS_YAMLS] or ["no-docs-examples"],
    )
    def test_docs_example_parses(self, yaml_path: Path):
        """Each documented YAML example must parse under the current validator."""
        # ARRANGE
        if not _DOCS_YAMLS:
            pytest.skip("No docs examples under docs/source/inputs/yaml/examples")

        # ACT / ASSERT
        try:
            SUEWSConfig.from_yaml(str(yaml_path))
        except Exception as exc:
            pytest.fail(
                f"Docs example {yaml_path.name} no longer parses under current "
                f"schema: {type(exc).__name__}: {exc}\n\n{_REMEDIATION_HINT}"
            )
