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
from supy.data_model.schema import CURRENT_SCHEMA_VERSION
from supy.util.converter.yaml_upgrade import (
    _PACKAGE_TO_SCHEMA,  # noqa: PLC2701 - test introspects the registry
    upgrade_yaml,
)

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


def _needs_upgrade(path: Path) -> bool:
    """True if the fixture's release tag maps to a pre-current schema."""
    mapped = _PACKAGE_TO_SCHEMA.get(path.stem)
    return mapped is not None and mapped != CURRENT_SCHEMA_VERSION


_RELEASE_YAMLS = _collect_yaml(RELEASE_FIXTURES)
_CURRENT_RELEASE_YAMLS = [p for p in _RELEASE_YAMLS if not _needs_upgrade(p)]
_PRE_DRIFT_RELEASE_YAMLS = [p for p in _RELEASE_YAMLS if _needs_upgrade(p)]
_DOCS_YAMLS = _collect_yaml(DOCS_EXAMPLES)


@pytest.mark.cfg
class TestReleaseCompat:
    """Parse-without-error guard for vendored release + docs YAMLs."""

    @pytest.mark.parametrize(
        "yaml_path",
        _CURRENT_RELEASE_YAMLS,
        ids=[p.stem for p in _CURRENT_RELEASE_YAMLS] or ["no-current-fixtures"],
    )
    def test_last_release_sample_parses(self, yaml_path: Path):
        """Each current-schema release YAML must parse under the current validator."""
        # ARRANGE
        if not _CURRENT_RELEASE_YAMLS:
            pytest.skip(
                "No current-schema release fixtures vendored under "
                "test/fixtures/release_configs"
            )

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
        _PRE_DRIFT_RELEASE_YAMLS,
        ids=[p.stem for p in _PRE_DRIFT_RELEASE_YAMLS] or ["no-pre-drift-fixtures"],
    )
    def test_pre_drift_release_upgrades_and_parses(
        self, yaml_path: Path, tmp_path: Path
    ):
        """Each pre-drift fixture must upgrade cleanly and then parse.

        Fixtures whose release tag maps to a pre-current schema in
        `_PACKAGE_TO_SCHEMA` are expected to fail direct parsing - that is
        the whole point of the upgrade handler. This guard asserts the
        handler still produces a config that the current validator accepts.
        """
        # ARRANGE
        if not _PRE_DRIFT_RELEASE_YAMLS:
            pytest.skip("No pre-drift release fixtures registered yet")
        upgraded = tmp_path / f"{yaml_path.stem}-upgraded.yml"

        # ACT
        try:
            upgrade_yaml(
                input_path=yaml_path,
                output_path=upgraded,
                from_ver=yaml_path.stem,
            )
            SUEWSConfig.from_yaml(str(upgraded))
        except Exception as exc:
            pytest.fail(
                f"Pre-drift fixture {yaml_path.name} no longer upgrades cleanly "
                f"to the current schema: {type(exc).__name__}: {exc}\n\n"
                f"{_REMEDIATION_HINT}"
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
