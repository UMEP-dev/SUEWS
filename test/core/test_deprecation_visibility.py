"""Public-surface tests for the procedural API retirement."""

from __future__ import annotations

import json
import subprocess
import sys
import textwrap

import pytest

pytestmark = pytest.mark.api


REMOVED_FUNCTIONAL_NAMES = {
    "init_supy",
    "init_config",
    "load_SampleData",
    "load_config_from_df",
    "load_sample_data",
    "resample_output",
    "run_supy",
    "run_supy_sample",
    "save_supy",
}


def _probe_public_surface() -> dict:
    """Inspect the lazy public surface in a clean interpreter."""
    probe = textwrap.dedent(
        f"""
        import json
        import sys
        import warnings
        import supy

        removed = {sorted(REMOVED_FUNCTIONAL_NAMES)!r}
        traces = {{}}
        for name in removed:
            try:
                getattr(supy, name)
            except AttributeError as exc:
                traces[name] = {{"error": str(exc)}}
            else:
                traces[name] = {{"resolved": True}}

        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            loader = getattr(supy, "load_forcing_grid")

        from supy._supy_module import _FUNCTIONAL_DEPRECATIONS
        json.dump(
            {{
                "all": sorted(supy.__all__),
                "deprecated": sorted(_FUNCTIONAL_DEPRECATIONS),
                "removed": traces,
                "loader_callable": callable(loader),
                "loader_warnings": [
                    {{"category": item.category.__name__, "message": str(item.message)}}
                    for item in caught
                ],
                "utilities": {{
                    "check_forcing": callable(supy.check_forcing),
                    "check_state": callable(supy.check_state),
                }},
            }},
            sys.stdout,
        )
        """
    )
    result = subprocess.run(
        [sys.executable, "-c", probe],
        capture_output=True,
        text=True,
        check=True,
    )
    return json.loads(result.stdout)


_SURFACE = _probe_public_surface()


@pytest.mark.core
def test_only_umep_loader_remains_deprecated():
    """Keep only the compatibility name required by the UMEP processor."""
    assert _SURFACE["deprecated"] == ["load_forcing_grid"]
    assert _SURFACE["loader_callable"] is True
    assert _SURFACE["loader_warnings"] == [
        {
            "category": "FutureWarning",
            "message": (
                "`supy.load_forcing_grid` is deprecated and will be removed in a "
                "future release. Please migrate to "
                "`SUEWSSimulation(path).forcing`."
            ),
        }
    ]


@pytest.mark.core
@pytest.mark.parametrize("name", sorted(REMOVED_FUNCTIONAL_NAMES))
def test_removed_names_are_absent(name):
    """Removed procedural names must not resolve or remain in ``__all__``."""
    assert "resolved" not in _SURFACE["removed"][name]
    assert name not in _SURFACE["all"]


@pytest.mark.core
def test_validation_utilities_remain_supported():
    """The procedural retirement must not remove validation utilities."""
    assert _SURFACE["utilities"] == {
        "check_forcing": True,
        "check_state": True,
    }
