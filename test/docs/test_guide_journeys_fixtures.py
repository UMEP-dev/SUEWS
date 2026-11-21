"""Base and derived fixture bytes must match their reviewed checksums."""

from copy import deepcopy
import hashlib
from pathlib import Path
import sys

import pytest
import yaml

_REPO_ROOT = Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from scripts.suews.guide_journeys.fixtures import fixture_bytes  # noqa: E402

pytestmark = pytest.mark.api


def test_all_fixture_checksums():
    """Verify every base and byte-derived variant without requiring docutils."""
    manifest = yaml.safe_load(
        (_REPO_ROOT / "test/docs/journeys/transition_guide.yml").read_text(
            encoding="utf-8"
        )
    )
    for name, spec in manifest["fixtures"].items():
        assert (
            hashlib.sha256(
                fixture_bytes(_REPO_ROOT, manifest["fixtures"], name)
            ).hexdigest()
            == spec["sha256"]
        )


@pytest.mark.parametrize("before", ["absent", "repeated", ""])
def test_fixture_patch_requires_one_nonempty_match(tmp_path, before):
    """Missing, ambiguous and empty byte patterns must fail before copying."""
    data = b"repeated repeated\n"
    (tmp_path / "base.yml").write_bytes(data)
    fixtures = {
        "base": {"path": "base.yml", "sha256": hashlib.sha256(data).hexdigest()},
        "variant": {
            "base": "base",
            "replacements": [{"before": before, "after": "replacement"}],
            "sha256": "unused",
        },
    }
    with pytest.raises(ValueError, match="exactly once"):
        fixture_bytes(tmp_path, fixtures, "variant")


def test_fixture_checksum_rejects_unreviewed_patch(tmp_path):
    """A unique replacement alone cannot certify unreviewed derived bytes."""
    data = b"original\n"
    (tmp_path / "base.yml").write_bytes(data)
    fixtures = {
        "base": {"path": "base.yml", "sha256": hashlib.sha256(data).hexdigest()}
    }
    fixtures["variant"] = {
        "base": "base",
        "replacements": [{"before": "original", "after": "changed"}],
        "sha256": fixtures["base"]["sha256"],
    }
    with pytest.raises(ValueError, match="checksum mismatch: variant"):
        fixture_bytes(tmp_path, fixtures, "variant")
    damaged = deepcopy(fixtures)
    damaged["base"]["sha256"] = "wrong"
    with pytest.raises(ValueError, match="checksum mismatch: base"):
        fixture_bytes(tmp_path, damaged, "variant")


def test_zero_area_control_changes_only_balanced_surface_fractions():
    """The control retains the empty destinations and changes only area fractions."""
    manifest = yaml.safe_load(
        (_REPO_ROOT / "test/docs/journeys/transition_guide.yml").read_text(
            encoding="utf-8"
        )
    )
    zero = yaml.safe_load(
        fixture_bytes(_REPO_ROOT, manifest["fixtures"], "zero-area-dev12")
    )
    positive = yaml.safe_load(
        fixture_bytes(_REPO_ROOT, manifest["fixtures"], "positive-area-dev12")
    )
    cover = positive["sites"][0]["properties"]["land_cover"]
    assert cover["bsoil"]["waterdist"] == {}
    assert cover["bsoil"]["sfr"]["value"] == 0.01
    assert cover["grass"]["sfr"]["value"] == 0.02
    cover["bsoil"]["sfr"]["value"] = 0.0
    cover["grass"]["sfr"]["value"] = 0.03
    assert positive == zero
