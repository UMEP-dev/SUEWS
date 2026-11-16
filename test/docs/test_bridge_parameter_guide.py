"""Keep the guide's nested land-cover parent table tied to actual Rust fields."""

from pathlib import Path
import re

import pytest

pytestmark = pytest.mark.api
ROOT = Path(__file__).resolve().parents[2]


@pytest.mark.parametrize(
    "nested", ["OhmPrm", "SoilPrm", "BioCo2Prm", "LaiPrm", "WaterDistPrm"]
)
def test_nested_parent_table_matches_rust(nested):
    guide = (ROOT / "dev-ref/RUST_BRIDGE_ADD_PARAMETER.md").read_text(encoding="utf-8")
    row = re.search(rf"^\| `{nested}` \| (.+) \|$", guide, re.MULTILINE)
    assert row, f"No parent table row for {nested}"
    documented = set(re.findall(r"`(Lc\w+Prm)`", row[1]))
    actual = set()
    for path in (ROOT / "src/suews_bridge/src").glob("lc_*_prm.rs"):
        source = path.read_text(encoding="utf-8")
        if re.search(rf"pub \w+: {nested},", source):
            actual.add(re.search(r"pub struct (\w+)", source)[1])
    assert documented == actual
