from __future__ import annotations

from importlib.resources import files
import importlib.util
import shutil

import pytest
from suews_mcp import tools

pytestmark = pytest.mark.api


@pytest.mark.smoke
def test_validate_packaged_sample_config_smoke(tmp_path):
    if importlib.util.find_spec("supy") is None:
        pytest.skip("Requires installed supy package")
    if shutil.which("suews-validate") is None:
        pytest.skip("Requires suews-validate executable on PATH")

    sample = files("supy").joinpath("sample_data").joinpath("sample_config.yml")
    config = tmp_path / "sample_config.yml"
    config.write_text(sample.read_text(encoding="utf-8"), encoding="utf-8")

    result = tools.validate_config(tmp_path, "sample_config.yml", timeout_s=30)

    assert result["command"]["name"] == "suews-validate"
    assert "result" in result
