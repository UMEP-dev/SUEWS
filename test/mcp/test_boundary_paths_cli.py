"""Real-CLI filesystem boundary regressions for MCP tools (F-MCP-02..04).

Marked ``native`` + ``os_compat`` because these tests shell out to the
installed ``suews`` console command from the built wheel. Pure-unit preflight
tests live in ``test/mcp/test_boundary_paths.py``.
"""

from __future__ import annotations

from importlib.resources import as_file
import shutil

import pytest
import yaml

pytestmark = [
    pytest.mark.native,
    pytest.mark.os_compat,
    pytest.mark.e2e,
    pytest.mark.contract("C-ERROR"),
]


@pytest.fixture
def project(tmp_path, monkeypatch):
    root = tmp_path / "project"
    root.mkdir()
    monkeypatch.setenv("SUEWS_MCP_PROJECT_ROOT", str(root))
    return root


@pytest.mark.parametrize("tool", ["diagnose_run", "summarise_run", "compare_runs"])
def test_real_cli_accepts_in_root_output_alias(project, tool):
    """Exercise real loaders/writers, not an invented success envelope."""
    from suews_mcp import tools

    if shutil.which("suews") is None:
        pytest.skip("Real CLI requires a built supy installation and suews on PATH")
    run = project / "run"
    run.mkdir()
    data = project / "output.csv"
    data.write_text(
        "datetime,QN,QF,QH,QE,QS\n"
        "2012-01-01 00:00:00,100,0,30,40,30\n"
        "2012-01-01 01:00:00,110,0,35,45,30\n"
        "2012-01-01 02:00:00,120,0,40,50,30\n",
        encoding="utf-8",
    )
    (run / "df_output.csv").symlink_to(data)
    (run / "provenance.json").write_text("{}", encoding="utf-8")
    args = [str(run), str(run)] if tool == "compare_runs" else [str(run)]
    result = getattr(tools, tool)(*args, project_root=str(project))
    assert result["status"] in {"success", "warning"}, result
    if tool == "diagnose_run":
        assert (run / "diagnostics.json").is_file()


def test_real_inspect_config_relative_in_root_forcing(project):
    from suews_mcp.tools import inspect_config

    if shutil.which("suews") is None:
        pytest.skip("Real CLI requires a built supy installation and suews on PATH")
    from supy._env import trv_supy_module

    with as_file(trv_supy_module / "sample_data" / "sample_config.yml") as sample:
        config = yaml.safe_load(sample.read_text(encoding="utf-8"))
    nested = project / "configs"
    nested.mkdir()
    (project / "forcing.txt").write_text("iy id it imin QN\n", encoding="utf-8")
    config["model"]["control"]["forcing"]["file"] = "../forcing.txt"
    path = nested / "config.yml"
    path.write_text(yaml.safe_dump(config), encoding="utf-8")
    result = inspect_config(str(path), project_root=str(project))
    assert result["status"] in {"success", "warning"}, result
    assert result["data"]["forcing_summary"]["columns"] == [
        "iy",
        "id",
        "it",
        "imin",
        "QN",
    ]
