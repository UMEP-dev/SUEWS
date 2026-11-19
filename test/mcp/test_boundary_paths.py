"""Filesystem regressions for MCP preflight checks (F-MCP-02..04)."""

import subprocess

import pytest
import yaml

pytestmark = pytest.mark.api


@pytest.fixture
def project(tmp_path, monkeypatch):
    root = tmp_path / "project"
    root.mkdir()
    monkeypatch.setenv("SUEWS_MCP_PROJECT_ROOT", str(root))
    return root


@pytest.fixture
def no_cli(monkeypatch):
    """Tripwire at the process boundary; all parsing and filesystem IO are real."""

    original_run = subprocess.run

    def forbidden(command, *args, **kwargs):
        if command[0] == "git":
            return original_run(command, *args, **kwargs)
        pytest.fail("Unsafe input reached the CLI subprocess")

    monkeypatch.setattr(subprocess, "run", forbidden)


@pytest.mark.parametrize("suffix", [".pkl", ".pickle", ".PKL", ".PICKLE"])
@pytest.mark.parametrize("alias", [False, True])
def test_pickle_rejected_before_cli(project, no_cli, suffix, alias):
    from suews_mcp.tools import convert_config

    source = project / ("state" + suffix)
    source.write_bytes(b"not deserialised")
    if alias:
        link = project / "state.csv"
        link.symlink_to(source)
        source = link
    result = convert_config(str(source), "out.yml", project_root=str(project))
    assert result["status"] == "error"
    assert "Pickle" in str(result["errors"])
    assert not (project / "out.yml").exists()


def test_pickle_named_alias_to_csv_rejected(project, no_cli):
    from suews_mcp.tools import convert_config

    (project / "state.csv").write_text("a,b\n1,2\n", encoding="utf-8")
    (project / "state.pkl").symlink_to(project / "state.csv")
    assert (
        convert_config("state.pkl", "out.yml", project_root=str(project))["status"]
        == "error"
    )


@pytest.mark.parametrize("tool", ["diagnose_run", "summarise_run", "compare_runs"])
@pytest.mark.parametrize(
    "child",
    [
        "diagnostics.json",
        "provenance.json",
        "nested/df_output.csv",
        "nested/df_output.parquet",
        "nested/Kc_SUEWS_2012.txt",
    ],
)
def test_run_children_rejected_before_cli(project, no_cli, tool, child):
    import suews_mcp.tools as tools

    run = project / "run"
    target = project.parent / "outside"
    target.write_text("private sentinel", encoding="utf-8")
    link = run / child
    link.parent.mkdir(parents=True, exist_ok=True)
    link.symlink_to(target)
    args = [str(run), str(run)] if tool == "compare_runs" else [str(run)]
    assert getattr(tools, tool)(*args, project_root=str(project))["status"] == "error"
    assert target.read_text(encoding="utf-8") == "private sentinel"


def test_dangling_diagnostics_target_rejected(project, no_cli):
    from suews_mcp.tools import diagnose_run

    run = project / "run"
    run.mkdir()
    target = project.parent / "not-created.json"
    (run / "diagnostics.json").symlink_to(target)
    assert diagnose_run(str(run), project_root=str(project))["status"] == "error"
    assert not target.exists()


@pytest.mark.parametrize("kind", ["provenance", "diagnostics"])
def test_run_resource_child_rejected(project, kind):
    from suews_mcp.resources.runs import read_run_resource

    run = project / "run"
    run.mkdir()
    target = project.parent / "private.json"
    target.write_text('{"private": true}', encoding="utf-8")
    (run / f"{kind}.json").symlink_to(target)
    result = read_run_resource("run", kind, project_root=str(project))
    assert result["status"] == "error"
    assert "payload" not in result["data"]


@pytest.mark.parametrize(
    "tool", ["inspect_config", "validate_config", "assess_readiness", "convert_config"]
)
@pytest.mark.parametrize(
    "shape", ["scalar", "list", "ref", "legacy", "symlink", "absolute"]
)
def test_forcing_escape_rejected_before_cli(project, no_cli, tool, shape):
    import suews_mcp.tools as tools

    outside = project.parent / "forcing.txt"
    outside.write_text("private header\n", encoding="utf-8")
    reference = "../forcing.txt"
    if shape == "absolute":
        reference = str(outside)
    if shape == "symlink":
        (project / "forcing.txt").symlink_to(outside)
        reference = "forcing.txt"
    value = ["safe.txt", reference] if shape == "list" else reference
    if shape == "ref":
        value = {"value": reference}
    control = (
        {"forcing_file": value} if shape == "legacy" else {"forcing": {"file": value}}
    )
    config = project / "config.yml"
    config.write_text(yaml.safe_dump({"model": {"control": control}}), encoding="utf-8")
    args = [str(config), "out.yml"] if tool == "convert_config" else [str(config)]
    assert getattr(tools, tool)(*args, project_root=str(project))["status"] == "error"


@pytest.mark.parametrize(
    "mode", ["relative", "absolute", "symlink", "child", "canonical_alias"]
)
def test_runcontrol_escape_rejected_before_cli(project, no_cli, mode):
    from suews_mcp.tools import convert_config

    outside = project.parent / "tables"
    outside.mkdir()
    reference = "../tables"
    if mode == "absolute":
        reference = str(outside)
    if mode == "symlink":
        (project / "Input").symlink_to(outside, target_is_directory=True)
        reference = "Input"
    if mode == "child":
        (project / "Input").mkdir()
        (project / "Input" / "SUEWS_SiteSelect.txt").symlink_to(outside / "missing.txt")
        reference = "Input"
    (project / "RunControl.nml").write_text(
        f"&runcontrol\n FileInputPath='{reference}'\n/\n", encoding="utf-8"
    )
    source = "RunControl.nml"
    if mode == "canonical_alias":
        source = "other.nml"
        (project / source).write_text(
            "&runcontrol\n fileinputpath='Input'\n/\n", encoding="utf-8"
        )
    assert (
        convert_config(source, "out.yml", project_root=str(project))["status"]
        == "error"
    )


def test_in_root_aliases_and_config_relative_forcing(project):
    from suews_mcp.backend import ProjectRoot
    from suews_mcp.resources.runs import read_run_resource

    data = project / "data"
    data.mkdir()
    (data / "forcing.txt").write_text("header\n", encoding="utf-8")
    run = project / "run"
    run.mkdir()
    (run / "alias").symlink_to(data, target_is_directory=True)
    (data / "cycle").symlink_to(run, target_is_directory=True)
    (data / "provenance.json").write_text('{"test": 1}', encoding="utf-8")
    (run / "provenance.json").symlink_to(data / "provenance.json")
    config = run / "config.yml"
    config.write_text(
        "model:\n  control:\n    forcing:\n      file: ../data/forcing.txt\n",
        encoding="utf-8",
    )
    root = ProjectRoot(project)
    root.check_config(config)
    assert root.check_tree(run) == run
    assert read_run_resource("run", "provenance", project_root=str(project))["data"][
        "payload"
    ] == {"test": 1}


@pytest.mark.parametrize("tool", ["diagnose_run", "summarise_run", "compare_runs"])
def test_real_cli_accepts_in_root_output_alias(project, tool):
    """Exercise real loaders/writers, not an invented success envelope."""
    import shutil
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
    import shutil
    from importlib.resources import as_file
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


def test_compare_second_run_cannot_escape(project, no_cli):
    from suews_mcp.tools import compare_runs

    first = project / "first"
    second = project / "second"
    first.mkdir()
    second.mkdir()
    (second / "df_output.csv").symlink_to(project.parent / "private.csv")
    assert (
        compare_runs(str(first), str(second), project_root=str(project))["status"]
        == "error"
    )


def test_spartacus_fileinputpath_override_rejected(project, no_cli):
    from suews_mcp.tools import convert_config

    (project / "Input").mkdir()
    (project / "RunControl.nml").write_text(
        "&runcontrol\n fileinputpath='Input'\n/\n", encoding="utf-8"
    )
    (project / "Input" / "SUEWS_SPARTACUS.nml").write_text(
        "&spartacus\n fileinputpath='../../outside'\n/\n", encoding="utf-8"
    )
    assert (
        convert_config("RunControl.nml", "out.yml", project_root=str(project))["status"]
        == "error"
    )


def test_versioned_conversion_cannot_relocate_parent_paths(project, no_cli):
    from suews_mcp.backend import ProjectRoot
    from suews_mcp.tools import convert_config

    case = project / "case"
    case.mkdir()
    (project / "Input").mkdir()
    config = case / "RunControl.nml"
    config.write_text("&runcontrol\n fileinputpath='../Input'\n/\n", encoding="utf-8")
    # Without relocation, this is a legitimate project-contained reference.
    ProjectRoot(project).check_runcontrol(config)
    result = convert_config(
        str(config), "out.yml", project_root=str(project), from_version="2025a"
    )
    assert result["status"] == "error"
    assert "parent-relative" in str(result["errors"])


def test_symlinked_run_subdirectory_rejected(project, no_cli):
    from suews_mcp.tools import summarise_run

    run = project / "run"
    run.mkdir()
    (run / "partitions").symlink_to(project.parent, target_is_directory=True)
    assert summarise_run(str(run), project_root=str(project))["status"] == "error"


def test_embedded_path_uses_effective_narrower_root(project, no_cli):
    from suews_mcp.tools import inspect_config

    case = project / "case"
    case.mkdir()
    (case / "config.yml").write_text(
        "model:\n  control:\n    forcing:\n      file: ../forcing.txt\n",
        encoding="utf-8",
    )
    assert inspect_config("config.yml", project_root=str(case))["status"] == "error"
