"""Path containment tests for ``convert_table`` and ``convert_to_yaml``.

Verifies that ``convert_table`` never writes converted tables outside ``toDir``
when ``RunControl.nml`` carries an external ``FileInputPath`` (absolute,
parent-relative ``../``, or Windows drive/UNC path), while leaving
``FileOutputPath`` untouched so downstream callers such as ``convert_to_yaml``
preserve the user's configured output destination.
"""

from pathlib import Path
import shutil

import f90nml
import pytest
import yaml

pytest.importorskip("supy")
from supy.util.converter import convert_table, convert_to_yaml
from supy.util.converter.table import table as table_mod

pytestmark = [pytest.mark.api, pytest.mark.contract("C-MIGRATE")]

_FIXTURE_ROOT = Path(__file__).resolve().parents[1] / "fixtures" / "legacy_tables"


def _build_run_dir(
    ver: str,
    src_dir: Path,
    *,
    tables_dir: Path,
    file_input_path: str,
    file_output_path: str,
) -> None:
    """Create a run directory from a vendored legacy fixture with custom paths."""
    fixture_src = _FIXTURE_ROOT / ver
    if not (fixture_src / "RunControl.nml").exists():
        pytest.skip(f"legacy table fixture {ver} not vendored under {_FIXTURE_ROOT}")

    src_dir.mkdir(parents=True, exist_ok=True)
    tables_dir.mkdir(parents=True, exist_ok=True)

    for item in fixture_src.iterdir():
        if item.is_file() and item.name != "RunControl.nml":
            shutil.copyfile(item, tables_dir / item.name)

    rc_text = (fixture_src / "RunControl.nml").read_text(encoding="utf-8")
    updated_text = rc_text.replace(
        'FileInputPath="./Input/"', f'FileInputPath="{file_input_path}"'
    ).replace('FileOutputPath="./Output/"', f'FileOutputPath="{file_output_path}"')
    assert file_input_path in updated_text
    assert file_output_path in updated_text
    (src_dir / "RunControl.nml").write_text(updated_text, encoding="utf-8")


def _snapshot_dir_bytes(root: Path) -> dict[Path, bytes]:
    """Capture exact bytes of every file under ``root``."""
    return {
        path.relative_to(root): path.read_bytes()
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


@pytest.mark.parametrize(
    ("from_ver", "to_ver", "input_folder", "output_folder"),
    [
        pytest.param("2020a", "2024a", "Input", "Output", id="chained-2020a-to-2024a"),
        pytest.param(
            "2020a", "2021a", "Input", "Output", id="single-step-2020a-to-2021a"
        ),
        pytest.param(
            "2020a", "2020a", "CustomInput", "CustomOutput", id="same-version-2020a"
        ),
    ],
)
def test_convert_table_rewrites_absolute_fileinputpath_and_preserves_fileoutputpath(
    tmp_path, from_ver, to_ver, input_folder, output_folder
):
    """Absolute FileInputPath is rewritten into toDir; FileOutputPath is untouched."""
    src_dir = tmp_path / "absrun"
    out_dir = tmp_path / "absout"
    src_input = src_dir / input_folder
    src_output = src_dir / output_folder
    abs_input = f"{src_input.resolve().as_posix()}/"
    abs_output = f"{src_output.resolve().as_posix()}/"

    _build_run_dir(
        from_ver,
        src_dir,
        tables_dir=src_input,
        file_input_path=abs_input,
        file_output_path=abs_output,
    )
    source_before = _snapshot_dir_bytes(src_dir)
    assert (Path(input_folder) / "SUEWS_SiteSelect.txt") in source_before

    convert_table(
        str(src_dir),
        str(out_dir),
        from_ver,
        to_ver,
        validate_profiles=False,
    )

    # Source tree is untouched and external FileOutputPath was not created.
    assert _snapshot_dir_bytes(src_dir) == source_before
    assert not src_output.exists()

    # Converted tables land inside toDir under the leaf name.
    converted_site_select = out_dir / input_folder / "SUEWS_SiteSelect.txt"
    assert converted_site_select.is_file()
    assert (
        converted_site_select.read_bytes()
        != source_before[Path(input_folder) / "SUEWS_SiteSelect.txt"]
    )
    assert not (out_dir / output_folder).exists()

    # Output RunControl.nml rewrites FileInputPath only and preserves FileOutputPath.
    out_rc_path = out_dir / "RunControl.nml"
    out_rc = f90nml.read(str(out_rc_path))["runcontrol"]
    assert out_rc["fileinputpath"] == f"./{input_folder}/"
    assert out_rc["fileoutputpath"] == abs_output
    assert (out_dir / out_rc["fileinputpath"]).resolve().is_relative_to(
        out_dir.resolve()
    )
    if to_ver == "2024a":
        assert "rslmethod" in out_rc
    if from_ver == to_ver:
        # Patching preserves comments from the source RunControl.nml.
        assert "!NEW" in out_rc_path.read_text(encoding="utf-8")


@pytest.mark.parametrize(
    ("from_ver", "to_ver"),
    [
        pytest.param("2020a", "2024a", id="chained-2020a-to-2024a"),
        pytest.param("2020a", "2020a", id="same-version-2020a"),
    ],
)
def test_convert_table_rewrites_parent_relative_fileinputpath(
    tmp_path, from_ver, to_ver
):
    """Relative ../ FileInputPath resolving outside toDir must not overwrite source."""
    shared_dir = tmp_path / "shared"
    shared_input = shared_dir / "CustomInput"
    src_dir = tmp_path / "run"
    out_dir = tmp_path / "converted"

    _build_run_dir(
        from_ver,
        src_dir,
        tables_dir=shared_input,
        file_input_path="../shared/CustomInput/",
        file_output_path="./Output/",
    )
    shared_before = _snapshot_dir_bytes(shared_dir)
    src_before = _snapshot_dir_bytes(src_dir)

    convert_table(
        str(src_dir),
        str(out_dir),
        from_ver,
        to_ver,
        validate_profiles=False,
    )

    assert _snapshot_dir_bytes(shared_dir) == shared_before
    assert _snapshot_dir_bytes(src_dir) == src_before

    converted_site_select = out_dir / "CustomInput" / "SUEWS_SiteSelect.txt"
    assert converted_site_select.is_file()
    assert (
        converted_site_select.read_bytes()
        != shared_before[Path("CustomInput") / "SUEWS_SiteSelect.txt"]
    )
    assert (out_dir / "Output").is_dir()

    out_rc = f90nml.read(str(out_dir / "RunControl.nml"))["runcontrol"]
    assert out_rc["fileinputpath"] == "./CustomInput/"
    assert out_rc["fileoutputpath"] == "./Output/"


def test_convert_table_avoids_same_leaf_collision_with_in_dir_fileoutputpath(tmp_path):
    """Rewritten FileInputPath avoids colliding with an in-toDir FileOutputPath."""
    external_input = tmp_path / "external" / "Output"
    src_dir = tmp_path / "run"
    out_dir = tmp_path / "converted"

    _build_run_dir(
        "2020a",
        src_dir,
        tables_dir=external_input,
        file_input_path=f"{external_input.resolve().as_posix()}/",
        file_output_path="./Output/",
    )
    external_before = _snapshot_dir_bytes(external_input)

    convert_table(
        str(src_dir),
        str(out_dir),
        "2020a",
        "2020a",
        validate_profiles=False,
    )

    assert _snapshot_dir_bytes(external_input) == external_before
    out_rc = f90nml.read(str(out_dir / "RunControl.nml"))["runcontrol"]
    assert out_rc["fileinputpath"] == "./Input/"
    assert out_rc["fileoutputpath"] == "./Output/"
    assert (out_dir / "Input" / "SUEWS_SiteSelect.txt").is_file()
    assert (out_dir / "Output").is_dir()
    assert list((out_dir / "Output").iterdir()) == []


def test_convert_table_same_version_preserves_relative_runcontrol_unchanged(tmp_path):
    """In-toDir relative paths leave RunControl.nml byte-for-byte unchanged."""
    src_dir = tmp_path / "relrun"
    out_dir = tmp_path / "relout"
    _build_run_dir(
        "2020a",
        src_dir,
        tables_dir=src_dir / "Input",
        file_input_path="./Input/",
        file_output_path="./Output/",
    )

    convert_table(str(src_dir), str(out_dir), "2020a", "2020a", validate_profiles=False)

    assert (out_dir / "Input" / "SUEWS_SiteSelect.txt").is_file()
    assert (out_dir / "Output").is_dir()
    assert (out_dir / "RunControl.nml").read_text(encoding="utf-8") == (
        src_dir / "RunControl.nml"
    ).read_text(encoding="utf-8")


def test_convert_table_debug_dir_keeps_external_fileinputpath_inside_to_dir(tmp_path):
    """debug_dir mode also keeps external FileInputPath inside toDir."""
    src_dir = tmp_path / "debugrun"
    out_dir = tmp_path / "debugout"
    debug_dir = tmp_path / "debug_steps"
    src_input = src_dir / "Input"
    src_output = src_dir / "Output"
    abs_output = f"{src_output.resolve().as_posix()}/"

    _build_run_dir(
        "2020a",
        src_dir,
        tables_dir=src_input,
        file_input_path=f"{src_input.resolve().as_posix()}/",
        file_output_path=abs_output,
    )
    source_before = _snapshot_dir_bytes(src_dir)

    convert_table(
        str(src_dir),
        str(out_dir),
        "2020a",
        "2024a",
        debug_dir=str(debug_dir),
        validate_profiles=False,
    )

    assert _snapshot_dir_bytes(src_dir) == source_before
    assert not src_output.exists()
    assert (out_dir / "Input" / "SUEWS_SiteSelect.txt").is_file()
    out_rc = f90nml.read(str(out_dir / "RunControl.nml"))["runcontrol"]
    assert out_rc["fileinputpath"] == "./Input/"
    assert out_rc["fileoutputpath"] == abs_output
    assert any(debug_dir.glob("step_*"))


def test_convert_table_rewrites_windows_fileinputpath_on_flat_run_dir(tmp_path):
    """Windows drive FileInputPath is rewritten into toDir even on POSIX hosts."""
    src_dir = tmp_path / "winrun"
    out_dir = tmp_path / "winout"
    win_input = r"C:\data\site\CustomInput\\"[:-1]
    win_output = r"C:\data\site\Output\\"[:-1]

    _build_run_dir(
        "2020a",
        src_dir,
        tables_dir=src_dir,
        file_input_path=win_input,
        file_output_path=win_output,
    )

    convert_table(str(src_dir), str(out_dir), "2020a", "2021a", validate_profiles=False)

    assert (out_dir / "CustomInput" / "SUEWS_SiteSelect.txt").is_file()
    out_rc = f90nml.read(str(out_dir / "RunControl.nml"))["runcontrol"]
    assert out_rc["fileinputpath"] == "./CustomInput/"
    assert out_rc["fileoutputpath"] == win_output


@pytest.mark.parametrize(
    ("raw_input", "expected"),
    [
        (r"C:\data\site\CustomInput" + "\\", "./CustomInput/"),
        ("C:/data/site/CustomInput/", "./CustomInput/"),
        (r"\\server\share\SiteInput" + "\\", "./SiteInput/"),
        (r"..\shared\CustomInput" + "\\", "./CustomInput/"),
        ("C:\\", "./Input/"),
        ("../shared/Input/", "./Input/"),
    ],
)
def test_rewrite_external_fileinputpath_windows_and_case_insensitive_lookup(
    tmp_path, raw_input, expected
):
    """Unit check for Windows/parent paths and case-insensitive RunControl lookup."""
    assert not table_mod._is_within_dir(raw_input, tmp_path)
    assert table_mod._is_within_dir("./Input/", tmp_path)

    rc_path = tmp_path / "runcontrol.nml"
    rc_path.write_text(
        "&RunControl\n"
        f'FileInputPath="{raw_input}"\n'
        'FileOutputPath="/keep/me/Output/"\n'
        "Tstep=300  !keep comment\n"
        "/\n",
        encoding="utf-8",
    )

    changed = table_mod._rewrite_external_fileinputpath(
        tmp_path / "RunControl.nml", tmp_path
    )
    assert changed is True

    patched = f90nml.read(str(rc_path))["runcontrol"]
    assert patched["fileinputpath"] == expected
    assert patched["fileoutputpath"] == "/keep/me/Output/"
    assert "!keep comment" in rc_path.read_text(encoding="utf-8")


def test_convert_to_yaml_preserves_absolute_fileoutputpath_and_source_tables(tmp_path):
    """convert_to_yaml keeps source tables untouched and preserves FileOutputPath."""
    src_dir = tmp_path / "yamlrun"
    src_input = src_dir / "Input"
    src_output = tmp_path / "external_results" / "Output"
    out_yml = tmp_path / "out" / "config.yml"
    out_yml.parent.mkdir(parents=True, exist_ok=True)

    _build_run_dir(
        "2020a",
        src_dir,
        tables_dir=src_input,
        file_input_path=f"{src_input.resolve().as_posix()}/",
        file_output_path=f"{src_output.resolve().as_posix()}/",
    )
    source_before = _snapshot_dir_bytes(src_dir)

    convert_to_yaml(
        str(src_dir / "RunControl.nml"),
        str(out_yml),
        from_ver="2020a",
        validate_profiles=False,
    )

    assert _snapshot_dir_bytes(src_dir) == source_before
    assert not src_output.exists()
    assert out_yml.is_file()

    payload = yaml.safe_load(out_yml.read_text(encoding="utf-8"))
    recorded_output_dir = payload["model"]["control"]["output"]["dir"]
    assert (out_yml.parent / recorded_output_dir).resolve() == src_output.resolve()
