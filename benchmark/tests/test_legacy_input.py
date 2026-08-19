"""Unit tests for the legacy input staging logic (legacy_input.py).

Synthesises a minimal InputTables dir (RunControl + tables + forcing + IC nml)
and asserts the year-stamp + path-resolution convention is honoured.
"""
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import legacy_input as li  # noqa: E402

RUN_CONTROL = """&RunControl
NetRadiationChoice=3
FileCode='Kc'
FileInputPath="./UK_London_Input/"
FileOutputPath="./UK_London_Output/"
SkipHeaderSiteInfo=2
SkipHeaderMet=1
Tstep=300
z=49.6
/
"""

# A RunControl that explicitly sets a 5-min forcing resolution (300 s).
RUN_CONTROL_RES300 = RUN_CONTROL.replace("Tstep=300\n", "Tstep=300\nResolutionFilesIn=300\n")


def _make_tables(src: Path, run_control: str = RUN_CONTROL, year: int = 2011):
    src.mkdir(parents=True, exist_ok=True)
    (src / "RunControl.nml").write_text(run_control, encoding="utf-8")
    (src / f"InitialConditionsKc1_{year}.nml").write_text(
        "&InitialConditions\nTemp_C0=4.57\n/\n", encoding="utf-8"
    )
    for name in ("SiteSelect", "NonVeg", "Veg", "Water"):
        (src / f"SUEWS_{name}.txt").write_text(f"# {name}\n1 2 3\n", encoding="utf-8")
    (src / "Kc_data.txt").write_text(
        "iy id it imin qn\n2011 1 1 0 -999\n", encoding="utf-8"
    )


def test_parse_run_control_basic(tmp_path):
    (tmp_path / "RunControl.nml").write_text(RUN_CONTROL, encoding="utf-8")
    rc = li.parse_run_control(tmp_path / "RunControl.nml")
    assert rc.file_code == "Kc"
    assert rc.file_input_path == "./UK_London_Input/"
    assert rc.file_output_path == "./UK_London_Output/"
    assert rc.skip_header_met == 1
    # No ResolutionFilesIn key -> default 3600 s -> 60 min.
    assert rc.resolution_seconds == li.DEFAULT_RESOLUTION_SECONDS
    assert rc.resolution_minutes == 60


def test_parse_run_control_explicit_resolution(tmp_path):
    (tmp_path / "RunControl.nml").write_text(RUN_CONTROL_RES300, encoding="utf-8")
    rc = li.parse_run_control(tmp_path / "RunControl.nml")
    assert rc.resolution_seconds == 300
    assert rc.resolution_minutes == 5


def test_parse_run_control_missing_key_raises(tmp_path):
    (tmp_path / "RunControl.nml").write_text(
        "&RunControl\nNetRadiationChoice=3\n/\n", encoding="utf-8"
    )
    with pytest.raises(KeyError):
        li.parse_run_control(tmp_path / "RunControl.nml")


def test_detect_year_from_ic_nml(tmp_path):
    (tmp_path / "InitialConditionsKc1_2011.nml").write_text("x", encoding="utf-8")
    assert li.detect_year(tmp_path) == 2011


def test_forcing_and_output_filenames():
    assert li.forcing_filename("Kc", 2011, 60) == "Kc_2011_data_60.txt"
    assert li.output_filename("Kc", 1, 2011, 60) == "Kc_1_2011_SUEWS_60.txt"


def test_stage_run_layout(tmp_path):
    src = tmp_path / "InputTables"
    run = tmp_path / "run"
    _make_tables(src)
    info = li.stage_run(src, run)

    # RunControl is at the run-dir root (SUEWS reads it from CWD).
    assert (run / "RunControl.nml").is_file()

    input_dir = Path(info["input_path"])
    output_dir = Path(info["output_path"])
    assert input_dir.name == "UK_London_Input"
    assert output_dir.is_dir()

    # All SUEWS_*.txt tables are UNDER FileInputPath (the prior pilot bug was
    # leaving them in the run root, so SiteSelect was "missing").
    assert (input_dir / "SUEWS_SiteSelect.txt").is_file()
    assert (input_dir / "SUEWS_NonVeg.txt").is_file()
    # The IC namelist is staged too.
    assert (input_dir / "InitialConditionsKc1_2011.nml").is_file()

    # The forcing is year-stamped to <code>_<year>_data_<res>.txt and lives
    # under FileInputPath, NOT a bare Kc_data.txt.
    assert info["forcing_name"] == "Kc_2011_data_60.txt"
    assert (input_dir / "Kc_2011_data_60.txt").is_file()
    assert not (input_dir / "Kc_data.txt").is_file()

    assert info["year"] == 2011
    assert info["resolution_minutes"] == 60
    assert info["file_code"] == "Kc"


def test_stage_run_explicit_year_overrides(tmp_path):
    src = tmp_path / "InputTables"
    run = tmp_path / "run"
    _make_tables(src, year=2011)
    info = li.stage_run(src, run, year=2012)
    assert info["forcing_name"] == "Kc_2012_data_60.txt"
    assert (Path(info["input_path"]) / "Kc_2012_data_60.txt").is_file()


def test_stage_run_missing_run_control_raises(tmp_path):
    src = tmp_path / "InputTables"
    src.mkdir()
    (src / "InitialConditionsKc1_2011.nml").write_text("x", encoding="utf-8")
    with pytest.raises(FileNotFoundError):
        li.stage_run(src, tmp_path / "run")


# --- 2016a-era convention (grid-stamped name + tstep resolution + disaggregation)

# 2016a RunControl: Tstep=300 (5 min), no ResolutionFilesIn key.
RUN_CONTROL_2016A = """&RunControl
NetRadiationChoice=3
FileCode='Kc'
FileInputPath="./UK_London_Input/"
FileOutputPath="./UK_London_Output/"
SkipHeaderMet=1
Tstep=300
z=49.6
/
"""

# A SiteSelect with the legacy numbered + name headers, then one data row grid=1.
SITESELECT_2016A = """1\t2\t3
Grid\tYear\tlat
1\t2011\t51.51
"""


def _make_2016a_tables(src: Path, year: int = 2011):
    src.mkdir(parents=True, exist_ok=True)
    (src / "RunControl.nml").write_text(RUN_CONTROL_2016A, encoding="utf-8")
    (src / f"InitialConditionsKc1_{year}.nml").write_text("x", encoding="utf-8")
    (src / "SUEWS_SiteSelect.txt").write_text(SITESELECT_2016A, encoding="utf-8")
    for name in ("NonVeg", "Veg", "Water"):
        (src / f"SUEWS_{name}.txt").write_text(f"# {name}\n1 2 3\n", encoding="utf-8")
    # Hourly forcing: 1-line header + 2 hourly rows (imin always 0).
    (src / "Kc_data.txt").write_text(
        "iy id it imin qn\n"
        "2011 1 0 0 -999\n"
        "2011 1 1 0 -888\n",
        encoding="utf-8",
    )


def test_parse_run_control_reads_tstep(tmp_path):
    (tmp_path / "RunControl.nml").write_text(RUN_CONTROL_2016A, encoding="utf-8")
    rc = li.parse_run_control(tmp_path / "RunControl.nml")
    assert rc.tstep_seconds == 300
    assert rc.tstep_minutes == 5


def test_forcing_filename_gridstamped():
    assert li.forcing_filename_gridstamped("Kc", 1, 2011, 5) == "Kc1_2011_data_5.txt"


def test_detect_grid_skips_headers(tmp_path):
    (tmp_path / "SUEWS_SiteSelect.txt").write_text(SITESELECT_2016A, encoding="utf-8")
    assert li.detect_grid(tmp_path) == 1


def test_disaggregate_hourly_to_tstep_holds_values(tmp_path):
    src = tmp_path / "Kc_data.txt"
    src.write_text(
        "iy id it imin qn\n"
        "2011 1 0 0 100\n"
        "2011 1 1 0 200\n",
        encoding="utf-8",
    )
    dst = tmp_path / "out.txt"
    n = li.disaggregate_hourly_to_tstep(src, dst, tstep_minutes=5, skip_header=1)
    assert n == 24  # 2 hourly rows * 12 sub-steps
    lines = dst.read_text(encoding="utf-8").splitlines()
    assert lines[0] == "iy id it imin qn"  # header preserved
    # First 12 rows: hour 0, imin 0,5,...,55, qn held at 100.
    assert lines[1] == "2011 1 0 0 100"
    assert lines[2] == "2011 1 0 5 100"
    assert lines[12] == "2011 1 0 55 100"
    # Next block: hour 1, qn held at 200.
    assert lines[13] == "2011 1 1 0 200"


def test_disaggregate_rejects_non_hourly(tmp_path):
    src = tmp_path / "Kc_data.txt"
    src.write_text("iy id it imin qn\n2011 1 0 30 100\n", encoding="utf-8")
    with pytest.raises(ValueError):
        li.disaggregate_hourly_to_tstep(src, tmp_path / "o.txt", tstep_minutes=5, skip_header=1)


def test_stage_run_2016a_convention(tmp_path):
    src = tmp_path / "InputTables"
    run = tmp_path / "run"
    _make_2016a_tables(src)
    info = li.stage_run(src, run, convention="2016a")

    # Grid-stamped, tstep-resolution forcing name: Kc1_2011_data_5.txt.
    assert info["forcing_name"] == "Kc1_2011_data_5.txt"
    assert info["grid"] == 1
    assert info["resolution_minutes"] == 5
    assert info["disaggregated"] is True
    assert info["forcing_rows_written"] == 24  # 2 hourly rows * 12

    input_dir = Path(info["input_path"])
    assert (input_dir / "Kc1_2011_data_5.txt").is_file()
    # The bare hourly Kc_data.txt is NOT staged under the run name.
    assert not (input_dir / "Kc_data.txt").is_file()
    assert (input_dir / "SUEWS_SiteSelect.txt").is_file()


def test_stage_run_default_convention_unchanged(tmp_path):
    # The default (2020a) path must keep the historical year-stamped behaviour.
    src = tmp_path / "InputTables"
    run = tmp_path / "run"
    _make_tables(src)
    info = li.stage_run(src, run)  # default convention
    assert info["convention"] == "2020a"
    assert info["forcing_name"] == "Kc_2011_data_60.txt"
    assert info["disaggregated"] is False


# --- new helpers: split / completeness / terminator / SiteSelect trim ---

def test_split_forcing_by_year_groups_rows(tmp_path):
    f = tmp_path / "Kc_data.txt"
    f.write_text(
        "iy id it imin qn\n"
        "2011 1 0 0 1\n"
        "2011 1 1 0 2\n"
        "2012 1 0 0 3\n",
        encoding="utf-8",
    )
    split = li.split_forcing_by_year(f, skip_header=1)
    assert split["header"].startswith("iy id it imin")
    assert sorted(split["by_year"]) == [2011, 2012]
    assert len(split["by_year"][2011]) == 2
    assert len(split["by_year"][2012]) == 1


def test_year_is_complete_detects_hour_zero_start():
    # Starts at hour 0 -> complete.
    assert li.year_is_complete([["2012", "1", "0", "0", "5"]])
    # Starts at hour 1 (missing hour 0) -> incomplete (the KCL-2011 defect).
    assert not li.year_is_complete([["2011", "1", "1", "0", "5"]])
    assert not li.year_is_complete([])


def test_write_year_forcing_adds_terminator(tmp_path):
    dst = tmp_path / "Kc1_2012_data_5.txt"
    rows = [["2012", "1", "0", "0", "100"], ["2012", "1", "1", "0", "200"]]
    n = li.write_year_forcing("iy id it imin qn", rows, dst, tstep_minutes=5)
    assert n == 24  # 2 hourly * 12 sub-steps (terminator not counted)
    lines = dst.read_text(encoding="utf-8").splitlines()
    assert lines[0] == "iy id it imin qn"        # header
    assert lines[1] == "2012 1 0 0 100"          # first 5-min step
    assert lines[12] == "2012 1 0 55 100"        # last sub-step of hour 0
    # Terminator: a -9-leading row so the binary's `if (iv==-9) exit` fires.
    assert lines[-1].split()[0] == "-9"


def test_siteselect_helpers_filter_years():
    text = (
        "1\t2\t3\n"
        "Grid\tYear\tlat\n"
        "1\t2011\t51.5\n"
        "1\t2012\t51.5\n"
        "1\t2013\t51.5\n"
        "!\t1\tcomment\n"
    )
    assert li._siteselect_data_years(text) == [2011, 2012, 2013]
    trimmed = li.restrict_siteselect_to_years(text, {2012})
    assert li._siteselect_data_years(trimmed) == [2012]
    # Headers + comment row survive.
    assert "Grid\tYear\tlat" in trimmed
    assert "comment" in trimmed


def _make_2016a_multiyear(src: Path):
    """2016a tables with a multi-year forcing: 2011 incomplete, 2012 complete."""
    src.mkdir(parents=True, exist_ok=True)
    (src / "RunControl.nml").write_text(RUN_CONTROL_2016A, encoding="utf-8")
    (src / "InitialConditionsKc1_2011.nml").write_text("&IC\n/\n", encoding="utf-8")
    ss = (
        "1\t2\t3\n"
        "Grid\tYear\tlat\n"
        "1\t2011\t51.51\n"
        "1\t2012\t51.51\n"
    )
    (src / "SUEWS_SiteSelect.txt").write_text(ss, encoding="utf-8")
    for name in ("NonVeg", "Veg", "Water"):
        (src / f"SUEWS_{name}.txt").write_text(f"# {name}\n1 2 3\n", encoding="utf-8")
    # 2011 starts at hour 1 (incomplete); 2012 starts at hour 0 (complete).
    (src / "Kc_data.txt").write_text(
        "iy id it imin qn\n"
        "2011 1 1 0 -999\n"
        "2012 1 0 0 100\n"
        "2012 1 1 0 110\n",
        encoding="utf-8",
    )


def test_stage_run_2016a_skips_incomplete_year(tmp_path):
    src = tmp_path / "InputTables"
    run = tmp_path / "run"
    _make_2016a_multiyear(src)
    info = li.stage_run(src, run, convention="2016a")

    # 2011 (starts hour 1) skipped; 2012 (complete) staged.
    assert info["years_staged"] == [2012]
    assert 2011 in info["years_skipped"]
    input_dir = Path(info["input_path"])
    assert (input_dir / "Kc1_2012_data_5.txt").is_file()
    assert not (input_dir / "Kc1_2011_data_5.txt").is_file()
    # SiteSelect trimmed to the staged year only.
    trimmed = (input_dir / "SUEWS_SiteSelect.txt").read_text(encoding="utf-8")
    assert li._siteselect_data_years(trimmed) == [2012]
    # Per-year IC created for the staged year.
    assert (input_dir / "InitialConditionsKc1_2012.nml").is_file()


def test_stage_run_2016a_raises_when_no_complete_year(tmp_path):
    src = tmp_path / "InputTables"
    src.mkdir(parents=True)
    (src / "RunControl.nml").write_text(RUN_CONTROL_2016A, encoding="utf-8")
    (src / "InitialConditionsKc1_2011.nml").write_text("&IC\n/\n", encoding="utf-8")
    (src / "SUEWS_SiteSelect.txt").write_text(
        "1\t2\nGrid\tYear\n1\t2011\n", encoding="utf-8"
    )
    # Only year 2011, and it starts at hour 1 (incomplete).
    (src / "Kc_data.txt").write_text(
        "iy id it imin qn\n2011 1 1 0 -999\n", encoding="utf-8"
    )
    with pytest.raises(ValueError):
        li.stage_run(src, run := tmp_path / "run", convention="2016a")


def test_stage_run_2020a_copies_all_prestamped_forcings(tmp_path):
    src = tmp_path / "InputTables"
    run = tmp_path / "run"
    src.mkdir(parents=True)
    (src / "RunControl.nml").write_text(RUN_CONTROL, encoding="utf-8")  # Tstep=300, res default 3600
    (src / "InitialConditionstest1_2005.nml").write_text("x", encoding="utf-8")
    (src / "SUEWS_SiteSelect.txt").write_text("1 2\nGrid Year\n1 2004\n1 2005\n", encoding="utf-8")
    # FileCode in RUN_CONTROL is 'Kc'; pre-stamped per-year forcing at 60 min.
    (src / "Kc_2004_data_60.txt").write_text("h\n2004 1 0 0\n", encoding="utf-8")
    (src / "Kc_2005_data_60.txt").write_text("h\n2005 1 0 0\n", encoding="utf-8")
    info = li.stage_run(src, run, convention="2020a")
    input_dir = Path(info["input_path"])
    # BOTH years' forcing copied (the binary opens one file per year).
    assert (input_dir / "Kc_2004_data_60.txt").is_file()
    assert (input_dir / "Kc_2005_data_60.txt").is_file()
    assert set(info["forcing_files"]) == {"Kc_2004_data_60.txt", "Kc_2005_data_60.txt"}
