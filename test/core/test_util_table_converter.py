"""Behavioural tests for ``profile_manager.py`` and ``table.py`` converters."""

from __future__ import annotations

import logging
from pathlib import Path
import shutil

import f90nml
import numpy as np
import pandas as pd
import pytest

from supy.util.converter.table import table as table_mod
from supy.util.converter.table.profile_manager import (
    ProfileManager,
    validate_and_fix_profiles,
)

pytestmark = [pytest.mark.api, pytest.mark.contract("C-MIGRATE")]


@pytest.fixture
def supy_caplog(caplog):
    """Capture SuPy logger records even though SuPy defaults to propagate=False."""
    supy_logger = logging.getLogger("SuPy")
    original = supy_logger.propagate
    supy_logger.propagate = True
    try:
        yield caplog
    finally:
        supy_logger.propagate = original


_LEGACY_FORMAT_ROOT = Path(__file__).resolve().parents[1] / "fixtures" / "legacy_format"
_LEGACY_TABLES_ROOT = Path(__file__).resolve().parents[1] / "fixtures" / "legacy_tables"


def _write_modern_profiles(path: Path, codes: dict[int, float]) -> None:
    """Write a single-header SUEWS_Profiles.txt file."""
    lines = ["Code " + " ".join(str(h) for h in range(24))]
    for code, val in codes.items():
        vals = " ".join(f"{val:.3f}" for _ in range(24))
        lines.append(f"{code} {vals}")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def _write_legacy_profiles(path: Path, codes: dict[int, float]) -> None:
    """Write a two-line-header (legacy) SUEWS_Profiles.txt file."""
    lines = [
        " ".join(str(i) for i in range(1, 26)),
        "Code " + " ".join(str(h) for h in range(24)),
    ]
    for code, val in codes.items():
        vals = " ".join(f"{val:.3f}" for _ in range(24))
        lines.append(f"{code} {vals}")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def _write_simple_table(path: Path, headers: list[str], rows: list[list[str]]) -> None:
    """Write a minimal two-line-header SUEWS table with -9 footers."""
    col_nums = " ".join(str(i) for i in range(1, len(headers) + 1))
    lines = [col_nums, " ".join(headers)]
    lines.extend(" ".join(r) for r in rows)
    lines.extend(["-9", "-9"])
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


class TestProfileManager:
    """Tests for ``ProfileManager`` and ``validate_and_fix_profiles``."""

    def test_load_modern_and_legacy_profile_formats(self, tmp_path):
        """ProfileManager loads both single-header and legacy two-line-header files."""
        modern_path = tmp_path / "modern_profiles.txt"
        _write_modern_profiles(modern_path, {10: 0.5, 20: 1.5})
        pm_modern = ProfileManager(modern_path)
        assert pm_modern.available_profiles == {10, 20}
        assert pm_modern.profile_data is not None
        assert pm_modern.profile_data.loc[10].iloc[0] == pytest.approx(0.5)

        legacy_path = tmp_path / "legacy_profiles.txt"
        _write_legacy_profiles(legacy_path, {100: 0.8, 200: 1.2})
        pm_legacy = ProfileManager(legacy_path)
        assert pm_legacy.available_profiles == {100, 200}
        assert pm_legacy.profile_data is not None
        assert pm_legacy.profile_data.loc[200].iloc[0] == pytest.approx(1.2)

    def test_load_profiles_handles_malformed_file_gracefully(self, tmp_path):
        """A non-integer-indexed malformed file resets profile_data without crashing."""
        bad_path = tmp_path / "SUEWS_Profiles.txt"
        bad_path.write_text(
            "not_a_num col1\nheader col1\nstill_not_int 1.0\n",
            encoding="utf-8",
        )

        pm = ProfileManager(bad_path)
        assert pm.profile_data is None
        assert pm.available_profiles == set()

    def test_validate_profile_code_sentinels_existing_and_missing(self):
        """Special sentinels (-999, -9) and existing codes pass; missing codes return 999."""
        pm = ProfileManager()
        pm.available_profiles = {10, 20}

        assert pm.validate_profile_code(-999, "PopProfWD") == (True, -999)
        assert pm.validate_profile_code(-9, "PopProfWE") == (True, -9)
        assert pm.validate_profile_code(10, "EnergyUseProfWD") == (True, 10)
        assert pm.missing_profiles == set()

        # Missing code when 999 is absent tracks both the missing code and 999.
        assert pm.validate_profile_code(555, "TraffProfWD") == (False, 999)
        assert pm.missing_profiles == {555, 999}

        # When 999 already exists, only the missing code is tracked.
        pm2 = ProfileManager()
        pm2.available_profiles = {10, 999}
        assert pm2.validate_profile_code(777, "TraffProfWE") == (False, 999)
        assert pm2.missing_profiles == {777}

    def test_create_default_and_ensure_required_profiles_round_trip(self, tmp_path):
        """ensure_required_profiles writes all 24 hours for aligned tables and documents CSV str-column mismatch."""
        # Part 1: integer-column profile_data aligns with create_default_profile(range(24)),
        # so every generated profile (1, 321, 999) writes and reloads finite uniform 1.0 values.
        pm_aligned = ProfileManager()
        pm_aligned.profile_data = pd.DataFrame(
            [[0.75] * 24], index=pd.Index([50], name="Code"), columns=range(24)
        )
        pm_aligned.available_profiles = {50}
        default_ser = pm_aligned.create_default_profile(42)
        assert default_ser.name == 42
        assert list(default_ser.index) == list(range(24))
        assert (default_ser == 1.0).all()

        pm_aligned.validate_profile_code(321, "ActivityProfWD")
        aligned_dir = tmp_path / "out_aligned"
        aligned_dir.mkdir()
        pm_aligned.ensure_required_profiles(aligned_dir)

        written_aligned = aligned_dir / "SUEWS_Profiles.txt"
        assert written_aligned.is_file()
        # Code 0 (NO_ACTIVITY) is skipped (code > 0 guard); 1, 50, 321, 999 are present.
        assert pm_aligned.available_profiles == {1, 50, 321, 999}

        pm_reloaded_aligned = ProfileManager(written_aligned)
        assert pm_reloaded_aligned.available_profiles == {1, 50, 321, 999}
        assert pm_reloaded_aligned.profile_data is not None
        np.testing.assert_allclose(
            pm_reloaded_aligned.profile_data.loc[50].to_numpy(dtype=float),
            np.full(24, 0.75),
        )
        for gen_code in (1, 321, 999):
            vals = pm_reloaded_aligned.profile_data.loc[gen_code].to_numpy(dtype=float)
            assert np.all(np.isfinite(vals))
            np.testing.assert_allclose(vals, np.ones(24))

        # Part 2: loading from disk produces string columns ("0".."23"); existing profile 50
        # preserves all 24 values, while newly added rows currently reload as NaN due to the
        # pre-existing int-vs-str label mismatch in profile_manager.py:144,164 (reported in PR).
        src_path = tmp_path / "SUEWS_Profiles.txt"
        _write_legacy_profiles(src_path, {50: 0.75})
        pm = ProfileManager(src_path)
        pm.validate_profile_code(321, "ActivityProfWD")
        out_dir = tmp_path / "out_csv"
        out_dir.mkdir()
        pm.ensure_required_profiles(out_dir)

        pm_reloaded = ProfileManager(out_dir / "SUEWS_Profiles.txt")
        assert pm_reloaded.available_profiles == {1, 50, 321, 999}
        assert pm_reloaded.profile_data is not None
        np.testing.assert_allclose(
            pm_reloaded.profile_data.loc[50].to_numpy(dtype=float), np.full(24, 0.75)
        )
        for gen_code in (1, 321, 999):
            assert pm_reloaded.profile_data.loc[gen_code].isna().all()

        summary = pm.get_profile_summary()
        assert summary["available_profiles"] == 4
        assert summary["missing_profiles"] == 2
        assert summary["missing_codes"] == [321, 999]
        assert summary["default_profiles"] == ProfileManager.DEFAULT_PROFILES

    def test_process_conversion_rules_replaces_only_missing_add_profile_codes(self):
        """Only 'Add' rules on PROFILE_TYPES with missing integer codes are rewritten."""
        pm = ProfileManager()
        pm.available_profiles = {10, 999}

        rules_df = pd.DataFrame({
            "Action": ["Add", "Add", "Add", "Add", "Rename"],
            "Variable": [
                "EnergyUseProfWD",
                "TraffProfWD",
                "PopProfWD",
                "UnrelatedVar",
                "ActivityProfWD",
            ],
            "Value": ["10", "888", "not_an_int", "888", "888"],
        })

        updated = pm.process_conversion_rules(rules_df.copy())

        assert updated.loc[0, "Value"] == "10"  # valid code kept
        assert updated.loc[1, "Value"] == "999"  # missing code replaced
        assert updated.loc[2, "Value"] == "not_an_int"  # non-int ignored
        assert updated.loc[3, "Value"] == "888"  # non-profile variable ignored
        assert updated.loc[4, "Value"] == "888"  # non-Add action ignored
        assert pm.missing_profiles == {888}

    def test_validate_and_fix_profiles_with_and_without_missing_codes(self, tmp_path):
        """validate_and_fix_profiles backs up rules on fixes and skips backup when clean."""
        # Case 1: missing code -> creates rules.csv.bak and updates rules.csv.
        conv_dir = tmp_path / "case_missing"
        conv_dir.mkdir()
        _write_legacy_profiles(conv_dir / "SUEWS_Profiles.txt", {10: 1.0})
        rules_path = conv_dir / "rules.csv"
        pd.DataFrame({
            "Action": ["Add"],
            "Variable": ["SnowClearingProfWD"],
            "Value": ["404"],
        }).to_csv(rules_path, index=False)

        mgr = validate_and_fix_profiles(conv_dir, rules_path, debug=True)
        assert (conv_dir / "rules.csv.bak").is_file()
        updated_rules = pd.read_csv(rules_path)
        assert str(updated_rules.loc[0, "Value"]) == "999"
        assert 404 in mgr.missing_profiles
        assert 404 in mgr.available_profiles

        # Case 2: all codes valid -> no .bak created.
        clean_dir = tmp_path / "case_clean"
        clean_dir.mkdir()
        _write_legacy_profiles(clean_dir / "SUEWS_Profiles.txt", {10: 1.0, 999: 1.0})
        clean_rules = clean_dir / "rules.csv"
        pd.DataFrame({
            "Action": ["Add"],
            "Variable": ["SnowClearingProfWD"],
            "Value": ["10"],
        }).to_csv(clean_rules, index=False)

        mgr_clean = validate_and_fix_profiles(clean_dir, clean_rules, debug=False)
        assert not (clean_dir / "rules.csv.bak").exists()
        assert mgr_clean.missing_profiles == set()

        # Case 3: missing rules_path still writes SUEWS_Profiles.txt cleanly.
        norules_dir = tmp_path / "case_norules"
        norules_dir.mkdir()
        _write_legacy_profiles(norules_dir / "SUEWS_Profiles.txt", {10: 1.0})
        mgr_nr = validate_and_fix_profiles(norules_dir, norules_dir / "absent.csv")
        assert 999 in mgr_nr.available_profiles


class TestTableVersionDetectionAndHelpers:
    """Tests for version detection, file/column mutations, and path utilities in table.py."""

    def test_version_index_and_requires_grid_layout(self):
        """Version ordering helpers handle known, unknown, and None values."""
        assert table_mod._version_index(None) is None
        assert table_mod._version_index("1999z") is None
        assert table_mod._version_index("2016a") == 0
        assert table_mod._requires_grid_layout(None) is False
        assert table_mod._requires_grid_layout("unknown") is False
        assert table_mod._requires_grid_layout("2023a") is False
        assert table_mod._requires_grid_layout("2024a") is True
        assert table_mod._requires_grid_layout("2025a") is True

    @pytest.mark.parametrize(
        ("fixture_ver", "expected_set"),
        [
            ("2016a", {"2016a"}),
            ("2018a", {"2018a", "2018b"}),
            ("2018c", {"2018c", "2018b", "2018a"}),
            ("2019a", {"2019a"}),
            ("2020a", {"2020a", "2021a"}),
            ("2025a", {"2025a"}),
        ],
    )
    def test_detect_table_version_on_vendored_fixtures(self, fixture_ver, expected_set):
        """detect_table_version identifies vendored legacy_format snapshots."""
        fixture_dir = _LEGACY_FORMAT_ROOT / fixture_ver
        if not fixture_dir.is_dir():
            pytest.skip(f"legacy_format fixture {fixture_ver} not present")
        detected = table_mod.detect_table_version(fixture_dir)
        assert detected in expected_set

    def test_detect_table_version_synthetic_2024a_2023a_2017a_empty_and_absolute_path(
        self, tmp_path
    ):
        """detect_table_version distinguishes 2024a, 2023a, 2017a, absolute paths, and empty dirs."""
        # Empty directory -> None.
        empty_dir = tmp_path / "empty"
        empty_dir.mkdir()
        assert table_mod.detect_table_version(empty_dir) is None

        # Synthetic 2024a with SPARTACUS/GridLayout files and rslmethod/rsllevel/faimethod.
        dir_2024 = tmp_path / "v2024a"
        inp_2024 = dir_2024 / "Input"
        inp_2024.mkdir(parents=True)
        (dir_2024 / "RunControl.nml").write_text(
            '&RunControl\nFileInputPath="./Input/"\nrslmethod=2\nrsllevel=0\nfaimethod=0\n/\n',
            encoding="utf-8",
        )
        (inp_2024 / "SUEWS_SPARTACUS.nml").write_text(
            "&Spartacus\n/\n", encoding="utf-8"
        )
        (inp_2024 / "GridLayoutKc.nml").write_text(
            "&dim\nnlayer=3\n/\n", encoding="utf-8"
        )
        assert table_mod.detect_table_version(dir_2024) == "2024a"

        # Synthetic 2023a with absolute FileInputPath (H_maintain present, BaseT_HC absent).
        dir_2023 = tmp_path / "v2023a"
        inp_2023 = tmp_path / "abs_inp_2023"
        dir_2023.mkdir()
        inp_2023.mkdir()
        (dir_2023 / "RunControl.nml").write_text(
            f'&RunControl\nFileInputPath="{inp_2023.resolve().as_posix()}/"\n/\n',
            encoding="utf-8",
        )
        _write_simple_table(
            inp_2023 / "SUEWS_Irrigation.txt", ["Code", "H_maintain"], [["1", "0.5"]]
        )
        _write_simple_table(
            inp_2023 / "SUEWS_AnthropogenicEmission.txt", ["Code", "QF"], [["1", "10"]]
        )
        assert table_mod.detect_table_version(dir_2023) == "2023a"

        # Synthetic 2017a with malformed RunControl.nml falling back to Input/ subdir.
        dir_2017 = tmp_path / "v2017a"
        inp_2017 = dir_2017 / "Input"
        inp_2017.mkdir(parents=True)
        (dir_2017 / "RunControl.nml").write_text("not valid namelist", encoding="utf-8")
        _write_simple_table(
            inp_2017 / "SUEWS_AnthropogenicHeat.txt", ["Code", "AHMin"], [["1", "5"]]
        )
        _write_simple_table(
            inp_2017 / "SUEWS_Conductance.txt", ["Code", "gsModel"], [["1", "2"]]
        )
        assert table_mod.detect_table_version(dir_2017) == "2017a"

        # _check_columns_in_file edge cases: missing file and 1-line file.
        assert (
            table_mod._check_columns_in_file(tmp_path / "missing.txt", ["Code"])
            is False
        )
        short_file = tmp_path / "one_line.txt"
        short_file.write_text("only_one_line\n", encoding="utf-8")
        assert table_mod._check_columns_in_file(short_file, ["Code"]) is False

        # _check_nml_parameters edge cases: missing file, malformed nml, missing param.
        assert table_mod._check_nml_parameters(tmp_path, {"absent.nml": ["x"]}) is False
        bad_nml = tmp_path / "bad.nml"
        bad_nml.write_text("<<<broken>>>", encoding="utf-8")
        assert table_mod._check_nml_parameters(tmp_path, {"bad.nml": ["x"]}) is False

    def test_table_and_nml_crud_actions(self, tmp_path, supy_caplog):
        """rename_file, rename_var, delete_var, add_var, and read_suews_table behaviours."""
        tbl = tmp_path / "SUEWS_Test.txt"
        _write_simple_table(tbl, ["Code", "Alpha", "Beta"], [["1", "10", "20"]])

        # read_suews_table happy path and missing-file error.
        df = table_mod.read_suews_table(str(tbl))
        assert list(df.columns) == ["Code", "Alpha", "Beta"]
        assert df.iloc[0]["Alpha"] == "10"
        with pytest.raises(FileNotFoundError):
            table_mod.read_suews_table(str(tmp_path / "nonexistent.txt"))

        # rename_var: existing and missing column warning.
        table_mod.rename_var(str(tbl), "Alpha", None, "AlphaNew")
        assert list(table_mod.read_suews_table(str(tbl)).columns) == [
            "Code",
            "AlphaNew",
            "Beta",
        ]
        with supy_caplog.at_level(logging.WARNING, logger="SuPy"):
            table_mod.rename_var(str(tbl), "NoSuchCol", None, "X")
        assert any("NoSuchCol" in r.message for r in supy_caplog.records)

        # add_var: normal insert, duplicate column skip, out-of-range position skip.
        table_mod.add_var(str(tbl), "Gamma", 3, "99")
        df_after_add = table_mod.read_suews_table(str(tbl))
        assert list(df_after_add.columns) == ["Code", "AlphaNew", "Gamma", "Beta"]
        assert df_after_add.iloc[0]["Gamma"] == "99"

        table_mod.add_var(str(tbl), "Gamma", 2, "100")  # duplicate -> skipped
        assert len(table_mod.read_suews_table(str(tbl)).columns) == 4
        table_mod.add_var(str(tbl), "TooFar", 99, "0")  # beyond len -> skipped
        assert "TooFar" not in table_mod.read_suews_table(str(tbl)).columns

        # delete_var: existing and missing column.
        table_mod.delete_var(str(tbl), "Gamma", None, None)
        assert "Gamma" not in table_mod.read_suews_table(str(tbl)).columns
        table_mod.delete_var(str(tbl), "MissingCol", None, None)

        # rename_file: existing file and missing file (calls sys.exit).
        table_mod.rename_file(str(tbl), None, None, "SUEWS_Renamed.txt")
        assert (tmp_path / "SUEWS_Renamed.txt").is_file()
        assert not tbl.exists()
        with pytest.raises(SystemExit):
            table_mod.rename_file(
                str(tmp_path / "does_not_exist.txt"), None, None, "x.txt"
            )

        # Namelist actions: add (int, float, str, duplicate), rename (found/missing), delete (found/missing).
        nml_path = tmp_path / "RunControl.nml"
        nml_path.write_text("&RunControl\nOldKey = 1\n/\n", encoding="utf-8")
        table_mod.add_var(str(nml_path), "IntKey", None, "42")
        table_mod.add_var(str(nml_path), "FloatKey", None, "3.14")
        table_mod.add_var(str(nml_path), "StrKey", None, "hello")
        table_mod.add_var(str(nml_path), "IntKey", None, "99")  # duplicate warning
        nml_data = f90nml.read(str(nml_path))["runcontrol"]
        assert nml_data["intkey"] == 42
        assert nml_data["floatkey"] == pytest.approx(3.14)
        assert nml_data["strkey"] == "hello"

        table_mod.rename_var(str(nml_path), "OldKey", None, "NewKey")
        table_mod.rename_var(str(nml_path), "AbsentKey", None, "Ignored")
        nml_data = f90nml.read(str(nml_path))["runcontrol"]
        assert "newkey" in nml_data and "oldkey" not in nml_data

        table_mod.delete_var(str(nml_path), "NewKey", None, None)
        table_mod.delete_var(str(nml_path), "AbsentKey", None, None)
        assert "newkey" not in f90nml.read(str(nml_path))["runcontrol"]

    def test_path_helpers_and_dijkstra_edge_cases(self, tmp_path):
        """Path containment, collision suffixing, rewrite early returns, and unreachable dijkstra."""
        assert table_mod._is_absolute_path("") is False
        assert table_mod._is_absolute_path("/abs/path") is True
        assert table_mod._is_within_dir("a\\b\\..\\c", tmp_path) is True
        assert table_mod._is_within_dir("a\\..\\..\\escape", tmp_path) is False

        # _choose_relative_input_dir enters suffix=2 loop when leaf ("Input") collides with
        # raw_output and "Input_input" collides with source_input_dir (portable, no symlinks).
        out_dir = tmp_path / "base"
        out_dir.mkdir()
        chosen = table_mod._choose_relative_input_dir(
            "/ext/Input/",
            out_dir,
            raw_output="./Input/",
            source_input_dir=out_dir / "Input_input",
        )
        assert chosen == "./Input_input_2/"

        # _rewrite_external_fileinputpath early returns: missing file, empty nml, non-str value.
        assert (
            table_mod._rewrite_external_fileinputpath(tmp_path / "none.nml", tmp_path)
            is False
        )
        empty_nml = tmp_path / "empty.nml"
        empty_nml.write_text("", encoding="utf-8")
        assert table_mod._rewrite_external_fileinputpath(empty_nml, tmp_path) is False
        nonstr_nml = tmp_path / "nonstr.nml"
        nonstr_nml.write_text("&RunControl\nFileInputPath = 123\n/\n", encoding="utf-8")
        assert table_mod._rewrite_external_fileinputpath(nonstr_nml, tmp_path) is False

        # _build_file_list fallback when RunControl.nml is absent.
        norc_dir = tmp_path / "norc"
        norc_dir.mkdir()
        (norc_dir / "SUEWS_SiteSelect.txt").write_text("1\nCode\n", encoding="utf-8")
        file_list = table_mod._build_file_list(str(norc_dir), "2020a")
        assert ("", "SUEWS_SiteSelect.txt") in file_list

        # _read_nml_value on missing/malformed and missing key.
        assert table_mod._read_nml_value(tmp_path / "absent.nml", "k") is None
        assert table_mod._read_nml_value(nonstr_nml, "missing_key") is None

        # dijkstra returns inf when target is unreachable.
        assert table_mod.dijkstra([("a", "b", 1)], "a", "z") == float("inf")

    def test_converter_file_placeholders_and_error_wrapping(self, tmp_path):
        """SUEWS_Converter_file creates BiogenCO2/ESTM/default placeholders and skips missing nml."""
        # Missing BiogenCO2.txt -> creates placeholder with Code=31 then adds column.
        bio_path = tmp_path / "SUEWS_BiogenCO2.txt"
        table_mod.SUEWS_Converter_file(
            str(bio_path),
            [["Add", str(bio_path), "alpha", "2", "0.1"]],
        )
        df_bio = table_mod.read_suews_table(str(bio_path))
        assert list(df_bio.columns) == ["Code", "alpha"]
        assert df_bio.iloc[0]["Code"] == "31"

        # Missing ESTMCoefficients.txt -> creates placeholder with 12 standard codes.
        estm_path = tmp_path / "SUEWS_ESTMCoefficients.txt"
        table_mod.SUEWS_Converter_file(
            str(estm_path),
            [["Add", str(estm_path), "Surf_thick1", "2", "0.2"]],
        )
        df_estm = table_mod.read_suews_table(str(estm_path))
        assert len(df_estm) == 12
        assert "Surf_thick1" in df_estm.columns

        # Missing generic .txt -> creates default Code=800 placeholder.
        gen_path = tmp_path / "SUEWS_Generic.txt"
        table_mod.SUEWS_Converter_file(
            str(gen_path),
            [["Keep", str(gen_path), "-999", "-999", "-999"]],
        )
        assert table_mod.read_suews_table(str(gen_path)).iloc[0]["Code"] == "800"

        # Missing .nml and unknown extension return without creating files.
        missing_nml = tmp_path / "Missing.nml"
        table_mod.SUEWS_Converter_file(
            str(missing_nml), [["Keep", str(missing_nml), "-999", "-999", "-999"]]
        )
        assert not missing_nml.exists()

        missing_other = tmp_path / "Missing.dat"
        table_mod.SUEWS_Converter_file(
            str(missing_other), [["Keep", str(missing_other), "-999", "-999", "-999"]]
        )
        assert not missing_other.exists()

        # Unknown action inside SUEWS_Converter_file wraps error as RuntimeError.
        with pytest.raises(RuntimeError, match="Conversion failed"):
            table_mod.SUEWS_Converter_file(
                str(gen_path), [["Move", str(gen_path), "X", "1", "0"]]
            )

    def test_harvest_step_moves_warns_on_heterogeneous_codes_and_convert_utf8(
        self, tmp_path, supy_caplog
    ):
        """Heterogeneous SiteSelect move-carry warns and keeps first; convert_utf8 transcodes UTF-16."""
        move_dir = tmp_path / "move_test"
        move_dir.mkdir()
        # 2017a -> 2018a moves EnergyUseProfWD from SiteSelect to AnthropogenicHeat via AnthropogenicCode.
        _write_simple_table(
            move_dir / "SUEWS_SiteSelect.txt",
            ["Grid", "AnthropogenicCode", "EnergyUseProfWD"],
            [["1", "10", "101"], ["2", "10", "202"]],
        )
        with supy_caplog.at_level(logging.WARNING, logger="SuPy"):
            harvested = table_mod._harvest_step_moves(str(move_dir), "2017a", "2018a")

        assert any("move-carry" in r.message for r in supy_caplog.records)
        carried = [
            h
            for h in harvested
            if ("SUEWS_AnthropogenicHeat.txt", "EnergyUseProfWD") in h["targets"]
        ]
        assert carried and carried[0]["values"]["10"] == "101"

        # convert_utf8 transcodes a UTF-16 file cleanly into plain UTF-8.
        utf16_file = tmp_path / "utf16_table.txt"
        utf16_file.write_bytes("SUEWS table header\n1 2 3\n".encode("utf-16"))
        table_mod.convert_utf8(utf16_file)
        assert utf16_file.read_text(encoding="utf-8") == "SUEWS table header\n1 2 3\n"

    def test_convert_table_profile_validation_init_rename_and_gridlayout_matching(
        self, tmp_path
    ):
        """convert_table validates profiles, renames InitialConditions when multipleinitfiles=0, and ensures GridLayout."""
        fixture_2020 = _LEGACY_TABLES_ROOT / "2020a"
        if not (fixture_2020 / "RunControl.nml").is_file():
            pytest.skip("legacy_tables/2020a fixture not vendored")

        src_dir = tmp_path / "src_2020a"
        src_inp = src_dir / "Input"
        src_inp.mkdir(parents=True)
        for item in fixture_2020.iterdir():
            if item.is_file() and item.name != "RunControl.nml":
                shutil.copyfile(item, src_inp / item.name)

        # Use a custom FileCode ('TestSite') so GridLayoutKc.nml does not match and
        # triggers the non-matching GridLayout placeholder branch; MultipleInitFiles=0
        # triggers InitialConditionsTestSite1_2012.nml -> InitialConditionsTestSite_2012.nml.
        rc_text = (fixture_2020 / "RunControl.nml").read_text(encoding="utf-8")
        assert "FileCode='test'" in rc_text
        rc_text = rc_text.replace("FileCode='test'", "FileCode='TestSite'")
        (src_dir / "RunControl.nml").write_text(rc_text, encoding="utf-8")
        (src_inp / "InitialConditionsTestSite1_2012.nml").write_text(
            "&InitialConditions\n/\n", encoding="utf-8"
        )

        out_dir = tmp_path / "out_2024a"
        table_mod.convert_table(
            str(src_dir),
            str(out_dir),
            "2020a",
            "2024a",
            validate_profiles=True,
        )

        out_inp = out_dir / "Input"
        # InitialConditionsTestSite1_2012.nml renamed to InitialConditionsTestSite_2012.nml.
        assert (out_inp / "InitialConditionsTestSite_2012.nml").is_file()
        assert not (out_inp / "InitialConditionsTestSite1_2012.nml").exists()
        # Placeholder GridLayoutTestSite.nml created because only GridLayoutKc.nml existed.
        assert (out_inp / "GridLayoutTestSite.nml").is_file()
        assert "Placeholder GridLayout" in (
            out_inp / "GridLayoutTestSite.nml"
        ).read_text(encoding="utf-8")
        # Profile validation ensured SUEWS_Profiles.txt has default profile 999.
        pm = ProfileManager(out_inp / "SUEWS_Profiles.txt")
        assert 999 in pm.available_profiles

        # Same-version copy rejects missing RunControl.nml with FileNotFoundError.
        no_rc_dir = tmp_path / "missing_rc"
        no_rc_dir.mkdir()
        with pytest.raises(FileNotFoundError, match=r"RunControl\.nml not found"):
            table_mod.convert_table(
                str(no_rc_dir), str(tmp_path / "unused_out"), "2020a", "2020a"
            )
