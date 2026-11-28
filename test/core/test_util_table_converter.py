"""Behavioural tests for ``profile_manager.py`` and ``table.py`` converters."""

from __future__ import annotations

import logging
from pathlib import Path
import shutil

from click.testing import CliRunner
import f90nml
import numpy as np
import pandas as pd
import pytest

from supy._load import load_SUEWS_dict_ModConfig
from supy.cmd.table_converter import convert_table_cmd
from supy.data_model.core.config import SUEWSConfig
from supy.util.converter.table import table as table_mod
from supy.util.converter.table.profile_manager import (
    ProfileManager,
    validate_and_fix_profiles,
)
import supy.util.converter.table.reverse as reverse_mod

pytestmark = [pytest.mark.unit, pytest.mark.contract("C-MIGRATE")]


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
        """ensure_required_profiles preserves file-loaded profiles and writes finite 24-hour defaults."""
        # Part 1: real file-loading route when required default codes exist preserves
        # all 24 finite hourly values across write and reload.
        src_path = tmp_path / "SUEWS_Profiles.txt"
        _write_legacy_profiles(src_path, {1: 1.0, 50: 0.75, 999: 1.0})
        pm_loaded = ProfileManager(src_path)
        loaded_out = tmp_path / "out_loaded"
        loaded_out.mkdir()
        pm_loaded.ensure_required_profiles(loaded_out)

        pm_reloaded_loaded = ProfileManager(loaded_out / "SUEWS_Profiles.txt")
        assert pm_reloaded_loaded.available_profiles == {1, 50, 999}
        assert pm_reloaded_loaded.profile_data is not None
        np.testing.assert_allclose(
            pm_reloaded_loaded.profile_data.loc[50].to_numpy(dtype=float),
            np.full(24, 0.75),
        )
        for code in (1, 999):
            vals = pm_reloaded_loaded.profile_data.loc[code].to_numpy(dtype=float)
            assert np.all(np.isfinite(vals))
            np.testing.assert_allclose(vals, np.ones(24))

        # Part 2: default and placeholder profile generation (aligned integer hour
        # columns matching create_default_profile's range(24) index) writes finite
        # uniform 1.0 values across all 24 hours for codes 1, 321, and 999 while
        # skipping code 0 (NO_ACTIVITY).
        pm_gen = ProfileManager()
        pm_gen.profile_data = pd.DataFrame(
            [[0.75] * 24], index=pd.Index([50], name="Code"), columns=range(24)
        )
        pm_gen.available_profiles = {50}
        default_ser = pm_gen.create_default_profile(42)
        assert default_ser.name == 42
        assert list(default_ser.index) == list(range(24))
        assert (default_ser == 1.0).all()

        pm_gen.validate_profile_code(321, "ActivityProfWD")
        gen_dir = tmp_path / "out_generated"
        gen_dir.mkdir()
        pm_gen.ensure_required_profiles(gen_dir)

        written_gen = gen_dir / "SUEWS_Profiles.txt"
        assert written_gen.is_file()
        assert pm_gen.available_profiles == {1, 50, 321, 999}

        pm_reloaded_gen = ProfileManager(written_gen)
        assert pm_reloaded_gen.available_profiles == {1, 50, 321, 999}
        assert pm_reloaded_gen.profile_data is not None
        np.testing.assert_allclose(
            pm_reloaded_gen.profile_data.loc[50].to_numpy(dtype=float),
            np.full(24, 0.75),
        )
        for gen_code in (1, 321, 999):
            vals = pm_reloaded_gen.profile_data.loc[gen_code].to_numpy(dtype=float)
            assert np.all(np.isfinite(vals))
            np.testing.assert_allclose(vals, np.ones(24))

        summary = pm_gen.get_profile_summary()
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
            ("2018b", {"2018a", "2018b"}),
            ("2018c", {"2018c", "2018b", "2018a"}),
            ("2019a", {"2019a"}),
            ("2020a", {"2020a", "2021a"}),
            ("2021a", {"2021a"}),
            ("2024a", {"2024a"}),
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

    @pytest.mark.parametrize(
        ("nml_body", "gridlayout_name"),
        [
            pytest.param(
                '&RunControl\nFileInputPath="./Input/"\ndiagMethod=2\nLocalClimateMethod=0\nFAIMethod=0\n/\n',
                "GridLayoutLondon.nml",
                id="historical-2024a-keys-custom-gridlayout",
            ),
            pytest.param(
                '&RunControl\nFileInputPath="./Input/"\nrslmethod=2\nrsllevel=0\nfaimethod=0\n/\n',
                "GridLayoutCustom.nml",
                id="modern-2024a-keys-custom-gridlayout",
            ),
            pytest.param(
                '&RunControl\nFileInputPath="./Input/"\nrslmethod=2\nLocalClimateMethod=0\nFAIMethod=0\n/\n',
                "GridLayoutKc.nml",
                id="mixed-alias-2024a-keys",
            ),
            pytest.param(
                "&RunControl\ndiagMethod=2\nLocalClimateMethod=0\nFAIMethod=0\n/\n",
                "gridlayoutsitea.nml",
                id="omitted-fileinputpath-input-subdir-fallback-lowercase-gridlayout",
            ),
        ],
    )
    def test_detect_table_version_2024a_aliases_and_gridlayout_variants(
        self, tmp_path, nml_body, gridlayout_name
    ):
        """detect_table_version recognises both historical and post-#864 2024a sets."""
        dir_2024 = tmp_path / "site_2024a"
        inp_2024 = dir_2024 / "Input"
        inp_2024.mkdir(parents=True)
        (dir_2024 / "RunControl.nml").write_text(nml_body, encoding="utf-8")
        (inp_2024 / "SUEWS_SPARTACUS.nml").write_text(
            "&Spartacus\n/\n", encoding="utf-8"
        )
        (inp_2024 / gridlayout_name).write_text("&dim\nnlayer=3\n/\n", encoding="utf-8")
        _write_simple_table(
            inp_2024 / "SUEWS_Irrigation.txt", ["Code", "H_maintain"], [["1", "0.5"]]
        )
        _write_simple_table(
            inp_2024 / "SUEWS_AnthropogenicEmission.txt",
            ["Code", "BaseT_HC"],
            [["1", "10"]],
        )
        assert table_mod.detect_table_version(dir_2024) == "2024a"

    def test_detect_table_version_2024a_boundaries_and_conversion_chain(self, tmp_path):
        """2024a detection rejects incomplete sets, preserves 2021a/2025a, and aligns with convert_table."""

        def _make_base_2021a_dir(name: str, nml_body: str) -> tuple[Path, Path]:
            root = tmp_path / name
            inp = root / "Input"
            inp.mkdir(parents=True)
            (root / "RunControl.nml").write_text(nml_body, encoding="utf-8")
            _write_simple_table(
                inp / "SUEWS_Irrigation.txt", ["Code", "H_maintain"], [["1", "0.5"]]
            )
            _write_simple_table(
                inp / "SUEWS_AnthropogenicEmission.txt",
                ["Code", "BaseT_HC"],
                [["1", "10"]],
            )
            return root, inp

        # Partial RunControl keys (DiagMethod only, lacking faimethod/localclimatemethod)
        # must not match 2024a even when SPARTACUS and GridLayout files exist.
        partial_dir, partial_inp = _make_base_2021a_dir(
            "partial_2024a",
            '&RunControl\nFileInputPath="./Input/"\nDiagMethod=2\n/\n',
        )
        (partial_inp / "SUEWS_SPARTACUS.nml").write_text(
            "&Spartacus\n/\n", encoding="utf-8"
        )
        (partial_inp / "GridLayouttest.nml").write_text(
            "&dim\nnlayer=1\n/\n", encoding="utf-8"
        )
        assert table_mod.detect_table_version(partial_dir) == "2021a"

        # Missing GridLayout*.nml (or a directory masquerading as one) must not match 2024a.
        no_grid_dir, no_grid_inp = _make_base_2021a_dir(
            "missing_gridlayout",
            '&RunControl\nFileInputPath="./Input/"\ndiagMethod=2\nLocalClimateMethod=0\nFAIMethod=0\n/\n',
        )
        (no_grid_inp / "SUEWS_SPARTACUS.nml").write_text(
            "&Spartacus\n/\n", encoding="utf-8"
        )
        (no_grid_inp / "GridLayoutFake.nml").mkdir()
        assert table_mod.detect_table_version(no_grid_dir) == "2021a"

        # Missing SUEWS_SPARTACUS.nml (only pre-2024a Spartacus.nml) must not match 2024a.
        old_spart_dir, old_spart_inp = _make_base_2021a_dir(
            "old_spartacus_name",
            '&RunControl\nFileInputPath="./Input/"\ndiagMethod=2\nLocalClimateMethod=0\nFAIMethod=0\n/\n',
        )
        (old_spart_inp / "Spartacus.nml").write_text(
            "&Spartacus\n/\n", encoding="utf-8"
        )
        (old_spart_inp / "GridLayoutKc.nml").write_text(
            "&dim\nnlayer=3\n/\n", encoding="utf-8"
        )
        assert table_mod.detect_table_version(old_spart_dir) == "2021a"

        # 2025a takes precedence over 2024a even with historical 2024a RunControl keys.
        dir_2025, inp_2025 = _make_base_2021a_dir(
            "v2025a_with_legacy_keys",
            '&RunControl\nFileInputPath="./Input/"\ndiagMethod=2\nLocalClimateMethod=0\nFAIMethod=0\n/\n',
        )
        (inp_2025 / "SUEWS_SPARTACUS.nml").write_text(
            "&Spartacus\n/\n", encoding="utf-8"
        )
        (inp_2025 / "GridLayoutKc.nml").write_text(
            "&dim\nnlayer=3\n/\n", encoding="utf-8"
        )
        _write_simple_table(
            inp_2025 / "SUEWS_SiteSelect.txt",
            ["Grid", "n_buildings", "h_std"],
            [["1", "150", "5.0"]],
        )
        assert table_mod.detect_table_version(dir_2025) == "2025a"

        # Empty alias group in _check_nml_parameters returns False safely.
        assert (
            table_mod._check_nml_parameters(dir_2025, {"RunControl.nml": [()]}) is False
        )

        # Vendored 2024a converts cleanly to 2025a when driven by detect_table_version,
        # migrating historical diagMethod/LocalClimateMethod to canonical rslmethod/rsllevel.
        fixture_2024 = _LEGACY_FORMAT_ROOT / "2024a"
        assert fixture_2024.is_dir(), f"Missing vendored fixture: {fixture_2024}"
        detected_2024 = table_mod.detect_table_version(fixture_2024)
        assert detected_2024 == "2024a"
        out_2025 = tmp_path / "converted_2024a_to_2025a"
        table_mod.convert_table(
            str(fixture_2024),
            str(out_2025),
            detected_2024,
            "2025a",
            validate_profiles=False,
        )
        assert table_mod.detect_table_version(out_2025) == "2025a"
        out_rc = f90nml.read(str(out_2025 / "RunControl.nml"))["runcontrol"]
        assert out_rc["rslmethod"] == 2
        assert out_rc["rsllevel"] == 0
        assert "diagmethod" not in out_rc
        assert "localclimatemethod" not in out_rc

        # Forward conversion from 2020a -> 2024a produces a directory detected as 2024a.
        fixture_2020 = _LEGACY_TABLES_ROOT / "2020a"
        assert (fixture_2020 / "RunControl.nml").is_file(), (
            f"Missing vendored fixture: {fixture_2020}"
        )
        out_2024 = tmp_path / "converted_2020a_to_2024a"
        table_mod.convert_table(
            str(fixture_2020),
            str(out_2024),
            "2020a",
            "2024a",
            validate_profiles=False,
        )
        assert table_mod.detect_table_version(out_2024) == "2024a"

    def test_cli_auto_detect_2024a_and_2025a_historical_aliases_to_yaml(self, tmp_path):
        """CLI auto-detection converts historical 2024a/2025a fixtures to valid YAML preserving RSL settings."""
        runner = CliRunner()

        # 1. Vendored 2024a fixture (diagMethod=2, LocalClimateMethod=0, FAIMethod=0).
        fixture_2024 = _LEGACY_FORMAT_ROOT / "2024a"
        assert fixture_2024.is_dir(), f"Missing vendored fixture: {fixture_2024}"
        out_2024_yml = tmp_path / "detected_2024a.yml"
        res_2024 = runner.invoke(
            convert_table_cmd,
            ["-i", str(fixture_2024 / "RunControl.nml"), "-o", str(out_2024_yml)],
        )
        assert res_2024.exit_code == 0, res_2024.output
        assert "Detected version: 2024a" in res_2024.output
        cfg_2024 = SUEWSConfig.from_yaml(str(out_2024_yml))
        assert int(cfg_2024.model.physics.roughness_sublayer.value) == 2
        assert int(cfg_2024.model.physics.roughness_sublayer_level.value) == 0
        assert int(cfg_2024.model.physics.frontal_area_index.value) == 0

        # 2. Mutated historical 2024a copy with non-default LocalClimateMethod=1 verifies
        # legacy alias overrides dict_RunControl_default['rsllevel'] = 0.
        custom_2024 = tmp_path / "custom_2024a"
        shutil.copytree(fixture_2024, custom_2024)
        rc_custom = custom_2024 / "RunControl.nml"
        rc_text = rc_custom.read_text(encoding="utf-8")
        assert "LocalClimateMethod    = 0" in rc_text
        rc_custom.write_text(
            rc_text.replace("LocalClimateMethod    = 0", "LocalClimateMethod    = 1"),
            encoding="utf-8",
        )
        out_custom_yml = tmp_path / "custom_2024a.yml"
        res_custom = runner.invoke(
            convert_table_cmd,
            ["-i", str(rc_custom), "-o", str(out_custom_yml)],
        )
        assert res_custom.exit_code == 0, res_custom.output
        assert "Detected version: 2024a" in res_custom.output
        cfg_custom = SUEWSConfig.from_yaml(str(out_custom_yml))
        assert int(cfg_custom.model.physics.roughness_sublayer.value) == 2
        assert int(cfg_custom.model.physics.roughness_sublayer_level.value) == 1

        # 3. Vendored 2025a fixture (same-version path via _load.py alias normalisation).
        fixture_2025 = _LEGACY_FORMAT_ROOT / "2025a"
        assert fixture_2025.is_dir(), f"Missing vendored fixture: {fixture_2025}"
        out_2025_yml = tmp_path / "detected_2025a.yml"
        res_2025 = runner.invoke(
            convert_table_cmd,
            ["-i", str(fixture_2025 / "RunControl.nml"), "-o", str(out_2025_yml)],
        )
        assert res_2025.exit_code == 0, res_2025.output
        assert "Detected version: 2025a" in res_2025.output
        cfg_2025 = SUEWSConfig.from_yaml(str(out_2025_yml))
        assert int(cfg_2025.model.physics.roughness_sublayer.value) == 2
        assert int(cfg_2025.model.physics.roughness_sublayer_level.value) == 0

    def test_legacy_rsl_alias_precedence_in_loader_and_converter(self, tmp_path):
        """Canonical rslmethod/rsllevel take precedence over historical aliases when both appear."""
        rc_both = tmp_path / "both" / "RunControl.nml"
        rc_both.parent.mkdir()
        rc_both.write_text(
            "&RunControl\n"
            'FileInputPath="./Input/"\n'
            "diagMethod = 2\n"
            "rslmethod = 1\n"
            "LocalClimateMethod = 1\n"
            "rsllevel = 0\n"
            "/\n",
            encoding="utf-8",
        )
        loaded = load_SUEWS_dict_ModConfig(rc_both)
        assert loaded["rslmethod"] == 1
        assert loaded["rsllevel"] == 0
        assert "diagmethod" not in loaded
        assert "localclimatemethod" not in loaded

        # Same precedence in _migrate_legacy_runcontrol_keys.
        changed = table_mod._migrate_legacy_runcontrol_keys(rc_both)
        assert changed is True
        patched = f90nml.read(str(rc_both))["runcontrol"]
        assert patched["rslmethod"] == 1
        assert patched["rsllevel"] == 0
        assert "diagmethod" not in patched
        assert "localclimatemethod" not in patched

        # Legacy-only file preserves legacy values (including non-zero rsllevel).
        rc_legacy = tmp_path / "legacy_only" / "RunControl.nml"
        rc_legacy.parent.mkdir()
        rc_legacy.write_text(
            "&RunControl\n"
            'FileInputPath="./Input/"\n'
            "diagMethod = 2\n"
            "LocalClimateMethod = 1\n"
            "/\n",
            encoding="utf-8",
        )
        loaded_legacy = load_SUEWS_dict_ModConfig(rc_legacy)
        assert loaded_legacy["rslmethod"] == 2
        assert loaded_legacy["rsllevel"] == 1
        assert "diagmethod" not in loaded_legacy
        assert "localclimatemethod" not in loaded_legacy

        # Absent or empty namelist returns False cleanly.
        assert (
            table_mod._migrate_legacy_runcontrol_keys(tmp_path / "absent.nml") is False
        )
        empty_nml = tmp_path / "empty.nml"
        empty_nml.write_text("", encoding="utf-8")
        assert table_mod._migrate_legacy_runcontrol_keys(empty_nml) is False

    @pytest.mark.parametrize(
        "variant_name",
        [
            pytest.param("gridlayoutkc.nml", id="lowercase-gridlayout"),
            pytest.param("GRIDLAYOUTKC.NML", id="uppercase-gridlayout-ext"),
        ],
    )
    def test_cli_auto_detect_2024a_preserves_case_variant_gridlayout_geometry(
        self, tmp_path, variant_name
    ):
        """Case-variant GridLayout*.nml geometry survives auto-detected CLI conversion without placeholder substitution."""
        fixture_2024 = _LEGACY_FORMAT_ROOT / "2024a"
        assert fixture_2024.is_dir(), f"Missing vendored fixture: {fixture_2024}"

        site_dir = tmp_path / "site_case_variant"
        shutil.copytree(fixture_2024, site_dir)
        orig_grid = site_dir / "Input" / "GridLayoutKc.nml"
        grid_text = orig_grid.read_text(encoding="utf-8")
        assert "height = 0., 11., 15., 22." in grid_text
        distinctive_text = grid_text.replace(
            "height = 0., 11., 15., 22.",
            "height = 0., 13., 21., 34.",
        )
        orig_grid.unlink()
        (site_dir / "Input" / variant_name).write_text(
            distinctive_text, encoding="utf-8"
        )

        out_yml = tmp_path / "converted_case_variant.yml"
        runner = CliRunner()
        res = runner.invoke(
            convert_table_cmd,
            ["-i", str(site_dir / "RunControl.nml"), "-o", str(out_yml)],
        )
        assert res.exit_code == 0, res.output
        assert "Detected version: 2024a" in res.output

        cfg = SUEWSConfig.from_yaml(str(out_yml))
        heights = cfg.sites[0].properties.vertical_layers.height.value
        assert heights == pytest.approx([0.0, 13.0, 21.0, 34.0])
        assert int(cfg.model.physics.roughness_sublayer.value) == 2

    @pytest.mark.parametrize(
        ("mode", "rc_replacements", "expected_fwd", "expected_rev"),
        [
            pytest.param(
                "historical",
                {},
                {"rslmethod": 2, "rsllevel": 0},
                {"diagmethod": 2, "localclimatemethod": 0},
                id="historical-2024a-roundtrip",
            ),
            pytest.param(
                "canonical",
                {
                    "diagMethod            = 2": "rslmethod             = 2",
                    "LocalClimateMethod    = 0": "rsllevel              = 1",
                },
                {"rslmethod": 2, "rsllevel": 1},
                {"rslmethod": 2, "rsllevel": 1},
                id="canonical-2024a-roundtrip",
            ),
            pytest.param(
                "mixed",
                {
                    "diagMethod            = 2": "diagMethod            = 2\nrslmethod             = 1",
                    "LocalClimateMethod    = 0": "LocalClimateMethod    = 1\nrsllevel              = 0",
                },
                {"rslmethod": 1, "rsllevel": 0},
                {
                    "diagmethod": 2,
                    "rslmethod": 1,
                    "localclimatemethod": 1,
                    "rsllevel": 0,
                },
                id="mixed-2024a-roundtrip",
            ),
        ],
    )
    def test_2024a_runcontrol_roundtrip_preserves_spellings_and_values(
        self, tmp_path, mode, rc_replacements, expected_fwd, expected_rev
    ):
        """2024a -> 2025a -> 2024a preserves RunControl keys and values for historical, canonical, and mixed inputs."""
        fixture_2024 = _LEGACY_FORMAT_ROOT / "2024a"
        assert fixture_2024.is_dir(), f"Missing vendored fixture: {fixture_2024}"

        src_dir = tmp_path / f"src_{mode}"
        shutil.copytree(fixture_2024, src_dir)
        rc_path = src_dir / "RunControl.nml"
        rc_text = rc_path.read_text(encoding="utf-8")
        for old, new in rc_replacements.items():
            assert old in rc_text
            rc_text = rc_text.replace(old, new)
        rc_path.write_text(rc_text, encoding="utf-8")

        src_rc = f90nml.read(str(rc_path))["runcontrol"]
        extras = reverse_mod.capture_legacy_extras(src_dir, "2024a")

        fwd_dir = tmp_path / f"fwd_{mode}"
        table_mod.convert_table(
            str(src_dir), str(fwd_dir), "2024a", "2025a", validate_profiles=False
        )
        fwd_rc = f90nml.read(str(fwd_dir / "RunControl.nml"))["runcontrol"]
        for key, val in expected_fwd.items():
            assert fwd_rc[key] == val
        assert "diagmethod" not in fwd_rc
        assert "localclimatemethod" not in fwd_rc

        flat_dir = tmp_path / f"flat_{mode}"
        flat_dir.mkdir()
        for produced in list(fwd_dir.rglob("SUEWS_*.txt")) + list(
            fwd_dir.rglob("*.nml")
        ):
            shutil.copyfile(produced, flat_dir / produced.name)

        rev_dir = tmp_path / f"rev_{mode}"
        reverse_mod.reverse_convert_table(flat_dir, rev_dir, "2024a", extras)
        rev_rc = f90nml.read(str(rev_dir / "RunControl.nml"))["runcontrol"]

        assert set(rev_rc) == set(src_rc)
        for key, val in expected_rev.items():
            assert rev_rc[key] == val
        diffs = {
            k: (src_rc[k], rev_rc[k])
            for k in src_rc
            if str(src_rc[k]) != str(rev_rc[k])
        }
        assert not diffs, f"{mode} 2024a RunControl round-trip diffs: {diffs}"

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
