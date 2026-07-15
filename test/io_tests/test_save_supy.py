"""Test save_supy functionality with various output configurations."""

from pathlib import Path
import tempfile
from types import SimpleNamespace

import pandas as pd
import pytest

import supy as sp
from supy._filename import safe_filename_component
from supy._supy_module import _save_supy
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.model import OutputControl, OutputFormat
from supy.suews_checkpoint import SUEWSCheckpoint
from supy.suews_output import SUEWSOutput
from supy.suews_sim import SUEWSSimulation

pytestmark = pytest.mark.api


class TestSaveSuPy:
    """Test saving functionality of SuPy outputs."""

    @pytest.fixture(scope="class")
    def sample_output(self, sample_run_cached):
        """Sample output data for testing.

        72 steps (6 hours of the sample's 5-min forcing) for faster tests.
        Class-scoped: one copy is retrieved once for the class and the
        read-only ``(df_output, df_state_final)`` tuple is shared across
        every test in the class (each test writes to its own
        ``TemporaryDirectory``), backed by the shared session run cache.
        """
        return sample_run_cached(24 * 3)

    def test_save_default_groups(self, sample_output):
        """Test that default saving includes SUEWS and DailyState groups."""
        df_output, df_state_final = sample_output

        with tempfile.TemporaryDirectory() as tmpdir:
            # Save with default settings
            list_files = sp.save_supy(
                df_output, df_state_final, path_dir_save=tmpdir, site="test"
            )

            # Check that files were created
            assert len(list_files) > 0, "No files were created"

            # Check for SUEWS output file
            suews_files = [f for f in list_files if "SUEWS" in str(f)]
            assert len(suews_files) > 0, "No SUEWS output file was created"

            # Check that SUEWS file has content
            suews_file = Path(suews_files[0])
            assert suews_file.exists(), "SUEWS file does not exist"
            assert suews_file.stat().st_size > 0, "SUEWS file is empty"

            # Read and verify content
            with open(suews_file, "r", encoding="utf-8") as f:
                lines = f.readlines()
                assert len(lines) > 1, "SUEWS file has no data rows"
                # Check header contains expected columns
                header = lines[0]
                assert "Kdown" in header, "Missing Kdown in header"
                assert "QN" in header, "Missing QN in header"
                assert "Fcld" in header, "Fcld should be in output even if NaN"

    def test_save_with_nan_values(self, sample_output):
        """Test that saving works correctly even with NaN values in some variables."""
        df_output, df_state_final = sample_output

        # Verify that Fcld has NaN values (this is what was causing the issue)
        if "SUEWS" in df_output.columns.get_level_values("group"):
            fcld_data = df_output.xs(("SUEWS", "Fcld"), level=("group", "var"), axis=1)
            assert fcld_data.isna().all().all(), (
                "Expected Fcld to be all NaN for this test"
            )

        with tempfile.TemporaryDirectory() as tmpdir:
            # This should work despite NaN values
            list_files = sp.save_supy(
                df_output, df_state_final, path_dir_save=tmpdir, site="test"
            )

            # Verify SUEWS file was created
            suews_files = [f for f in list_files if "SUEWS" in str(f)]
            assert len(suews_files) > 0, (
                "SUEWS file should be created despite NaN values"
            )

    def test_save_output_groups_filter(self, sample_output):
        """Test filtering output groups using output_config."""
        df_output, df_state_final = sample_output

        with tempfile.TemporaryDirectory() as tmpdir:
            # Save only DailyState group
            # Note: Currently dict-based output_config doesn't support groups filtering
            # This would require using the OutputControl class from data_model
            # For now, we'll test that the default behavior works

            # Test default behavior (should include SUEWS)
            list_files = sp.save_supy(
                df_output, df_state_final, path_dir_save=tmpdir, site="test"
            )

            # Check that both SUEWS and state files were created
            assert len(list_files) >= 2, (
                "At least SUEWS and state files should be created"
            )

            # SUEWS file should be created by default
            suews_files = [f for f in list_files if "SUEWS" in str(f)]
            assert len(suews_files) > 0, "SUEWS file should be created by default"

    def test_resample_frequency(self, sample_output):
        """Test different resampling frequencies."""
        df_output, df_state_final = sample_output

        with tempfile.TemporaryDirectory() as tmpdir:
            # Save with 30-minute frequency
            list_files = sp.save_supy(
                df_output,
                df_state_final,
                path_dir_save=tmpdir,
                site="test",
                freq_s=1800,  # 30 minutes
            )

            # Check SUEWS file
            suews_files = [f for f in list_files if "SUEWS" in str(f)]
            assert len(suews_files) > 0, "SUEWS file should be created"

            # Verify filename contains correct frequency
            suews_filename = Path(suews_files[0]).name
            assert "_30.txt" in suews_filename, (
                f"Expected _30.txt in filename, got {suews_filename}"
            )

    def test_internal_save_honours_output_config_format(self, sample_output):
        """Test the internal save helper honours OutputControl.format by default."""
        df_output, df_state_final = sample_output

        with tempfile.TemporaryDirectory() as tmpdir:
            list_files = _save_supy(
                df_output,
                df_state_final,
                path_dir_save=tmpdir,
                site="test",
                output_config=OutputControl(format=OutputFormat.PARQUET),
            )

            parquet_files = [Path(f) for f in list_files if str(f).endswith(".parquet")]
            txt_files = [Path(f) for f in list_files if str(f).endswith(".txt")]

            assert parquet_files, (
                "_save_supy should honour OutputControl.format=parquet when no "
                "explicit output_format kwarg is supplied"
            )
            assert not txt_files, "Parquet save should not fall back to text output"

    @staticmethod
    def _make_output(sample_output, output_format):
        """Build a SUEWSOutput whose config requests ``output_format``."""
        df_output, df_state_final = sample_output
        config = SUEWSConfig()
        config.model.control.output.format = output_format
        return SUEWSOutput(
            df_output=df_output,
            df_state_final=df_state_final,
            config=config,
        )

    def test_output_save_honours_config_txt_format(self, sample_output):
        """SUEWSOutput.save() must respect config format=txt (gh#1451)."""
        output = self._make_output(sample_output, OutputFormat.TXT)

        with tempfile.TemporaryDirectory() as tmpdir:
            # No explicit format: must follow the config, not the old
            # hard-coded parquet default.
            list_files = output.save(path=tmpdir)

            txt_files = [Path(f) for f in list_files if str(f).endswith(".txt")]
            parquet_files = [Path(f) for f in list_files if str(f).endswith(".parquet")]

            assert txt_files, (
                "SUEWSOutput.save() should honour config format=txt when no "
                "explicit format is supplied"
            )
            assert not parquet_files, (
                "SUEWSOutput.save() must not override config format with parquet"
            )

    def test_output_save_honours_config_parquet_format(self, sample_output):
        """SUEWSOutput.save() must respect config format=parquet (gh#1451)."""
        output = self._make_output(sample_output, OutputFormat.PARQUET)

        with tempfile.TemporaryDirectory() as tmpdir:
            list_files = output.save(path=tmpdir)

            parquet_files = [Path(f) for f in list_files if str(f).endswith(".parquet")]
            txt_files = [Path(f) for f in list_files if str(f).endswith(".txt")]

            assert parquet_files, (
                "SUEWSOutput.save() should honour config format=parquet"
            )
            assert not txt_files, "Parquet save should not fall back to text output"

    def test_output_save_explicit_format_overrides_config(self, sample_output):
        """An explicit format kwarg still wins over the stored config."""
        output = self._make_output(sample_output, OutputFormat.TXT)

        with tempfile.TemporaryDirectory() as tmpdir:
            list_files = output.save(path=tmpdir, format="parquet")

            parquet_files = [Path(f) for f in list_files if str(f).endswith(".parquet")]
            assert parquet_files, (
                "An explicit format='parquet' must override config format=txt"
            )

    def test_output_save_uses_safe_site_name_for_checkpoint(self, sample_output):
        """The modern API must apply one safe token to every saved artifact."""
        df_output, df_state_final = sample_output
        output_control = OutputControl(format=OutputFormat.PARQUET)
        config = SimpleNamespace(
            model=SimpleNamespace(control=SimpleNamespace(output=output_control)),
            sites=[SimpleNamespace(name="grid no: 0")],
        )
        checkpoint = SUEWSCheckpoint.from_grid_states({0: {}})
        output = SUEWSOutput(
            df_output=df_output,
            df_state_final=df_state_final,
            config=config,
            checkpoint=checkpoint,
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            with pytest.warns(
                UserWarning,
                match="output files will use 'grid no_ 0' instead",
            ):
                list_files = output.save(path=tmpdir)

            names = {Path(path).name for path in list_files}
            assert "grid no_ 0_SUEWS_checkpoint.json" in names
            assert all(":" not in name for name in names)
            path_metadata = next(
                Path(path)
                for path in list_files
                if Path(path).name.endswith("_SUEWS_metadata.parquet")
            )
            assert pd.read_parquet(path_metadata).loc[0, "site"] == "grid no: 0"

    def test_simulation_save_uses_safe_site_name_for_checkpoint(self, sample_output):
        """The simulation API must apply the safe token to every artifact."""
        df_output, df_state_final = sample_output
        output_control = OutputControl(format=OutputFormat.PARQUET)
        config = SimpleNamespace(
            model=SimpleNamespace(control=SimpleNamespace(output=output_control)),
            sites=[SimpleNamespace(name="grid no: 0")],
        )
        simulation = SUEWSSimulation.__new__(SUEWSSimulation)
        simulation._run_completed = True
        simulation._df_output = df_output
        simulation._df_state_final = df_state_final
        simulation._checkpoint = SUEWSCheckpoint.from_grid_states({0: {}})
        simulation._config = config

        with tempfile.TemporaryDirectory() as tmpdir:
            with pytest.warns(
                UserWarning,
                match="output files will use 'grid no_ 0' instead",
            ):
                list_files = simulation.save(tmpdir, format="parquet")

            names = {Path(path).name for path in list_files}
            assert "grid no_ 0_SUEWS_checkpoint.json" in names
            assert all(":" not in name for name in names)
            path_metadata = next(
                Path(path)
                for path in list_files
                if Path(path).name.endswith("_SUEWS_metadata.parquet")
            )
            assert pd.read_parquet(path_metadata).loc[0, "site"] == "grid no: 0"

    def test_legacy_save_preserves_original_site_in_parquet_metadata(
        self, sample_output
    ):
        """The legacy API must not replace the semantic site identifier."""
        df_output, df_state_final = sample_output

        with tempfile.TemporaryDirectory() as tmpdir:
            with pytest.warns(UserWarning):
                list_files = sp.save_supy(
                    df_output,
                    df_state_final,
                    path_dir_save=tmpdir,
                    site="grid no: 0",
                    output_config=OutputControl(format=OutputFormat.PARQUET),
                )

            path_metadata = next(
                Path(path)
                for path in list_files
                if Path(path).name.endswith("_SUEWS_metadata.parquet")
            )
            assert pd.read_parquet(path_metadata).loc[0, "site"] == "grid no: 0"

    def test_save_sanitises_unsafe_site_name(self, sample_output):
        """A site name with filename-unsafe characters must not leak into paths.

        A colon in the site name is the NTFS Alternate Data Stream separator on
        Windows, which silently writes output into a hidden stream instead of a
        normal file (gh#1619). The saved paths must therefore be free of the
        unsafe characters and every file must have real content, on any OS.
        """
        df_output, df_state_final = sample_output

        with tempfile.TemporaryDirectory() as tmpdir:
            with pytest.warns(
                UserWarning,
                match="output files will use 'grid no_ 0' instead",
            ):
                list_files = sp.save_supy(
                    df_output,
                    df_state_final,
                    path_dir_save=tmpdir,
                    site="grid no: 0",
                )

            assert list_files, "No files were created"
            for path in list_files:
                name = Path(path).name
                # None of the Windows-unsafe characters may reach the filename.
                for ch in ':<>"/\\|?*':
                    assert ch not in name, (
                        f"Unsafe character {ch!r} leaked into filename {name!r}"
                    )
                # The sanitised site token must still be present as a prefix.
                assert "grid no_ 0" in name, (
                    f"Sanitised site token missing from {name!r}"
                )
                # And the file must actually hold data, not be a 0-byte stub
                # (the failure mode when data lands in an alternate stream).
                assert Path(path).stat().st_size > 0, f"{name!r} is empty"


class TestSafeFilenameComponent:
    """Unit tests for the filesystem-safety helper (gh#1619)."""

    def test_colon_replaced(self):
        # The reporter's case: a colon must not survive into the token.
        assert ":" not in safe_filename_component("grid no: 0")

    @pytest.mark.parametrize(
        "raw",
        ["a<b", "a>b", "a:b", 'a"b', "a/b", "a\\b", "a|b", "a?b", "a*b", "a\x01b"],
    )
    def test_all_unsafe_chars_replaced(self, raw):
        result = safe_filename_component(raw)
        for ch in '<>:"/\\|?*':
            assert ch not in result
        assert "\x01" not in result

    def test_empty_is_preserved(self):
        # An empty identifier means "no site prefix" and must stay empty.
        assert safe_filename_component("") == ""

    def test_degenerate_name_uses_fallback(self):
        # A name that reduces to nothing (only dots/spaces, which are stripped)
        # must fall back rather than vanish.
        assert safe_filename_component(". .") == "site"
        assert safe_filename_component("  ") == "site"

    def test_unsafe_chars_become_underscores_not_fallback(self):
        # Unsafe characters are replaced (not stripped), so a name of them is a
        # valid non-empty token and does not trigger the fallback.
        assert safe_filename_component("///") == "___"

    def test_trailing_dot_and_space_stripped(self):
        # Windows silently strips these; we strip them ourselves for stability.
        assert safe_filename_component("site .") == "site"

    @pytest.mark.parametrize(
        ("name", "expected"),
        [
            ("CON", "CON_"),
            ("nul", "nul_"),
            ("Com1", "Com1_"),
            ("LPT9", "LPT9_"),
            ("CON.txt", "CON_.txt"),
            ("COM¹", "COM¹_"),
            ("LPT³.tar.gz", "LPT³_.tar.gz"),
            ("CONIN$.txt", "CONIN$_.txt"),
            ("CON .txt", "CON _.txt"),
        ],
    )
    def test_reserved_device_names_guarded(self, name, expected):
        result = safe_filename_component(name)
        assert result == expected

    def test_safe_name_unchanged(self):
        # A name that is already safe must pass through untouched.
        assert safe_filename_component("KCL_London") == "KCL_London"
