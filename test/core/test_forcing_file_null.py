"""Explicit ``model.control.forcing.file: null`` means runtime forcing (gh#1722).

A YAML whose forcing arrives through ``SUEWSSimulation.update_forcing()`` can
declare so by setting ``forcing.file`` to a bare ``null``. The contract pinned
here:

- explicit null: the validator skips the forcing-file checks and warns; the
  configuration loads on every public path and the simulation does not try to
  auto-load forcing; ``run()`` without forcing fails with a clear message;
  null plus ``update_forcing(df)`` runs;
- an omitted ``forcing.file`` key or a path that does not exist stays an error.
"""

from pathlib import Path
import shutil
import warnings

from conftest import SHORT_RUN_STEPS
import pytest
import yaml

from supy.data_model.core import SUEWSConfig
from supy.data_model.validation.pipeline.phase_a import (
    RUNTIME_FORCING_WARNING,
    annotate_missing_parameters,
    validate_forcing_data,
)
from supy.suews_sim import SUEWSSimulation

pytestmark = pytest.mark.api

NULL_WARNING_TEXT = "forcing.file is null"


def _write_config(tmp_path: Path, source: Path, mutate) -> Path:
    """Copy ``source`` beside its forcing file into ``tmp_path``, then mutate."""
    data = yaml.safe_load(source.read_text(encoding="utf-8"))
    forcing_name = data["model"]["control"]["forcing"]["file"]["value"]
    shutil.copy(source.parent / forcing_name, tmp_path / forcing_name)
    mutate(data["model"]["control"])
    path_config = tmp_path / "config.yml"
    path_config.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")
    return path_config


def _set_null(control: dict) -> None:
    control["forcing"]["file"] = None


def _drop_key(control: dict) -> None:
    del control["forcing"]["file"]


def _set_missing_path(control: dict) -> None:
    control["forcing"]["file"] = {"value": "does_not_exist.txt"}


def _run_phase_a(path_config: Path, tmp_path: Path):
    return annotate_missing_parameters(
        user_file=str(path_config),
        standard_file=str(path_config),
        uptodate_file=str(tmp_path / "updated.yml"),
        report_file=str(tmp_path / "report.txt"),
    )


@pytest.fixture
def null_config_path(tmp_path, short_sample_yaml_path):
    return _write_config(tmp_path, short_sample_yaml_path, _set_null)


# ---------------------------------------------------------------------------
# Validator (Phase A)
# ---------------------------------------------------------------------------


class TestPhaseA:
    @pytest.mark.core
    def test_explicit_null_warns_without_error(self, null_config_path, tmp_path):
        """Explicit null skips the file checks and reports a warning only."""
        report = _run_phase_a(null_config_path, tmp_path)

        codes = {issue.code: issue for issue in report.issues}
        assert "A.FORCING" not in codes
        warning = codes["A.FORCING.RUNTIME"]
        assert warning.severity == "WARNING"
        assert warning.yaml_path == "model.control.forcing.file"
        assert not report.has_errors
        text = (tmp_path / "report.txt").read_text(encoding="utf-8")
        assert RUNTIME_FORCING_WARNING in text
        assert "ACTION NEEDED" not in text.replace("NO ACTION NEEDED", "")
        # The null survives into the updated YAML Phase A writes.
        updated = yaml.safe_load((tmp_path / "updated.yml").read_text(encoding="utf-8"))
        assert updated["model"]["control"]["forcing"]["file"] is None

    @pytest.mark.parametrize(
        ("mutate", "message"),
        [
            (_drop_key, "Forcing file path not found"),
            (_set_missing_path, "Forcing file not found"),
        ],
        ids=["absent-key", "missing-file"],
    )
    def test_absent_key_or_missing_file_stays_an_error(
        self, tmp_path, short_sample_yaml_path, mutate, message
    ):
        path_config = _write_config(tmp_path, short_sample_yaml_path, mutate)

        report = _run_phase_a(path_config, tmp_path)

        forcing_issues = [i for i in report.issues if i.category == "FORCING"]
        assert [i.code for i in forcing_issues] == ["A.FORCING"]
        assert forcing_issues[0].severity == "ERROR"
        assert message in forcing_issues[0].message

    def test_forcing_off_suppresses_the_warning(self, null_config_path, tmp_path):
        """``--forcing off`` disables forcing checks, the null warning included."""
        report = annotate_missing_parameters(
            user_file=str(null_config_path),
            standard_file=str(null_config_path),
            forcing="off",
        )
        assert not [i for i in report.issues if i.category == "FORCING"]

    @pytest.mark.parametrize(
        ("control", "expected_errors"),
        [
            ({"forcing": {"file": None}}, 0),
            ({"forcing": {"file": {"value": None}}}, 0),
            # The explicit current key wins over the legacy one, even when null,
            # matching ModelControl._coerce_legacy_forcing_file.
            ({"forcing": {"file": None}, "forcing_file": "legacy.txt"}, 0),
            # A legacy null is lifted to forcing.file: null by the model too.
            ({"forcing_file": None}, 0),
            ({"forcing": {}}, 1),
            ({}, 1),
        ],
        ids=[
            "null",
            "refvalue-null",
            "null-beats-legacy",
            "legacy-null",
            "empty-block",
            "no-entry",
        ],
    )
    def test_validate_forcing_data_entry_shapes(
        self, tmp_path, control, expected_errors
    ):
        path_config = tmp_path / "config.yml"
        path_config.write_text(
            yaml.safe_dump({"model": {"control": control}}), encoding="utf-8"
        )

        forcing_errors, forcing_paths = validate_forcing_data(str(path_config))

        assert len(forcing_errors) == expected_errors
        assert forcing_paths is None


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------


class TestDataModel:
    def test_null_loads_and_round_trips_through_yaml(self, null_config_path, tmp_path):
        config = SUEWSConfig.from_yaml(str(null_config_path))
        assert config.model.control.forcing.file is None

        path_out = tmp_path / "roundtrip.yml"
        config.to_yaml(str(path_out))

        dumped = yaml.safe_load(path_out.read_text(encoding="utf-8"))
        assert dumped["model"]["control"]["forcing"]["file"] is None
        assert SUEWSConfig.from_yaml(str(path_out)).model.control.forcing.file is None

    def test_absent_key_keeps_the_default(self, tmp_path, short_sample_yaml_path):
        path_config = _write_config(tmp_path, short_sample_yaml_path, _drop_key)
        config = SUEWSConfig.from_yaml(str(path_config))
        assert config.model.control.forcing.file == "forcing.txt"


# ---------------------------------------------------------------------------
# SUEWSSimulation public paths
# ---------------------------------------------------------------------------


def _null_warnings(record) -> list:
    return [w for w in record if NULL_WARNING_TEXT in str(w.message)]


class TestSimulation:
    @pytest.mark.core
    def test_yaml_path_warns_and_skips_auto_load(self, null_config_path):
        with pytest.warns(UserWarning, match=NULL_WARNING_TEXT):
            sim = SUEWSSimulation(str(null_config_path))
        assert sim.forcing is None

    def test_dict_path_warns_and_skips_auto_load(self, null_config_path):
        data = yaml.safe_load(null_config_path.read_text(encoding="utf-8"))
        with pytest.warns(UserWarning, match=NULL_WARNING_TEXT):
            sim = SUEWSSimulation(data)
        assert sim.forcing is None

    def test_config_object_path_loads_without_forcing(self, null_config_path):
        config = SUEWSConfig.from_yaml(str(null_config_path))
        with warnings.catch_warnings(record=True) as record:
            warnings.simplefilter("always")
            sim = SUEWSSimulation(config)
        assert not _null_warnings(record)  # this path never auto-loads forcing
        assert sim.forcing is None
        assert sim.config.model.control.forcing.file is None

    def test_update_config_patch_sets_null(self, short_sample_sim):
        sim = short_sample_sim()
        sim.update_config({"model": {"control": {"forcing": {"file": None}}}})
        assert sim.config.model.control.forcing.file is None
        # A further patch keeps the null rather than reverting to the default.
        sim.update_config({"model": {"control": {"tstep": 300}}})
        assert sim.config.model.control.forcing.file is None

    def test_run_without_forcing_names_the_null(self, null_config_path):
        with pytest.warns(UserWarning, match=NULL_WARNING_TEXT):
            sim = SUEWSSimulation(str(null_config_path))
        with pytest.raises(RuntimeError, match="No forcing data loaded") as excinfo:
            sim.run()
        assert "forcing.file to null" in str(excinfo.value)

    @pytest.mark.core
    def test_null_plus_update_forcing_runs(self, null_config_path, short_sample_sim):
        df_forcing = short_sample_sim().forcing.df.iloc[:SHORT_RUN_STEPS].copy()
        with pytest.warns(UserWarning, match=NULL_WARNING_TEXT):
            sim = SUEWSSimulation(str(null_config_path))

        sim.update_forcing(df_forcing)
        output = sim.run(end_date=df_forcing.index[-1])

        assert len(output.df) > 0
