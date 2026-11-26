"""End-to-end input-guard regressions for standalone ``suews-engine`` and Fortran anthro."""

from copy import deepcopy
from datetime import datetime, timedelta
from importlib import import_module
from importlib.resources import files
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pandas as pd
from pyarrow import ipc
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig

_run_rust = import_module("supy._run_rust")
_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)

pytestmark = [
    pytest.mark.physics,
    pytest.mark.api,
    pytest.mark.contract("C-STRICT", "C-FORCING", "C-NATIVE-ERROR"),
]


def _dump_yaml(doc: dict) -> str:
    return yaml.dump(doc, Dumper=_YAML_DUMPER, sort_keys=False)


def _load_yaml(text: str) -> dict:
    return yaml.load(text, Loader=_YAML_LOADER)


@pytest.fixture(scope="module")
def _engine_guards_base(sample_config_loaded):
    """Cache resolved YAML template and sample forcing lines for this module."""
    resolved_base = _load_yaml(_run_rust.config_to_engine_yaml(sample_config_loaded))
    forcing_entry = resolved_base["model"]["control"]["forcing"]["file"]
    forcing_name = (
        forcing_entry["value"] if isinstance(forcing_entry, dict) else forcing_entry
    )
    assert isinstance(forcing_name, str) and forcing_name
    forcing_raw = (files("supy") / "sample_data" / forcing_name).read_text(
        encoding="utf-8"
    )
    forcing_lines = [line for line in forcing_raw.splitlines() if line.strip()]
    return {
        "resolved_base": resolved_base,
        "forcing_name": forcing_name,
        "forcing_lines": forcing_lines,
    }


def _prepare_engine_inputs(
    base_cache: dict,
    run_dir: Path,
    *,
    emissions: int = 2,
    forcing_rows: int = 4,
    forcing_mutator=None,
    override_emissions_raw: int | None = None,
    config_mutator=None,
) -> tuple[Path, dict]:
    """Write resolved YAML config and short forcing prefix into ``run_dir``."""
    resolved_doc = deepcopy(base_cache["resolved_base"])
    control = resolved_doc["model"]["control"]
    control["output"]["dir"] = str(run_dir)
    forcing_name = base_cache["forcing_name"]
    forcing_lines = base_cache["forcing_lines"]

    header = forcing_lines[0].split()
    rows = [line.split() for line in forcing_lines[1 : forcing_rows + 1]]
    if forcing_mutator is not None:
        header, rows = forcing_mutator(header, rows)

    last_row = rows[-1]
    last_ts = datetime(int(last_row[0]), 1, 1) + timedelta(
        days=int(last_row[1]) - 1,
        hours=int(last_row[2]),
        minutes=int(last_row[3]),
    )
    control["end_time"] = last_ts.strftime("%Y-%m-%d %H:%M:%S")
    resolved_doc["model"]["physics"]["emissions"] = {
        "value": override_emissions_raw
        if override_emissions_raw is not None
        else emissions
    }
    if config_mutator is not None:
        config_mutator(resolved_doc)

    forcing_text = "\n".join([" ".join(header), *(" ".join(row) for row in rows)])
    (run_dir / forcing_name).write_text(forcing_text + "\n", encoding="utf-8")

    config_path = run_dir / "sample_config.yml"
    config_path.write_text(_dump_yaml(resolved_doc), encoding="utf-8")
    return config_path, resolved_doc


def _read_arrow_columns(arrow_path: Path, columns: list[str]) -> dict[str, np.ndarray]:
    """Read selected columns from an Arrow IPC file and release the mmap."""
    with ipc.open_file(str(arrow_path)) as reader:
        table = reader.read_all().select(columns)
        return {
            name: table.column(name).to_numpy(zero_copy_only=False).copy()
            for name in columns
        }


@pytest.mark.parametrize(
    ("mode", "field", "mutation", "expected_reason", "check_public_config"),
    [
        pytest.param(
            "missing",
            "raincover",
            None,
            "is missing",
            False,
            id="missing-raincover",
        ),
        pytest.param(
            "null",
            "raincover",
            {"value": None},
            "is null",
            True,
            id="null-raincover",
        ),
        pytest.param(
            "malformed",
            "veg_type",
            {"value": 1.5},
            "must be an integer",
            True,
            id="malformed-veg_type",
        ),
    ],
)
@pytest.mark.parametrize("route", ["file", "buffer", "resume"])
def test_resolved_lumps_br02_failure_witnesses(
    tmp_path: Path,
    suews_engine_bin: Path,
    _engine_guards_base: dict,
    sample_forcing_parsed,
    mode: str,
    field: str,
    mutation,
    expected_reason: str,
    check_public_config: bool,
    route: str,
):
    """LUMPS witnesses cover missing/null/malformed across file, buffer, resume, and Python."""
    path, data = _prepare_engine_inputs(_engine_guards_base, tmp_path)
    site = data["sites"][0]
    site["gridiv"] = 27
    intact = _dump_yaml(data)
    if mode == "missing":
        del site["properties"]["lumps"][field]
    else:
        site["properties"]["lumps"][field] = mutation
    payload = _dump_yaml(data)
    expected_path = f"`sites[gridiv=27].properties.lumps.{field}`"

    if route == "file":
        path.write_text(payload, encoding="utf-8")
        result = subprocess.run(
            [str(suews_engine_bin), "run", str(path)],
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )
        assert result.returncode != 0
        assert expected_path in result.stderr, result.stderr
        assert expected_reason in result.stderr, result.stderr
        assert not (tmp_path / "suews_output.arrow").exists()
        if check_public_config:
            with pytest.raises((ValueError, TypeError)):
                SUEWSConfig.from_dict(data)
    else:
        _, forcing = sample_forcing_parsed(4)
        block = _run_rust._prepare_forcing_block(forcing)
        native = _run_rust._check_rust_available()
        run = native.run_suews
        args = (block[:2].ravel(order="C").tolist(), 2)
        if route == "resume":
            _, checkpoint, length, _ = run(intact, *args)
            assert length == 2
            run = native.run_suews_with_state
            args = (block[2:].ravel(order="C").tolist(), 2, checkpoint)
        assert run(intact, *args)[2] == 2
        with pytest.raises(RuntimeError) as exc:
            run(payload, *args)
        assert expected_path in str(exc.value), str(exc.value)
        assert expected_reason in str(exc.value), str(exc.value)


def test_lumps_default_and_supplied_vegetation_reach_the_energy_fluxes(
    short_sample_yaml_path: Path, sample_forcing_parsed
):
    """Compare complete valid outputs, then perturb only the vegetation selector."""
    source = _load_yaml(short_sample_yaml_path.read_text(encoding="utf-8"))
    props = source["sites"][0]["properties"]
    del props["lumps"]
    _, forcing = sample_forcing_parsed(288)

    def run():
        config = SUEWSConfig.from_dict(source)
        grid = int(config.sites[0].gridiv)
        output, _, _ = _run_rust.run_suews_rust(config, forcing, grid)
        return output

    default = run()
    props["lumps"] = {
        "raincover": 0.25,
        "rainmaxres": 0.25,
        "drainrt": 0.25,
        "veg_type": 1,
    }
    explicit = run()
    pd.testing.assert_frame_equal(default, explicit, check_exact=True)
    props["lumps"]["veg_type"] = 2
    alternative = run()
    for field in ("QHlumps", "QElumps"):
        baseline = default["SUEWS", field].to_numpy()
        changed = alternative["SUEWS", field].to_numpy()
        for values in (baseline, changed):
            assert values.shape == (len(forcing),)
            assert np.isfinite(values).all() and (values != -999).all()
        assert (changed != baseline).any()


@pytest.mark.parametrize(
    ("mutator", "expected_substrings", "check_public_config"),
    [
        pytest.param(
            lambda d: d["model"]["physics"].pop("emissions", None),
            ("model.physics.emissions",),
            False,
            id="missing-emissions",
        ),
        pytest.param(
            lambda d: d["model"]["physics"].__setitem__("emissions", {"value": None}),
            ("model.physics.emissions",),
            True,
            id="null-emissions",
        ),
        pytest.param(
            lambda d: d["model"]["physics"].__setitem__("emissions", {"value": 17}),
            ("model.physics.emissions", "17"),
            True,
            id="malformed-emissions-hole-17",
        ),
    ],
)
def test_engine_and_python_reject_invalid_emissions_selector(
    tmp_path: Path,
    suews_engine_bin: Path,
    _engine_guards_base: dict,
    sample_forcing_parsed,
    mutator,
    expected_substrings: tuple[str, ...],
    check_public_config: bool,
):
    """BR-02 witnesses for model.physics.emissions across Python and suews-engine."""
    config_path, mutated_doc = _prepare_engine_inputs(
        _engine_guards_base,
        tmp_path,
        emissions=2,
        config_mutator=mutator,
    )

    result = subprocess.run(
        [str(suews_engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode != 0, "invalid emissions must not exit 0"
    for needle in expected_substrings:
        assert needle in result.stderr, result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()

    _, forcing = sample_forcing_parsed(2)
    block = _run_rust._prepare_forcing_block(forcing)
    native = _run_rust._check_rust_available()
    with pytest.raises(RuntimeError) as exc:
        native.run_suews(_dump_yaml(mutated_doc), block.ravel(order="C").tolist(), 2)
    for needle in expected_substrings:
        assert needle in str(exc.value), str(exc.value)

    if check_public_config:
        with pytest.raises((ValueError, TypeError)):
            SUEWSConfig.from_dict(mutated_doc)


@pytest.mark.parametrize("emissions", [2, 12])
def test_engine_accepts_valid_emissions_including_co2_methods(
    tmp_path: Path,
    suews_engine_bin: Path,
    _engine_guards_base: dict,
    emissions: int,
):
    """Valid modelled heat (2) and biogenic CO2 (>=11) emissions codes pass."""
    config_path, _ = _prepare_engine_inputs(
        _engine_guards_base,
        tmp_path,
        emissions=emissions,
        forcing_rows=2,
    )

    result = subprocess.run(
        [str(suews_engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )

    assert result.returncode == 0, (
        f"emissions={emissions} failed ({result.returncode}): {result.stderr}"
    )
    arrow_path = tmp_path / "suews_output.arrow"
    assert arrow_path.is_file()
    cols = _read_arrow_columns(arrow_path, ["QF", "QH", "Fc"])
    assert np.isfinite(cols["QF"]).all()
    assert np.isfinite(cols["QH"]).all()
    assert np.isfinite(cols["Fc"]).all()
    assert np.all(cols["QF"] > -900.0)


@pytest.mark.parametrize(
    "missing_mode", ["omitted", "all_sentinel", "interior_sentinel"]
)
def test_engine_rejects_missing_observed_qf_for_emissions_zero(
    tmp_path: Path,
    suews_engine_bin: Path,
    _engine_guards_base: dict,
    missing_mode: str,
):
    """Standalone ``suews-engine`` fails loudly when emissions=0 lacks valid qf."""

    def mutate_forcing(header: list[str], rows: list[list[str]]):
        qf_idx = next(i for i, col in enumerate(header) if col.lower() == "qf")
        if missing_mode == "omitted":
            new_header = [c for i, c in enumerate(header) if i != qf_idx]
            new_rows = [
                [val for i, val in enumerate(row) if i != qf_idx] for row in rows
            ]
            return new_header, new_rows
        if missing_mode == "all_sentinel":
            for row in rows:
                row[qf_idx] = "-999"
            return header, rows
        # interior_sentinel: valid neighbours surrounding an hourly -999 gap.
        for idx, row in enumerate(rows):
            row[qf_idx] = "-999" if idx == 1 else "25.0"
        return header, rows

    config_path, _ = _prepare_engine_inputs(
        _engine_guards_base,
        tmp_path,
        emissions=0,
        forcing_rows=4,
        forcing_mutator=mutate_forcing,
    )

    result = subprocess.run(
        [str(suews_engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )

    assert result.returncode != 0, f"emissions=0 ({missing_mode}) must fail loudly"
    assert "forcing column 'qf'" in result.stderr
    assert "emissions=0" in result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()


def test_python_simulation_rejects_missing_observed_qf_for_emissions_zero(
    sample_config_loaded, sample_forcing_parsed
):
    """Public ``SUEWSSimulation.run`` rejects missing/sentinel qf forcing under emissions=0."""
    cfg = sample_config_loaded.model_copy(deep=True)
    cfg.model.physics.emissions = 0
    _, df_forcing = sample_forcing_parsed(12)
    df_forcing["qf"] = -999.0

    sim = SUEWSSimulation()
    sim.update_config(cfg, auto_load_forcing=False)
    sim.update_forcing(df_forcing)
    with pytest.raises(RuntimeError) as exc:
        sim.run(
            start_date=df_forcing.index[0],
            end_date=df_forcing.index[-1],
            n_jobs=1,
            _validate_forcing=False,
        )
    assert "forcing column 'qf'" in str(exc.value), str(exc.value)
    assert "emissions=0" in str(exc.value), str(exc.value)


def test_engine_accepts_valid_observed_qf_for_emissions_zero(
    tmp_path: Path, suews_engine_bin: Path, _engine_guards_base: dict
):
    """Standalone ``suews-engine`` honours valid observed qf when emissions=0."""

    def set_valid_qf(header: list[str], rows: list[list[str]]):
        qf_idx = next(i for i, col in enumerate(header) if col.lower() == "qf")
        for row in rows:
            row[qf_idx] = "25.0"
        return header, rows

    config_path, _ = _prepare_engine_inputs(
        _engine_guards_base,
        tmp_path,
        emissions=0,
        forcing_rows=3,
        forcing_mutator=set_valid_qf,
    )

    result = subprocess.run(
        [str(suews_engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )

    assert result.returncode == 0, (
        f"emissions=0 with valid qf failed ({result.returncode}): {result.stderr}"
    )
    arrow_path = tmp_path / "suews_output.arrow"
    assert arrow_path.is_file()
    cols = _read_arrow_columns(arrow_path, ["QF", "QH"])
    np.testing.assert_allclose(cols["QF"], 25.0, rtol=0.0, atol=1e-12)
    assert np.isfinite(cols["QH"]).all()
    assert np.all(cols["QH"] > -200.0)


def test_engine_does_not_interpolate_sentinel_into_corrupted_floats(
    tmp_path: Path, suews_engine_bin: Path, _engine_guards_base: dict
):
    """An interior -999 in hourly forcing must stay -999, never blend into valid data."""

    def inject_fcld_sentinel(header: list[str], rows: list[list[str]]):
        fcld_idx = next(i for i, col in enumerate(header) if col.lower() == "fcld")
        values = ["0.2", "-999", "0.8", "0.5"]
        for row, value in zip(rows, values, strict=True):
            row[fcld_idx] = value
        return header, rows

    config_path, _ = _prepare_engine_inputs(
        _engine_guards_base,
        tmp_path,
        emissions=2,
        forcing_rows=4,
        forcing_mutator=inject_fcld_sentinel,
    )

    result = subprocess.run(
        [str(suews_engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )

    assert result.returncode == 0, (
        f"suews-engine failed ({result.returncode}): {result.stderr}"
    )
    cols = _read_arrow_columns(tmp_path / "suews_output.arrow", ["Fcld"])
    fcld = cols["Fcld"]
    # Every 5-minute output step must be either exact -999.0 or inside [0.2, 0.8];
    # raw linear interpolation of -999 would produce corrupted values such as -499.4.
    is_sentinel = np.equal(fcld, -999.0)
    is_valid = (fcld >= 0.2 - 1e-12) & (fcld <= 0.8 + 1e-12)
    assert np.any(is_sentinel), "expected missing interval to remain -999.0"
    assert np.any(is_valid), "expected valid knots to retain observed fcld"
    assert np.all(is_sentinel | is_valid), (
        f"found sentinel-corrupted interpolated Fcld values: {fcld[~(is_sentinel | is_valid)]}"
    )


def test_fortran_anthropogenic_emissions_invalid_method_raises_error_73(
    tmp_path: Path,
):
    """Direct Fortran kernel call on invalid EmissionsMethod holes triggers ErrorHint(73)."""
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        pytest.skip(
            "Direct Fortran anthro probe requires gfortran and source-build libraries"
        )

    source = tmp_path / "anthro_probe.f90"
    source.write_text(
        """program anthro_probe
use module_phys_anthro, only: AnthropogenicEmissions
use module_ctrl_error_state, only: reset_supy_error, get_supy_error
implicit none
real(8) :: hdd_id(12), pair2(2), prof(0:23, 2)
real(8) :: qf_sahp, fc_anthro, fc_metab, fc_traff, fc_build, fc_point
integer :: dow(3), code, idx
integer, parameter :: invalid_methods(4) = [7, 17, 20, 47]
character(512) :: message

hdd_id = 0.0d0
pair2 = 1.0d0
prof = 1.0d0
dow = [2, 1, 1]

do idx = 1, size(invalid_methods)
  qf_sahp = 123.0d0
  fc_anthro = 123.0d0
  fc_metab = 123.0d0
  fc_traff = 123.0d0
  fc_build = 123.0d0
  fc_point = 123.0d0

  call reset_supy_error()
  call AnthropogenicEmissions( &
    0.0d0, invalid_methods(idx), &
    12, 0, 0, dow, &
    1.0d0, pair2, 1.0d0, 1.0d0, &
    0.5d0, 0.5d0, &
    1.0d0, 2.0d0, 1.0d0, 2.0d0, &
    pair2, 1.0d0, &
    15.0d0, hdd_id, pair2, pair2, pair2, &
    pair2, pair2, pair2, &
    pair2, pair2, &
    pair2, &
    pair2, qf_sahp, &
    fc_anthro, fc_metab, fc_traff, fc_build, fc_point, &
    prof, prof, prof, prof, 1000.0d0)

  call get_supy_error(code, message)
  if (code /= 73) stop 1
  if (index(message, 'EmissionsMethod') == 0) stop 2
  if (qf_sahp /= -999.0d0 .or. fc_anthro /= -999.0d0 .or. &
      fc_metab /= -999.0d0 .or. fc_traff /= -999.0d0 .or. &
      fc_build /= -999.0d0 .or. fc_point /= -999.0d0) stop 3
end do
write(*, '(a)') 'anthro error 73 with sentinel outputs'
end program
""",
        encoding="ascii",
    )
    executable = tmp_path / "anthro_probe"
    build = subprocess.run(
        [
            compiler,
            str(source),
            "-I",
            str(suews / "mod"),
            "-L",
            str(suews / "lib"),
            "-lsuewsphys",
            "-lsuewsutil",
            "-lspartacus",
            "-o",
            str(executable),
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    assert build.returncode == 0, build.stderr

    run = subprocess.run(
        [str(executable)],
        capture_output=True,
        text=True,
        check=False,
    )
    assert run.returncode == 0, run.stdout + run.stderr
    assert "anthro error 73 with sentinel outputs" in run.stdout
