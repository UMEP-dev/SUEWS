"""End-to-end input-guard regressions for standalone ``suews-engine`` and Fortran anthro."""

from importlib import import_module
from importlib.resources import as_file, files
import os
from pathlib import Path
import shutil
import subprocess

import numpy as np
from pyarrow import ipc
import pytest
import yaml

from supy.data_model import SUEWSConfig

_run_rust = import_module("supy._run_rust")
pytestmark = [pytest.mark.physics, pytest.mark.api]


def _locate_suews_engine() -> Path:
    """Return the bundled or locally built ``suews-engine`` binary."""
    binary_name = "suews-engine.exe" if os.name == "nt" else "suews-engine"
    bundled = files("supy").joinpath("bin").joinpath(binary_name)
    with as_file(bundled) as bundled_path:
        if bundled_path.exists():
            return Path(bundled_path)

    repo_root = Path(__file__).resolve().parents[2]
    dev_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / binary_name
    if dev_binary.exists():
        return dev_binary
    pytest.skip("Rust CLI binary not found; build with: make bridge")


def _prepare_engine_inputs(
    run_dir: Path,
    *,
    emissions: int = 2,
    forcing_rows: int = 4,
    forcing_mutator=None,
    override_emissions_raw: int | None = None,
) -> Path:
    """Write resolved YAML config and short forcing prefix into ``run_dir``."""
    sample_dir = files("supy").joinpath("sample_data")
    with sample_dir.joinpath("sample_config.yml").open(encoding="utf-8") as handle:
        cfg = yaml.safe_load(handle)

    control = cfg["model"]["control"]
    control["output"]["dir"] = str(run_dir)
    forcing_entry = control.get("forcing", {}).get("file")
    forcing_name = (
        forcing_entry["value"] if isinstance(forcing_entry, dict) else forcing_entry
    )
    assert isinstance(forcing_name, str) and forcing_name

    cfg["model"]["physics"]["emissions"] = {"value": emissions}
    resolved_yaml = _run_rust.config_to_engine_yaml(SUEWSConfig.from_dict(cfg))

    if override_emissions_raw is not None:
        resolved_doc = yaml.safe_load(resolved_yaml)
        resolved_doc["model"]["physics"]["emissions"] = {
            "value": override_emissions_raw
        }
        resolved_yaml = yaml.safe_dump(resolved_doc, sort_keys=False)

    forcing_raw = sample_dir.joinpath(forcing_name).read_text(encoding="utf-8")
    forcing_lines = [line for line in forcing_raw.splitlines() if line.strip()]
    header = forcing_lines[0].split()
    rows = [line.split() for line in forcing_lines[1 : forcing_rows + 1]]
    if forcing_mutator is not None:
        header, rows = forcing_mutator(header, rows)

    forcing_text = "\n".join([" ".join(header), *(" ".join(row) for row in rows)])
    (run_dir / forcing_name).write_text(forcing_text + "\n", encoding="utf-8")

    config_path = run_dir / "sample_config.yml"
    config_path.write_text(resolved_yaml, encoding="utf-8")
    return config_path


def _read_arrow_columns(arrow_path: Path, columns: list[str]) -> dict[str, np.ndarray]:
    """Read selected columns from an Arrow IPC file and release the mmap."""
    with ipc.open_file(str(arrow_path)) as reader:
        table = reader.read_all().select(columns)
        return {
            name: table.column(name).to_numpy(zero_copy_only=False).copy()
            for name in columns
        }


def test_engine_rejects_invalid_emissions_hole(tmp_path: Path):
    """Standalone ``suews-engine`` must reject invalid emissions holes such as 17."""
    engine_bin = _locate_suews_engine()
    config_path = _prepare_engine_inputs(
        tmp_path,
        emissions=2,
        override_emissions_raw=17,
    )

    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )

    assert result.returncode != 0, "emissions=17 must not exit 0"
    assert "model.physics.emissions" in result.stderr
    assert "17" in result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()


@pytest.mark.parametrize("emissions", [2, 12])
def test_engine_accepts_valid_emissions_including_co2_methods(
    tmp_path: Path, emissions: int
):
    """Valid modelled heat (2) and biogenic CO2 (>=11) emissions codes pass."""
    engine_bin = _locate_suews_engine()
    config_path = _prepare_engine_inputs(
        tmp_path,
        emissions=emissions,
        forcing_rows=2,
    )

    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
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
    tmp_path: Path, missing_mode: str
):
    """Standalone ``suews-engine`` fails loudly when emissions=0 lacks valid qf."""
    engine_bin = _locate_suews_engine()

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

    config_path = _prepare_engine_inputs(
        tmp_path,
        emissions=0,
        forcing_rows=4,
        forcing_mutator=mutate_forcing,
    )

    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )

    assert result.returncode != 0, f"emissions=0 ({missing_mode}) must fail loudly"
    assert "forcing column 'qf'" in result.stderr
    assert "emissions=0" in result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()


def test_engine_accepts_valid_observed_qf_for_emissions_zero(tmp_path: Path):
    """Standalone ``suews-engine`` honours valid observed qf when emissions=0."""
    engine_bin = _locate_suews_engine()

    def set_valid_qf(header: list[str], rows: list[list[str]]):
        qf_idx = next(i for i, col in enumerate(header) if col.lower() == "qf")
        for row in rows:
            row[qf_idx] = "25.0"
        return header, rows

    config_path = _prepare_engine_inputs(
        tmp_path,
        emissions=0,
        forcing_rows=3,
        forcing_mutator=set_valid_qf,
    )

    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
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


def test_engine_does_not_interpolate_sentinel_into_corrupted_floats(tmp_path: Path):
    """An interior -999 in hourly forcing must stay -999, never blend into valid data."""
    engine_bin = _locate_suews_engine()

    def inject_fcld_sentinel(header: list[str], rows: list[list[str]]):
        fcld_idx = next(i for i, col in enumerate(header) if col.lower() == "fcld")
        values = ["0.2", "-999", "0.8", "0.5"]
        for row, value in zip(rows, values, strict=True):
            row[fcld_idx] = value
        return header, rows

    config_path = _prepare_engine_inputs(
        tmp_path,
        emissions=2,
        forcing_rows=4,
        forcing_mutator=inject_fcld_sentinel,
    )

    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
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


@pytest.mark.parametrize("invalid_method", [7, 17, 20, 47])
def test_fortran_anthropogenic_emissions_invalid_method_raises_error_73(
    tmp_path: Path, invalid_method: int
):
    """Direct Fortran kernel call on an invalid EmissionsMethod triggers ErrorHint(73)."""
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        pytest.skip(
            "Direct Fortran anthro probe requires gfortran and source-build libraries"
        )

    source = tmp_path / "anthro_probe.f90"
    source.write_text(
        f"""program anthro_probe
use module_phys_anthro, only: AnthropogenicEmissions
use module_ctrl_error_state, only: reset_supy_error, get_supy_error
implicit none
real(8) :: hdd_id(12), pair2(2), prof(0:23, 2)
real(8) :: qf_sahp, fc_anthro, fc_metab, fc_traff, fc_build, fc_point
integer :: dow(3), code
character(512) :: message

hdd_id = 0.0d0
pair2 = 1.0d0
prof = 1.0d0
dow = [2, 1, 1]
qf_sahp = 123.0d0
fc_anthro = 123.0d0
fc_metab = 123.0d0
fc_traff = 123.0d0
fc_build = 123.0d0
fc_point = 123.0d0

call reset_supy_error()
call AnthropogenicEmissions( &
  0.0d0, {invalid_method}, &
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
