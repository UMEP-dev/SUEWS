"""Kernel stability dispatch must never return an unassigned result (GH#1783)."""

import copy
import gzip
from importlib import import_module
import json
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

pytestmark = pytest.mark.physics


@pytest.fixture(scope="module")
def stability_probe(tmp_path_factory):
    """Call the actual Fortran selectors, including their thread-local errors."""
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        pytest.skip("Native checks need gfortran and source-build libraries; run tests cover wheels")
    try:
        with gzip.open(suews / "mod" / "module_phys_atmmoiststab.mod", "rb") as stream:
            is_gnu = stream.readline().startswith(b"GFORTRAN")
    except gzip.BadGzipFile:
        is_gnu = False
    if not is_gnu:
        pytest.skip("Native harness needs GNU Fortran modules")
    directory = tmp_path_factory.mktemp("stability")
    source = directory / "probe.f95"
    source.write_text(
        """program probe
use module_phys_atmmoiststab
use module_ctrl_error_state, only: reset_supy_error, get_supy_error
implicit none
integer :: method, selector, code, status
real(kind(1d0)) :: zeta, value
character(len=512) :: message
do
  read(*, *, iostat=status) method, selector, zeta
  if (status /= 0) exit
  call reset_supy_error()
  select case(selector)
  case(1)
    value = stab_psi_mom(method, zeta)
  case(2)
    value = stab_psi_heat(method, zeta)
  case(3)
    value = stab_phi_mom(method, zeta)
  case(4)
    value = stab_phi_heat(method, zeta)
  end select
  call get_supy_error(code, message)
  write(*, '(ES24.16,1X,I0)') value, code
  write(*, '(A)') trim(message)
end do
end program
""",
        encoding="ascii",
    )
    executable = directory / "probe"
    compiled = subprocess.run(
        [compiler, str(source), "-I", str(suews / "mod"),
         "-L", str(suews / "lib"), "-lsuewsphys", "-lsuewsutil",
         "-lspartacus", "-fopenmp", "-o", str(executable)],
        check=False, capture_output=True, text=True,
    )
    assert compiled.returncode == 0, compiled.stderr

    def run(method, selector, zeta):
        result = subprocess.run(
            [str(executable)], input=f"{method} {selector} {zeta}\n",
            check=True, capture_output=True, text=True,
        )
        lines = result.stdout.splitlines()
        value, code = lines[0].split()
        return float(value), int(code), lines[1].strip()

    return run


@pytest.mark.parametrize("method", [0, 1, -1, -999, 99])
@pytest.mark.parametrize("selector", [1, 2, 3, 4])
def test_reserved_and_unknown_kernel_codes_fail(stability_probe, method, selector):
    value, code, message = stability_probe(method, selector, -0.1)
    assert code == 107
    assert f"stability={method} is reserved or unsupported" in message
    assert value == (0.0 if selector <= 2 else 1.0)


@pytest.mark.parametrize("method", [3, 4, 5])
@pytest.mark.parametrize("selector", [1, 2, 3, 4])
@pytest.mark.parametrize("zeta", [-0.1, 0.0, 0.1])
def test_implemented_kernel_methods_remain_finite(stability_probe, method, selector, zeta):
    value, code, message = stability_probe(method, selector, zeta)
    assert code == 0, message
    assert np.isfinite(value)
    if zeta == 0:
        assert value == (0.0 if selector <= 2 else 1.0)


@pytest.fixture(scope="module")
def bridge_inputs(sample_config_loaded, sample_data_loaded):
    """Use the direct bridge's mapping input, which bypasses Pydantic."""
    run_rust = import_module("supy._run_rust")
    _, forcing = sample_data_loaded
    forcing = forcing.iloc[:2]
    return (
        run_rust._check_rust_available(),
        sample_config_loaded.model_dump(exclude_none=True, mode="json"),
        run_rust._prepare_forcing_block(forcing).ravel(order="C").tolist(),
        len(forcing),
    )


@pytest.mark.parametrize("method", [0, 1, -1, -999, 99])
def test_unchecked_mapping_reports_kernel_error(method, bridge_inputs):
    """Direct bridge callers also receive GH#1783's fatal kernel error."""
    rust, config, forcing, n_steps = bridge_inputs
    config = copy.deepcopy(config)
    config["model"]["physics"]["stability"] = {"value": method}
    with pytest.raises(RuntimeError, match=f"stability={method} is reserved or unsupported") as error:
        rust.run_suews_multi([json.dumps(config)], forcing, n_steps, 1)
    assert "(code 107)" in str(error.value)
