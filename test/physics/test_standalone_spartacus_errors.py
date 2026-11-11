"""The standalone CLI must report fatal TLS diagnostics from any worker."""

import os
from pathlib import Path
import subprocess

import numpy as np
import pytest
from scipy.io import netcdf_file

pytestmark = [pytest.mark.physics, pytest.mark.core]


@pytest.fixture(scope="module")
def standalone_spartacus():
    vendor = Path(__file__).resolve().parents[2] / "src/suews/ext_lib/spartacus-surface"
    executable = Path(os.environ.get(
        "SUEWS_STANDALONE_SPARTACUS", vendor / "bin/spartacus_surface",
    )).resolve()
    if not executable.is_file():
        pytest.skip("Standalone SPARTACUS executable with NetCDF/OpenMP is not built")
    fixture = vendor / "test/urban/russell_square.nc"
    return executable, fixture


@pytest.mark.parametrize("failure", ["serial_config", "worker_only", "both_workers"])
def test_standalone_reports_first_fatal(standalone_spartacus, tmp_path, failure):
    executable, fixture = standalone_spartacus
    parallel = failure != "serial_config"
    config = tmp_path / "config.nam"
    config.write_text(
        "&radsurf_driver\n"
        f"do_parallel = {'.true.' if parallel else '.false.'},\n"
        "nblocksize = 1, iverbose = 3, vegetation_fsd = 0.5,\n"
        "top_flux_dn_sw = 1000., top_flux_dn_direct_sw = 1000.,\n"
        "do_conservation_check = .false.\n/\n"
        "&radsurf\ndo_sw = .true., do_lw = .false.,\n"
        f"n_stream_sw_urban = {2 if parallel else 0}\n/\n",
        encoding="ascii",
    )
    input_path = tmp_path / "input.nc"
    if parallel:
        # Static scheduling sends column 2 to worker 1. In worker_only mode,
        # the master thread's TLS remains clear, so it cannot supply the text.
        with netcdf_file(fixture, "r", mmap=False) as source, netcdf_file(input_path, "w") as target:
            for name, size in source.dimensions.items():
                target.createDimension(name, 2 if name == "column" else size)
            for name, variable in source.variables.items():
                data = variable[:].copy()
                if "column" in variable.dimensions:
                    data = np.repeat(data, 2, axis=variable.dimensions.index("column"))
                if name == "building_fraction":
                    data[1, :2] = [0.2, 0.4]
                    if failure == "both_workers":
                        data[0, :2] = [0.2, 0.4]
                output = target.createVariable(name, variable.typecode(), variable.dimensions)
                output[:] = data
        message = "Building fraction cannot increase with height"
    else:
        message = "SPARTACUS stream counts must be positive"

    output_path = tmp_path / "output.nc"
    result = subprocess.run(
        [str(executable), str(config), str(input_path), str(output_path)],
        env={**os.environ, "OMP_NUM_THREADS": "2", "OMP_DYNAMIC": "FALSE",
             "OMP_SCHEDULE": "static,1"},
        capture_output=True, text=True, timeout=30, check=False,
    )
    assert result.returncode == 1, result.stdout + result.stderr
    assert result.stderr.count(f"SPARTACUS error 111: {message}") == 1, result.stderr
    assert not output_path.exists()
    if parallel:
        assert "Thread 1 processing columns 2-2" in result.stdout
