"""Probe the Fortran timestep and batch entry points without Rust validation."""

from pathlib import Path
import shutil
import subprocess

import pytest

pytestmark = pytest.mark.physics


def test_fortran_input_bounds(tmp_path):
    """Native callers receive TLS errors before allocation or timestep division."""
    root = Path(__file__).resolve().parents[2]
    suews = root / "src" / "suews"
    compiler = shutil.which("gfortran")
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        pytest.skip("Requires gfortran and the source-built SUEWS native libraries")
    source = tmp_path / "bounds.f95"
    source.write_text(
        """program bounds
   use module_util_time, only: SUEWS_cal_tstep
   use SUEWS_Driver, only: SUEWS_cal_multitsteps_dts
   use module_phys_spartacus, only: SPARTACUS
   use module_ctrl_const_allocate, only: ncolumnsDataOutSPARTACUS
   use module_ctrl_type, only: SUEWS_TIMER, SUEWS_SITE, SUEWS_STATE, SUEWS_CONFIG
   use module_ctrl_error_state, only: reset_supy_error, supy_error_flag, get_supy_error
   implicit none
   type(SUEWS_TIMER) :: timer
   type(SUEWS_SITE) :: site
   type(SUEWS_STATE) :: state
   type(SUEWS_CONFIG) :: config
   integer :: i, nsh, code, nlayer
   integer :: invalid_steps(6) = [-999, -1, 0, 7, 3601, 7200]
   integer :: valid_steps(5) = [1, 60, 300, 900, 3600]
   integer :: invalid_layers(5) = [-999, -1, 0, 16, huge(1)]
   real(kind(1D0)) :: nsh_real, tstep_real, forcing(1,32), data(1,1), dates(1,5)
   real(kind(1D0)) :: qn, kup, lup, roof(16), wall(16), surface(7)
   real(kind(1D0)) :: roof_sw(15), roof_lw(15), wall_sw(15), wall_lw(15)
   real(kind(1D0)) :: diagnostic(ncolumnsDataOutSPARTACUS - 5)
   real(kind(1D0)) :: layer_input(16), height(17), spectral(1,16)
   character(len=512) :: message
   do i = 1, size(invalid_steps)
      call reset_supy_error()
      timer%tstep = invalid_steps(i)
      call SUEWS_cal_tstep(timer, nsh, nsh_real, tstep_real)
      call get_supy_error(code, message)
      if (.not. supy_error_flag() .or. code /= 110) stop 1
      if (nsh /= -999 .or. nsh_real /= -999D0 .or. tstep_real /= -999D0) stop 2
   end do
   do i = 1, size(valid_steps)
      call reset_supy_error()
      timer%tstep = valid_steps(i)
      call SUEWS_cal_tstep(timer, nsh, nsh_real, tstep_real)
      if (supy_error_flag()) stop 3
      if (nsh /= 3600/valid_steps(i) .or. nsh_real /= real(nsh,8)) stop 4
      if (tstep_real /= real(valid_steps(i),8)) stop 5
   end do
   do i = 1, size(invalid_layers)
      call reset_supy_error()
      site%nlayer = invalid_layers(i)
      data = 123D0
      dates = 123D0
      forcing = 0D0
      call SUEWS_cal_multitsteps_dts(timer, forcing, 1, config, site, state, data, dates)
      call get_supy_error(code, message)
      if (.not. supy_error_flag() .or. code /= 110) stop 6
      if (any(data /= 123D0) .or. any(dates /= 123D0)) stop 7
   end do
   ! Call the radiation leaf directly: invalid dimensions must not allocate
   ! local arrays or write the layer-sized caller buffers before rejection.
   layer_input = 0D0
   height = 0D0
   spectral = 0D0
   do i = 1, size(invalid_layers)
      nlayer = invalid_layers(i)
      call reset_supy_error()
      roof = 123D0
      wall = 123D0
      call SPARTACUS( &
         DiagQN=0, sfr_surf=[1D0,0D0,0D0,0D0,0D0,0D0,0D0], zenith_deg=45D0, nlayer=nlayer, &
         tsfc_surf=[20D0,20D0,20D0,20D0,20D0,20D0,20D0], tsfc_roof=layer_input, tsfc_wall=layer_input, &
         kdown=500D0, kdown_direct=300D0, kdown_diffuse=200D0, ldown=350D0, Tair_C=20D0, &
         rsl_z=[0D0,20D0], rsl_t_C=[20D0,20D0], &
         alb_surf=[0D0,0D0,0D0,0D0,0D0,0D0,0D0], emis_surf=[1D0,1D0,1D0,1D0,1D0,1D0,1D0], &
         LAI_id=[0D0,0D0,0D0], n_vegetation_region_urban=1, n_vegetation_region_forest=1, &
         n_stream_sw_urban=4, n_stream_sw_forest=1, n_stream_lw_urban=4, n_stream_lw_forest=4, &
         air_ext_sw=0D0, air_ssa_sw=0D0, veg_ssa_sw=0D0, air_ext_lw=0D0, air_ssa_lw=0D0, veg_ssa_lw=0D0, &
         veg_fsd_const=0D0, veg_contact_fraction_const=0D0, ground_albedo_dir_mult_fact=1D0, &
         use_sw_direct_albedo=.false., height=height, bldgH=0D0, sfr_evetr=0D0, sfr_dectr=0D0, &
         EveTreeH=0D0, DecTreeH=0D0, building_frac=layer_input, veg_frac=layer_input, &
         sfr_roof=layer_input, sfr_wall=layer_input, building_scale=layer_input, veg_scale=layer_input, &
         veg_ext_input=layer_input, alb_roof=layer_input, emis_roof=layer_input, &
         alb_wall=layer_input, emis_wall=layer_input, roof_albedo_dir_mult_fact=spectral, &
         wall_specular_frac=spectral, qn=qn, kup=kup, lup=lup, qn_roof=roof, qn_wall=wall, &
         qn_surf=surface, roof_in_sw_spc=roof_sw, roof_in_lw_spc=roof_lw, wall_in_sw_spc=wall_sw, &
         wall_in_lw_spc=wall_lw, dataOutLineSPARTACUS=diagnostic, errorState=state%errorState)
      call get_supy_error(code, message)
      if (.not. supy_error_flag() .or. code /= 110) stop 8
      if (any(roof /= 123D0) .or. any(wall /= 123D0)) stop 9
   end do
end program bounds
""",
        encoding="utf-8",
    )
    executable = tmp_path / "bounds"
    compiled = subprocess.run(
        [compiler, str(source), "-I", str(suews / "mod"), "-L", str(suews / "lib"),
         "-lsuewsdriver", "-lsuewsphys", "-lsuewsutil", "-lspartacus", "-fopenmp", "-o", str(executable)],
        capture_output=True, text=True, check=False,
    )
    assert compiled.returncode == 0, compiled.stderr
    result = subprocess.run([str(executable)], capture_output=True, text=True, check=False)
    assert result.returncode == 0, result.stdout + result.stderr
