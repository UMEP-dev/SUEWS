! spartacus_surface_config.F90 - Configure driver for SPARTACUS-Surface radiation scheme
!
! (C) Copyright 2020- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int

module spartacus_surface_config

  use parkind1,                      only : jprb, jpim

  implicit none

  type driver_config_type
    
     ! Parallel settings
     logical :: do_parallel = .true.
     integer(kind=jpim) :: nblocksize = 16 ! Number of columns processed at once

     ! Do we repeat the core call, for benchmarking?
     integer(kind=jpim) :: nrepeat = 1

     ! Process a limited number of columns (iendcol=0 indicates to
     ! process from istartcol up to the end)
     integer(kind=jpim) :: istartcol = 1
     integer(kind=jpim) :: iendcol = 0

     ! Control verbosity in driver routine: 0=none (no output to
     ! standard output; write to standard error only if an error
     ! occurs), 1=warning, 2=info, 3=progress, 4=detailed, 5=debug
     integer(kind=jpim) :: iverbose = 3

     ! Do we check the fluxes at the end for conservation?
     logical :: do_conservation_check = .false.
     
     ! Override values
     real(kind=jprb) :: cos_sza_override     = -1.0
     real(kind=jprb) :: ground_sw_albedo     = -1.0
     real(kind=jprb) :: roof_sw_albedo       = -1.0
     real(kind=jprb) :: wall_sw_albedo       = -1.0
     real(kind=jprb) :: ground_lw_emissivity = -1.0
     real(kind=jprb) :: roof_lw_emissivity   = -1.0
     real(kind=jprb) :: wall_lw_emissivity   = -1.0
     real(kind=jprb) :: vegetation_fraction      = -1.0
     real(kind=jprb) :: vegetation_extinction    = -1.0
     real(kind=jprb) :: vegetation_fsd           = -1.0
     real(kind=jprb) :: vegetation_sw_ssa        = -1.0
     real(kind=jprb) :: vegetation_lw_ssa        = -1.0
     real(kind=jprb) :: top_flux_dn_sw           = -1.0
     real(kind=jprb) :: top_flux_dn_direct_sw    = -1.0
     real(kind=jprb) :: top_flux_dn_lw           = -1.0
     
   contains
     procedure :: read => read_config_from_namelist

  end type driver_config_type

contains

  !---------------------------------------------------------------------
  ! This subroutine reads configuration data from a namelist file, and
  ! anything that is not in the namelists will be set to default
  ! values. If optional output argument "is_success" is present, then on
  ! error (e.g. missing file) it will be set to .false.; if this
  ! argument is missing then on error the program will be aborted.
  subroutine read_config_from_namelist(this, file_name, is_success)

    use radiation_io, only : nulerr, radiation_abort

    class(driver_config_type), intent(inout), target :: this
    character(*), intent(in)          :: file_name
    logical, intent(out), optional    :: is_success

    integer :: iosopen ! Status after calling open

    logical,            pointer :: do_parallel, do_conservation_check
    integer(kind=jpim), pointer :: nblocksize, istartcol, iendcol, iverbose, nrepeat
    real(kind=jprb),    pointer :: cos_solar_zenith_angle, vegetation_fsd, vegetation_fraction
    real(kind=jprb),    pointer :: ground_sw_albedo, roof_sw_albedo, wall_sw_albedo
    real(kind=jprb),    pointer :: ground_lw_emissivity, roof_lw_emissivity, wall_lw_emissivity
    real(kind=jprb),    pointer :: vegetation_extinction, vegetation_sw_ssa
    real(kind=jprb),    pointer :: top_flux_dn_sw, top_flux_dn_direct_sw
    real(kind=jprb),    pointer :: top_flux_dn_lw

    namelist /radsurf_driver/ do_parallel, nblocksize, nrepeat, istartcol, iendcol, &
         &  iverbose, cos_solar_zenith_angle, vegetation_fsd, &
         &  ground_sw_albedo, roof_sw_albedo, wall_sw_albedo, &
         &  ground_lw_emissivity, roof_lw_emissivity, wall_lw_emissivity, &
         &  vegetation_extinction, vegetation_sw_ssa, vegetation_fraction, &
         &  top_flux_dn_sw, top_flux_dn_direct_sw, top_flux_dn_lw, &
         &  do_conservation_check

    do_parallel            => this%do_parallel
    do_conservation_check  => this%do_conservation_check
    nblocksize             => this%nblocksize
    nrepeat                => this%nrepeat
    istartcol              => this%istartcol
    iendcol                => this%iendcol
    iverbose               => this%iverbose
    cos_solar_zenith_angle => this%cos_sza_override
    ground_sw_albedo       => this%ground_sw_albedo
    roof_sw_albedo         => this%roof_sw_albedo
    wall_sw_albedo         => this%wall_sw_albedo
    ground_lw_emissivity   => this%ground_lw_emissivity
    roof_lw_emissivity     => this%roof_lw_emissivity
    wall_lw_emissivity     => this%wall_lw_emissivity
    vegetation_fraction    => this%vegetation_fraction
    vegetation_fsd         => this%vegetation_fsd
    vegetation_extinction  =>this%vegetation_extinction
    vegetation_sw_ssa      => this%vegetation_sw_ssa
    top_flux_dn_sw         => this%top_flux_dn_sw
    top_flux_dn_direct_sw  => this%top_flux_dn_direct_sw
    top_flux_dn_lw         => this%top_flux_dn_lw

    ! Open the namelist file and read the radiation_driver namelist
    open(unit=10, iostat=iosopen, file=trim(file_name))
    if (iosopen /= 0) then
      ! An error occurred
      if (present(is_success)) then
        is_success = .false.
        ! We now continue the subroutine so that the default values
        ! are placed in the config structure
      else
        write(nulerr,'(a,a,a)') '*** Error: namelist file "', &
             &                trim(file_name), '" not found'
        call radiation_abort('Driver configuration error')
        return
      end if
    else
      ! Read the radiation_driver namelist, noting that it is not an
      ! error if this namelist is not present, provided all the required
      ! variables are present in the NetCDF data file instead
      read(unit=10, nml=radsurf_driver)
      close(unit=10)
    end if

  end subroutine read_config_from_namelist

end module spartacus_surface_config
