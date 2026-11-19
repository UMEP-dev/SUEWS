! radsurf_config.f90 - Derived type for configuring the radiative treament of surfaces
!
! (C) Copyright 2019- ECMWF.
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
!
module radsurf_config

  use parkind1, only : jpim, jprb
  use radtool_legendre_gauss, only : legendre_gauss_type

  implicit none

  ! Length of string buffer for printing config information
  integer, parameter :: NPrintStringLen = 41
  
  !---------------------------------------------------------------------
  ! Derived type containing all the configuration information needed
  ! to run the radiation scheme.  The intention is that this is fixed
  ! for a given model run.  The parameters are to list first those
  ! quantities that can be set directly by the user, for example using
  ! a namelist, and second those quantities that are computed
  ! afterwards from the user-supplied numbers.
  type config_type

    ! USER-CONFIGURABLE PARAMETERS

    ! Do we do shortwave and longwave calculations, and include
    ! shortwave direct albedo?
    logical :: do_sw = .true.
    logical :: do_lw = .true.
    logical :: use_sw_direct_albedo = .false.

    ! Do we include the capability to do vegetation and urban areas?
    ! If not, some arrays will not be allocated
    logical :: do_vegetation = .true.
    logical :: do_urban      = .true.

    ! Number of regions to represent vegetation (1 or 2) in forest and
    ! vegetated-urban tiles; since clear-skies are also represented,
    ! the total number of regions in which radiation will be modelled
    ! is one plus this number
    integer(kind=jpim) :: n_vegetation_region_forest = 1
    integer(kind=jpim) :: n_vegetation_region_urban  = 1

    ! Number of shortwave and longwave bands for user to supply
    ! properties of facets
    integer(kind=jpim) :: nsw = 1
    integer(kind=jpim) :: nlw = 1

    ! Number of diffuse streams to use in a single hemisphere for a
    ! forest and an urban tile
    integer(kind=jpim) :: n_stream_sw_forest = 4
    integer(kind=jpim) :: n_stream_sw_urban  = 4
    integer(kind=jpim) :: n_stream_lw_forest = 4
    integer(kind=jpim) :: n_stream_lw_urban  = 4

    ! If true, the normalized perimeter length is
    ! 4*frac*(1-frac)/scale, if false it is 4*frac/scale
    logical :: use_symmetric_vegetation_scale_forest = .true.
    logical :: use_symmetric_vegetation_scale_urban  = .true.

    ! If we have two vegetation regions of different optical depth
    ! then each will occupy half of the total vegetated fraction, but
    ! we need to determine the extent of the contact between them and
    ! the clear-sky region.  If the following numbers are 0 then the
    ! optically thick region is completely enclosed within the
    ! optically thin region so there is no contact between the thick
    ! region and the clear-air region. If the numbers are 1 then there
    ! is no contact between the two vegetation regions: they represent
    ! different types of vegetation that are in contact only with
    ! clear-air.
    real(kind=jprb) :: vegetation_isolation_factor_forest = 0.0_jprb
    real(kind=jprb) :: vegetation_isolation_factor_urban  = 0.0_jprb

    ! Minimum vegetation fraction below which vegetation is ignored
    real(kind=jprb) :: min_vegetation_fraction = 1.0e-6

    ! Minimum building fraction below which buildings are ignored
    real(kind=jprb) :: min_building_fraction = 1.0e-6

    ! Do we save broadband and spectral fluxes to output file?
    logical :: do_save_broadband_flux = .true.
    logical :: do_save_spectral_flux  = .false.
    logical :: do_save_flux_profile   = .false.

    integer(kind=jpim) :: iverbose = 3

    ! COMPUTED PARAMETERS

    ! Number of shortwave and longwave bands used internally to
    ! represent the spectral variation of atmospheric absorption
    integer(kind=jpim) :: nswinternal, nlwinternal

    ! Legendre-Gauss structures for each combination of tile type and
    ! spectral region
    type(legendre_gauss_type) :: lg_sw_urban, lg_sw_forest
    type(legendre_gauss_type) :: lg_lw_urban, lg_lw_forest

   contains
     procedure :: read => read_config_from_namelist
     procedure :: consolidate => consolidate_config
     procedure :: print => print_config
     
  end type config_type

contains
  
  !---------------------------------------------------------------------
  ! This subroutine reads configuration data from a namelist file, and
  ! anything that is not in the namelists will be set to default
  ! values. If optional output argument "is_success" is present, then
  ! on error (e.g. missing file) it will be set to .false.; if this
  ! argument is missing then on error the program will be aborted. You
  ! may either specify the file_name or the unit of an open file to
  ! read, but not both.
  subroutine read_config_from_namelist(this, file_name, unit, is_success)

    use yomhook,      only : lhook, dr_hook
    use radiation_io, only : nulout, nulerr, nulrad, radiation_abort

    class(config_type), intent(inout), target :: this
    character(*),       intent(in),  optional :: file_name
    integer(kind=jpim),            intent(in),  optional :: unit
    logical,            intent(out), optional :: is_success

    integer(kind=jpim) :: iosopen, iosread ! Status after calling open and read
    integer(kind=jpim) :: iunit ! Unit number of namelist file

    ! Namelist variables mirroring values in config_type
    
    logical, pointer :: do_sw, do_lw, use_sw_direct_albedo, do_vegetation, &
         &  do_urban, &
         &  use_symmetric_vegetation_scale_forest, &
         &  use_symmetric_vegetation_scale_urban, &
         &  do_save_spectral_flux, do_save_broadband_flux, do_save_flux_profile
    integer(kind=jpim), pointer :: nsw, nlw, n_stream_sw_forest, &
         &  n_stream_sw_urban, n_stream_lw_forest, n_stream_lw_urban, &
         &  iverbose, n_vegetation_region_forest, &
         &  n_vegetation_region_urban
    real(kind=jprb), pointer :: vegetation_isolation_factor_forest, &
         &  vegetation_isolation_factor_urban

    namelist /radsurf/ do_sw, do_lw, use_sw_direct_albedo, do_vegetation, &
         &  do_urban, nsw, nlw, n_stream_sw_forest, n_stream_sw_urban, &
         &  n_stream_lw_forest, n_stream_lw_urban, iverbose, &
         &  do_save_spectral_flux, do_save_broadband_flux, do_save_flux_profile, &
         &  n_vegetation_region_forest, n_vegetation_region_urban, &
         &  use_symmetric_vegetation_scale_forest, &
         &  use_symmetric_vegetation_scale_urban, &
         &  vegetation_isolation_factor_forest, vegetation_isolation_factor_urban

    real(jprb) :: hook_handle

    if (lhook) call dr_hook('radsurf_config:read',0,hook_handle)

    do_sw                => this%do_sw
    do_lw                => this%do_lw
    use_sw_direct_albedo => this%use_sw_direct_albedo
    do_vegetation        => this%do_vegetation
    do_urban             => this%do_urban
    nsw                  => this%nsw
    nlw                  => this%nlw
    n_stream_sw_forest   => this%n_stream_sw_forest
    n_stream_sw_urban    => this%n_stream_sw_urban
    n_stream_lw_forest   => this%n_stream_lw_forest
    n_stream_lw_urban    => this%n_stream_lw_urban
    do_save_spectral_flux=> this%do_save_spectral_flux
    do_save_broadband_flux=>this%do_save_broadband_flux
    do_save_flux_profile => this%do_save_flux_profile
    iverbose             => this%iverbose
    n_vegetation_region_forest => this%n_vegetation_region_forest
    n_vegetation_region_urban  => this%n_vegetation_region_urban
    use_symmetric_vegetation_scale_forest => this%use_symmetric_vegetation_scale_forest
    use_symmetric_vegetation_scale_urban  => this%use_symmetric_vegetation_scale_urban
    vegetation_isolation_factor_forest    => this%vegetation_isolation_factor_forest
    vegetation_isolation_factor_urban     => this%vegetation_isolation_factor_urban

    if (present(file_name) .and. present(unit)) then
      write(nulerr,'(a)') '*** Error: cannot specify both file_name and unit in call to config_type%read'
      call radiation_abort('Radiation configuration error')
      return
    else if (.not. present(file_name) .and. .not. present(unit)) then
      write(nulerr,'(a)') '*** Error: neither file_name nor unit specified in call to config_type%read'
      call radiation_abort('Radiation configuration error')
      return
    end if

    if (present(file_name)) then
      ! Open the namelist file
      iunit = nulrad
      open(unit=iunit, iostat=iosopen, file=trim(file_name))
    else
      ! Assume that iunit represents and open file
      iosopen = 0
      iunit = unit
    end if

    if (iosopen /= 0) then
      ! An error occurred opening the file
      if (present(is_success)) then
        is_success = .false.
        ! We now continue the subroutine so that the default values
        ! are placed in the config structure
      else
        write(nulerr,'(a,a,a)') '*** Error: namelist file "', &
             &                trim(file_name), '" not found'
        call radiation_abort('Radiation configuration error')
        return
      end if
    else
      read(unit=iunit, iostat=iosread, nml=radsurf)
      if (iosread /= 0) then
        ! An error occurred reading the file
        if (present(is_success)) then
          is_success = .false.
          ! We now continue the subroutine so that the default values
          ! are placed in the config structure
        else if (present(file_name)) then
          write(nulerr,'(a,a,a)') '*** Error reading namelist "radsurf" from file "', &
               &      trim(file_name), '"'
          close(unit=iunit)
          call radiation_abort('Radiation configuration error')
          return
        else
          write(nulerr,'(a,i0)') '*** Error reading namelist "radsurf" from unit ', &
               &      iunit
          call radiation_abort('Radiation configuration error')
          return
        end if
      end if

      if (present(file_name)) then
        close(unit=iunit)
      end if
    end if

    if (lhook) call dr_hook('radsurf_config:read',1,hook_handle)

  end subroutine read_config_from_namelist

  
  subroutine consolidate_config(this)

    use radiation_io, only : radiation_abort

    use yomhook,      only : lhook, dr_hook

    class(config_type), intent(inout) :: this

    real(jprb) :: hook_handle
    
    if (lhook) call dr_hook('radsurf_config:consolidate',0,hook_handle)

    if (min(this%n_stream_sw_forest, this%n_stream_sw_urban, &
         & this%n_stream_lw_forest, this%n_stream_lw_urban) < 1) then
      call radiation_abort('SPARTACUS stream counts must be positive')
      return
    end if
    if (this%do_vegetation) then
      if (min(this%n_vegetation_region_forest, this%n_vegetation_region_urban) < 1 &
           & .or. max(this%n_vegetation_region_forest, this%n_vegetation_region_urban) > 2) then
        call radiation_abort('SPARTACUS vegetation region counts must be 1 or 2')
        return
      end if
    end if

    this%nswinternal = this%nsw
    this%nlwinternal = this%nlw

    call this%lg_sw_forest%initialize(this%n_stream_sw_forest)
    call this%lg_sw_urban%initialize(this%n_stream_sw_urban)
    call this%lg_lw_forest%initialize(this%n_stream_lw_forest)
    call this%lg_lw_urban%initialize(this%n_stream_lw_urban)
    
    if (lhook) call dr_hook('radsurf_config:consolidate',1,hook_handle)
    
  end subroutine consolidate_config


  !---------------------------------------------------------------------
  ! Print configuration information to standard output
  subroutine print_config(this, iverbose)

    use radiation_io, only : nulout

    class(config_type), intent(in) :: this

    integer, optional,  intent(in) :: iverbose
    integer                        :: i_local_verbose

    if (present(iverbose)) then
      i_local_verbose = iverbose
    else
      i_local_verbose = this%iverbose
    end if

    if (i_local_verbose >= 2) then
      !---------------------------------------------------------------------
      write(nulout, '(a)') 'General settings:'
      call print_logical('  Represent vegetation', &
           &  'do_vegetation', this%do_vegetation)
      call print_logical('  Represent urban areas', &
           &  'do_urban', this%do_urban)
      call print_logical('  Do shortwave (SW) calculations', &
           &  'do_sw', this%do_sw)
      call print_logical('  Do longwave (LW) calculations', &
           &  'do_sw', this%do_lw)
      if (this%do_sw) then
        call print_integer('  Number of SW spectral intervals', &
             &  'nsw', this%nsw)
      end if
      if (this%do_lw) then
        call print_integer('  Number of LW spectral intervals', &
             &  'nlw', this%nlw)
      end if
      
      if (this%do_vegetation) then
        call print_real('  Minimum vegetation fraction', &
             &  'min_vegetation_fraction', this%min_vegetation_fraction)
        write(nulout, '(a)') 'Settings for forests:'
        call print_integer('  Number of vegetation regions', &
             &  'n_vegetation_region_forest', this%n_vegetation_region_forest)
        call print_logical('  Use symmetric vegetation scale', &
             &  'use_symmetric_vegetation_scale_forest', &
             &  this%use_symmetric_vegetation_scale_forest)
        call print_real('  Vegetation isolation factor', &
             &  'vegetation_isolation_factor_forest', &
             &  this%vegetation_isolation_factor_forest)
        if (this%do_sw) then
          call print_integer('  SW diffuse streams per hemisphere', &
               &  'n_stream_sw_forest', this%n_stream_sw_forest)
        end if
        if (this%do_lw) then
          call print_integer('  LW streams per hemisphere', &
               &  'n_stream_lw_forest', this%n_stream_lw_forest)
        end if
      end if

      if (this%do_urban) then
        write(nulout, '(a)') 'Settings for urban areas:'
        if (this%do_vegetation) then
          call print_integer('  Number of vegetation regions', &
               &  'n_vegetation_region_urban', this%n_vegetation_region_urban)
          call print_logical('  Use symmetric vegetation scale', &
               &  'use_symmetric_vegetation_scale_urban', &
               &  this%use_symmetric_vegetation_scale_urban)
          call print_real('  Vegetation isolation factor', &
               &  'vegetation_isolation_factor_urban', &
               &  this%vegetation_isolation_factor_urban)
        end if
        if (this%do_sw) then
          call print_integer('  SW diffuse streams per hemisphere', &
               &  'n_stream_sw_urban', this%n_stream_sw_urban)
        end if
        if (this%do_lw) then
          call print_integer('  LW streams per hemisphere', &
               &  'n_stream_lw_urban', this%n_stream_lw_urban)
        end if
      end if
     
    end if

  end subroutine print_config
  
  !---------------------------------------------------------------------
  ! Print one line of information: logical
  subroutine print_logical(message_str, name, val)
    use radiation_io, only : nulout
    character(len=*),   intent(in) :: message_str
    character(len=*),   intent(in) :: name
    logical,            intent(in) :: val
    character(4)                   :: on_or_off
    character(NPrintStringLen)     :: str
    if (val) then
      on_or_off = ' ON '
    else
      on_or_off = ' OFF'
    end if
    write(str, '(a,a4)') message_str, on_or_off
    write(nulout,'(a,a,a,a,l1,a)') str, ' (', name, '=', val,')'
  end subroutine print_logical


  !---------------------------------------------------------------------
  ! Print one line of information: integer
  subroutine print_integer(message_str, name, val)
    use radiation_io, only : nulout
    character(len=*),   intent(in) :: message_str
    character(len=*),   intent(in) :: name
    integer,            intent(in) :: val
    character(NPrintStringLen)     :: str
    write(str, '(a,a,i0)') message_str, ' = ', val
    write(nulout,'(a,a,a,a)') str, ' (', name, ')'
  end subroutine print_integer


  !---------------------------------------------------------------------
  ! Print one line of information: real
  subroutine print_real(message_str, name, val)
    use parkind1,     only : jprb
    use radiation_io, only : nulout
    character(len=*),   intent(in) :: message_str
    character(len=*),   intent(in) :: name
    real(jprb),         intent(in) :: val
    character(NPrintStringLen)     :: str
    write(str, '(a,a,g8.3)') message_str, ' = ', val
    write(nulout,'(a,a,a,a)') str, ' (', name, ')'
  end subroutine print_real

end module radsurf_config
