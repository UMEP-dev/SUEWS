! spartacus_surface_read_input.F90 - Read input structures from NetCDF file
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
!

module spartacus_surface_read_input

  use radiation_io, only : radiation_error

contains

  subroutine read_input(file, config, driver_config, ncol, ntotlay, &
    &  canopy_props, sw_spectral_props, lw_spectral_props, &
    &  top_flux_dn_sw, top_flux_dn_direct_sw, top_flux_dn_lw)

    use parkind1,                  only : jprb, jpim
    use radiation_io,              only : nulout
    use easy_netcdf,               only : netcdf_file
    use radsurf_config,            only : config_type
    use spartacus_surface_config,  only : driver_config_type
    use radsurf_canopy_properties, only : canopy_properties_type
    use radsurf_sw_spectral_properties,  only : sw_spectral_properties_type
    use radsurf_lw_spectral_properties,  only : lw_spectral_properties_type
    use radiation_constants,       only : StefanBoltzmann

    implicit none

    type(netcdf_file),            intent(in)    :: file
    type(config_type),            intent(in)    :: config
    type(driver_config_type),     intent(in)    :: driver_config
    type(canopy_properties_type), intent(inout) :: canopy_props
    type(sw_spectral_properties_type), intent(inout) :: sw_spectral_props
    type(lw_spectral_properties_type), intent(inout) :: lw_spectral_props

    ! Top-of-canopy downwelling fluxes
    real(kind=jprb), intent(inout), allocatable, dimension(:,:) &
         &  :: top_flux_dn_sw, top_flux_dn_direct_sw, top_flux_dn_lw

    ! Number of columns and layers of input data
    integer(kind=jpim), intent(out) :: ncol, ntotlay

    ! Height of layer interface
    real(kind=jprb), allocatable :: height(:,:)

    integer :: jcol, ilay

    call canopy_props%deallocate()
    call sw_spectral_props%deallocate()
    call lw_spectral_props%deallocate()

    call file%get('nlayer', canopy_props%nlay)
    if (radiation_error()) return

    ncol = size(canopy_props%nlay)
    
    if (config%do_sw) then
      if (driver_config%cos_sza_override >= 0.0_jprb) then
        allocate(canopy_props%cos_sza(ncol))
        canopy_props%cos_sza = driver_config%cos_sza_override
        if  (driver_config%iverbose >= 2) then
          write(nulout,'(a,g10.3)') '  Overriding cosine of the solar zenith angle with ', &
               &  driver_config%cos_sza_override
        end if
      else
        call file%get('cos_solar_zenith_angle', canopy_props%cos_sza)
        if (radiation_error()) return
      end if
    end if

    ncol = size(canopy_props%nlay)
    canopy_props%ncol = ncol
    canopy_props%ntotlay = sum(canopy_props%nlay)
    ntotlay = canopy_props%ntotlay

    call file%get('height', height)
    if (radiation_error()) return
    allocate(canopy_props%dz(canopy_props%ntotlay))
    allocate(canopy_props%istartlay(ncol))

    ilay = 1
    do jcol = 1,ncol
      canopy_props%dz(ilay:ilay+canopy_props%nlay(jcol)-1) &
           &  = height(2:canopy_props%nlay(jcol)+1,jcol) &
           &   -height(1:canopy_props%nlay(jcol),jcol)
      canopy_props%istartlay(jcol) = ilay
      ilay = ilay + canopy_props%nlay(jcol)
    end do

    call file%get('surface_type', canopy_props%i_representation)
    if (radiation_error()) return

    ! Read canopy geometry
    if (config%do_urban) then
      call read_packed_1d(file, 'building_fraction', canopy_props%nlay, &
           &              canopy_props%building_fraction)
      if (radiation_error()) return
      call read_packed_1d(file, 'building_scale', canopy_props%nlay, &
           &              canopy_props%building_scale)
      if (radiation_error()) return
      if (file%exists('veg_contact_fraction')) then
        call read_packed_1d(file, 'veg_contact_fraction', canopy_props%nlay, &
             &              canopy_props%veg_contact_fraction)
        if (radiation_error()) return
      else
        allocate(canopy_props%veg_contact_fraction(ntotlay))
        canopy_props%veg_contact_fraction = 0.0_jprb
      end if
    end if
    if (config%do_vegetation) then
      if (driver_config%vegetation_fraction >= 0.0_jprb) then
        allocate(canopy_props%veg_fraction(ntotlay))
        canopy_props%veg_fraction = driver_config%vegetation_fraction
      else
        call read_packed_1d(file, 'veg_fraction', canopy_props%nlay, &
             &              canopy_props%veg_fraction)
        if (radiation_error()) return
      end if
      call read_packed_1d(file, 'veg_extinction', canopy_props%nlay, &
           &  canopy_props%veg_ext)
      if (radiation_error()) return
      if (driver_config%vegetation_extinction >= 0.0) then
        if  (driver_config%iverbose >= 2) then
          write(nulout,'(a,g10.3)') '  Overriding vegetation extinction with ', &
               &  driver_config%vegetation_extinction
        end if
        canopy_props%veg_ext = driver_config%vegetation_extinction
      end if
      call read_packed_1d(file, 'veg_scale', canopy_props%nlay, &
           &              canopy_props%veg_scale)
      if (radiation_error()) return
      if (driver_config%vegetation_fsd >= 0.0_jprb) then
        allocate(canopy_props%veg_fsd(ntotlay))
        canopy_props%veg_fsd = driver_config%vegetation_fsd
      else
        call read_packed_1d(file, 'veg_fsd', canopy_props%nlay, &
             &              canopy_props%veg_fsd)
        if (radiation_error()) return
      end if
    end if

    if (config%do_lw) then
      ! Read ground properties needed for longwave calculations
      
      call file%get('ground_temperature', canopy_props%ground_temperature)
      if (radiation_error()) return
      if (config%do_urban) then
        call read_packed_1d(file, 'roof_temperature', canopy_props%nlay, &
             &              canopy_props%roof_temperature)
        if (radiation_error()) return
        call read_packed_1d(file, 'wall_temperature', canopy_props%nlay, &
             &              canopy_props%wall_temperature)
        if (radiation_error()) return
      end if

      call read_2d(file, 'ground_lw_emissivity', lw_spectral_props%ground_emissivity)
      if (radiation_error()) return
      if (driver_config%ground_lw_emissivity >= 0.0) then
        if  (driver_config%iverbose >= 2) then
          write(nulout,'(a,g10.3)') '  Overriding ground longwave emissivity with ', &
               &  driver_config%ground_lw_emissivity
        end if
        lw_spectral_props%ground_emissivity = driver_config%ground_lw_emissivity
      end if

      if (config%do_urban) then
        ! Read urban properties needed for longwave calculations
        
        call read_packed_2d(file, 'roof_lw_emissivity', canopy_props%nlay, lw_spectral_props%roof_emissivity)
        if (radiation_error()) return
        if (driver_config%roof_lw_emissivity >= 0.0) then
          if  (driver_config%iverbose >= 2) then
            write(nulout,'(a,g10.3)') '  Overriding roof longwave emissivity with ', &
                 &  driver_config%roof_lw_emissivity
          end if
          lw_spectral_props%roof_emissivity = driver_config%roof_lw_emissivity
        end if

        call read_packed_2d(file, 'wall_lw_emissivity', canopy_props%nlay, &
             &              lw_spectral_props%wall_emissivity)
        if (radiation_error()) return
        if (driver_config%wall_lw_emissivity >= 0.0) then
          if  (driver_config%iverbose >= 2) then
            write(nulout,'(a,g10.3)') '  Overriding wall longwave emissivity with ', &
                 &  driver_config%wall_lw_emissivity
          end if
          lw_spectral_props%wall_emissivity = driver_config%wall_lw_emissivity
        end if
      end if

      if (config%do_vegetation) then
        ! Read vegetation properties needed for longwave calculations
        call read_packed_2d(file, 'veg_lw_ssa', canopy_props%nlay, &
             &  lw_spectral_props%veg_ssa)
        if (radiation_error()) return
        if (driver_config%vegetation_lw_ssa >= 0.0) then
          if  (driver_config%iverbose >= 2) then
            write(nulout,'(a,g10.3)') '  Overriding vegetation longwave single-scattering albedo with ', &
                 &  driver_config%vegetation_lw_ssa
          end if
          lw_spectral_props%veg_ssa = driver_config%vegetation_lw_ssa
        end if
      end if

      if (config%do_vegetation .or. config%do_urban) then
        if (file%exists('clear_air_temperature')) then
          call read_packed_1d(file, 'clear_air_temperature', canopy_props%nlay, &
               &  canopy_props%clear_air_temperature)
          if (radiation_error()) return
          if (config%do_vegetation) then
            call read_packed_1d(file, 'veg_air_temperature', canopy_props%nlay, &
                 &  canopy_props%veg_air_temperature)
            if (radiation_error()) return
          end if
        else
          call read_packed_1d(file, 'air_temperature', canopy_props%nlay, &
               &  canopy_props%clear_air_temperature)
          if (radiation_error()) return
          if (config%do_vegetation) then
            if  (driver_config%iverbose >= 2) then
              write(nulout,'(a,g10.3)') '  Setting temperature of clear-air and air in vegetation to air_temperature'
            end if
            allocate(canopy_props%veg_air_temperature(ntotlay))
            canopy_props%veg_air_temperature = canopy_props%clear_air_temperature
          end if
        end if
        if (config%do_vegetation) then
          if (file%exists('veg_temperature')) then
            call read_packed_1d(file, 'veg_temperature', canopy_props%nlay, &
                 &  canopy_props%veg_temperature)
            if (radiation_error()) return
          else
            if  (driver_config%iverbose >= 2) then
              write(nulout,'(a,g10.3)') '  Setting vegetation temperature equal to air temperature'
            end if
            allocate(canopy_props%veg_temperature(ntotlay))
            canopy_props%veg_temperature = canopy_props%clear_air_temperature
          end if
        end if
        allocate(lw_spectral_props%air_ext(config%nlw, ntotlay))
        lw_spectral_props%air_ext = 1.0e-5_jprb
        allocate(lw_spectral_props%air_ssa(config%nlw, ntotlay))
        lw_spectral_props%air_ssa = 0.0_jprb
        allocate(lw_spectral_props%clear_air_planck(config%nlw, ntotlay))
        lw_spectral_props%clear_air_planck = 0.0_jprb
        if (config%do_vegetation) then
          allocate(lw_spectral_props%veg_planck(config%nlw, ntotlay))
          lw_spectral_props%veg_planck = 0.0_jprb
          allocate(lw_spectral_props%veg_air_planck(config%nlw, ntotlay))
          lw_spectral_props%veg_air_planck = 0.0_jprb
        end if
      end if

      ! Get the top-of-canopy fluxes
      if (file%exists('top_flux_lw_sw')) then
        call read_2d(file, 'top_flux_dn_lw', top_flux_dn_lw)
        if (radiation_error()) return
      else
        ! Spectral fluxes not provided; check for sky temperature
        ! which can provide a broadband longwave flux
        call read_2d(file, 'sky_temperature', top_flux_dn_lw)
        if (radiation_error()) return
        top_flux_dn_lw = StefanBoltzmann * top_flux_dn_lw**4
      end if

    end if ! do_lw

    if (config%do_sw) then
      ! Read ground properties needed for shortwave calculations
      call read_2d(file, 'ground_sw_albedo', sw_spectral_props%ground_albedo)
      if (radiation_error()) return
      if (driver_config%ground_sw_albedo >= 0.0) then
        if  (driver_config%iverbose >= 2) then
          write(nulout,'(a,g10.3)') '  Overriding ground shortwave albedo with ', &
               &  driver_config%ground_sw_albedo
        end if
        sw_spectral_props%ground_albedo = driver_config%ground_sw_albedo
      end if

      if (file%exists('ground_sw_albedo_direct')) then
        call read_2d(file, 'ground_sw_albedo_direct', sw_spectral_props%ground_albedo_dir)
        if (radiation_error()) return
      end if

      if (config%do_urban) then
        ! Read urban properties needed for shortwave calculations
        call read_packed_2d(file, 'roof_sw_albedo', canopy_props%nlay, &
             &              sw_spectral_props%roof_albedo)
        if (radiation_error()) return
        if (driver_config%roof_sw_albedo >= 0.0) then
          if  (driver_config%iverbose >= 2) then
            write(nulout,'(a,g10.3)') '  Overriding roof shortwave albedo with ', &
                 &  driver_config%roof_sw_albedo
          end if
          sw_spectral_props%roof_albedo = driver_config%roof_sw_albedo
        end if

        if (file%exists('roof_sw_albedo_direct')) then
          call read_packed_2d(file, 'roof_sw_albedo_direct', canopy_props%nlay, &
               &              sw_spectral_props%roof_albedo_dir)
          if (radiation_error()) return
        else
          if  (driver_config%iverbose >= 2) then
            write(nulout,'(a)') '  Assuming roof albedo to direct albedo is the same as to diffuse'
          end if
          allocate(sw_spectral_props%roof_albedo_dir(ubound(sw_spectral_props%roof_albedo,1),ntotlay))
          if (driver_config%roof_sw_albedo >= 0.0) then
            sw_spectral_props%roof_albedo_dir = driver_config%roof_sw_albedo
          else
            sw_spectral_props%roof_albedo_dir = sw_spectral_props%roof_albedo
          end if
        end if

        call read_packed_2d(file, 'wall_sw_albedo', canopy_props%nlay, sw_spectral_props%wall_albedo)
        if (radiation_error()) return
        if (driver_config%wall_sw_albedo >= 0.0) then
          if  (driver_config%iverbose >= 2) then
            write(nulout,'(a,g10.3)') '  Overriding wall shortwave albedo with ', &
                 &  driver_config%wall_sw_albedo
          end if
          sw_spectral_props%wall_albedo = driver_config%wall_sw_albedo
        end if

        if (file%exists('wall_sw_specular_fraction')) then
          call read_packed_2d(file, 'wall_sw_specular_fraction', canopy_props%nlay, &
               &              sw_spectral_props%wall_specular_frac)
          if (radiation_error()) return
        else
          allocate(sw_spectral_props%wall_specular_frac(ubound(sw_spectral_props%roof_albedo,1),ntotlay))
          sw_spectral_props%wall_specular_frac = 0.0_jprb
          if  (driver_config%iverbose >= 2) then
            write(nulout,'(a)') '  Assuming wall reflection is Lambertian (no specular component)'
          end if
        end if

      end if

      if (config%do_vegetation) then
        ! Read vegetation properties needed for shortwave calculations
        call read_packed_2d(file, 'veg_sw_ssa', canopy_props%nlay, &
             &  sw_spectral_props%veg_ssa)
        if (radiation_error()) return
        if (driver_config%vegetation_sw_ssa >= 0.0) then
          if  (driver_config%iverbose >= 2) then
            write(nulout,'(a,g10.3)') '  Overriding vegetation shortwave single-scattering albedo with ', &
                 &  driver_config%vegetation_sw_ssa
          end if
          sw_spectral_props%veg_ssa = driver_config%vegetation_sw_ssa
        end if
      end if

      if (config%do_vegetation .or. config%do_urban) then
        allocate(sw_spectral_props%air_ext(config%nsw, ntotlay))
        sw_spectral_props%air_ext = 1.0e-5_jprb
        allocate(sw_spectral_props%air_ssa(config%nsw, ntotlay))
        sw_spectral_props%air_ssa = 0.999_jprb
      end if

      ! Get the top-of-canopy fluxes
      if (driver_config%top_flux_dn_sw >= 0.0_jprb) then
        allocate(top_flux_dn_sw(config%nsw, ncol))
        top_flux_dn_sw = driver_config%top_flux_dn_sw
      else
        call read_2d(file, 'top_flux_dn_sw', top_flux_dn_sw)
        if (radiation_error()) return
      end if
      if (driver_config%top_flux_dn_direct_sw >= 0.0_jprb) then
        allocate(top_flux_dn_direct_sw(config%nsw, ncol))
        top_flux_dn_direct_sw = driver_config%top_flux_dn_direct_sw
      else
        call read_2d(file, 'top_flux_dn_direct_sw', top_flux_dn_direct_sw)
        if (radiation_error()) return
      end if

    end if
    
  end subroutine read_input


  subroutine read_2d(file, varname, var)

    use parkind1,                  only : jprb, jpim
    use easy_netcdf,               only : netcdf_file

    implicit none

    type(netcdf_file),            intent(in)    :: file
    character(*),                 intent(in)    :: varname
    real(kind=jprb), allocatable, intent(inout) :: var(:,:)

    real(kind=jprb), allocatable :: var_1d(:)

    if (allocated(var)) deallocate(var)
    
    if (file%get_rank(varname) == 1) then
      if (radiation_error()) return
      ! No spectral variation of the albedo/emissivity being read
      ! in: use a singleton first dimension
      call file%get(varname, var_1d)
      if (radiation_error()) return
      allocate(var(1,size(var_1d)))
      var(1,:) = var_1d
      deallocate(var_1d)
    else
      if (radiation_error()) return
      ! The first dimension of the input and output arrays is
      ! spectral band
      call file%get(varname, var)
      if (radiation_error()) return
    end if
    
  end subroutine read_2d

  subroutine read_packed_1d(file, varname, nlay, var)

    use parkind1,                  only : jprb, jpim
    use easy_netcdf,               only : netcdf_file

    implicit none

    type(netcdf_file),            intent(in)    :: file
    character(*),                 intent(in)    :: varname
    integer(kind=jpim),           intent(in)    :: nlay(:)
    real(kind=jprb), allocatable, intent(inout) :: var(:)
    
    real(kind=jprb), allocatable :: var_unpacked(:,:)
    integer(kind=jpim) :: ntotlay, ilay, jcol
    
    ntotlay = sum(nlay)
    
    call file%get(varname, var_unpacked)
    if (radiation_error()) return
    
    if (allocated(var)) deallocate(var)
      
    allocate(var(ntotlay))
      
    ilay = 1

    do jcol = 1,size(nlay)
      var(ilay:ilay+nlay(jcol)-1) = var_unpacked(1:nlay(jcol),jcol)
      ilay = ilay + nlay(jcol)
    end do
    
    deallocate(var_unpacked)

  end subroutine read_packed_1d

  subroutine read_packed_2d(file, varname, nlay, var)

    use parkind1,                  only : jprb, jpim
    use easy_netcdf,               only : netcdf_file

    implicit none

    type(netcdf_file),            intent(in)    :: file
    character(*),                 intent(in)    :: varname
    integer(kind=jpim),           intent(in)    :: nlay(:)
    real(kind=jprb), allocatable, intent(inout) :: var(:,:)

    real(kind=jprb), allocatable :: var_unpacked_2d(:,:)
    real(kind=jprb), allocatable :: var_unpacked_3d(:,:,:)
    integer(kind=jpim) :: ntotlay, ilay, jcol, nband
    
    ntotlay = sum(nlay)
    
    if (allocated(var)) deallocate(var)
    
    if (file%get_rank(varname) == 2) then
      if (radiation_error()) return
      ! No spectral variation of the albedo/emissivity being read
      ! in: use a singleton first dimension
      call file%get(varname, var_unpacked_2d)
      if (radiation_error()) return
      allocate(var(1,ntotlay))
      ilay = 1
      do jcol = 1,size(nlay)
        var(1,ilay:ilay+nlay(jcol)-1) = var_unpacked_2d(1:nlay(jcol),jcol)
        ilay = ilay + nlay(jcol)
      end do
      deallocate(var_unpacked_2d)
    else
      if (radiation_error()) return
      ! The first dimension of the input and output arrays is
      ! spectral band
      call file%get(varname, var_unpacked_3d)
      if (radiation_error()) return
      nband = size(var_unpacked_3d,1)
      allocate(var(nband,ntotlay))
      ilay = 1
      do jcol = 1,size(nlay)
        var(:,ilay:ilay+nlay(jcol)-1) = var_unpacked_3d(:,1:nlay(jcol),jcol)
        ilay = ilay + nlay(jcol)
      end do
      deallocate(var_unpacked_3d)
    end if
    
  end subroutine read_packed_2d
  
end module spartacus_surface_read_input
