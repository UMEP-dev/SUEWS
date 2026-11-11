! radsurf_save.f90 - Save results of surface radiation calculation
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

module radsurf_save

  use radiation_io, only : radiation_error

  use parkind1, only : jprb, jpim

  implicit none

  real(kind=jprb), parameter :: FillValueFlux = -9999.0_jprb

contains

  subroutine save_canopy_fluxes(file_name, config, canopy_props, &
       &                        flux_sw, flux_lw, &
       &                        iverbose, is_hdf5_file)

    use easy_netcdf
    use radiation_io,             only : nulout
    use radsurf_config,           only : config_type
    use radsurf_canopy_properties,only : canopy_properties_type
    use radsurf_canopy_flux,      only : canopy_flux_type

    character(len=*), intent(in)        :: file_name
    type(config_type), intent(in)       :: config
    type(canopy_properties_type), intent(in) :: canopy_props
    type(canopy_flux_type), intent(in)  :: flux_sw, flux_lw
    logical, optional, intent(in)       :: is_hdf5_file
    integer(kind=jpim), optional, intent(in) :: iverbose

    integer(kind=jpim)     :: i_local_verbose
    type(netcdf_file)      :: out_file
    integer(kind=jpim)     :: jcol, jlay, ncol, nlay, ninterface, itotlay
    logical                :: do_spectral_sw, do_spectral_lw
    logical                :: do_broadband_sw, do_broadband_lw

    ! Temporary variable at layer interfaces
    real(kind=jprb), allocatable :: tmp(:,:)

    if (present(iverbose)) then
      i_local_verbose = iverbose
    else
      i_local_verbose = config%iverbose
    end if

    if (config%do_sw) then
      do_spectral_sw = (config%do_save_spectral_flux &
           &            .and. flux_sw%nspec > 1)
      do_broadband_sw = config%do_save_broadband_flux
    else
      do_spectral_sw = .false.
      do_broadband_sw = .false.
    end if

    if (config%do_lw) then
      do_spectral_lw = (config%do_save_spectral_flux &
           &            .and. flux_lw%nspec > 1)
      do_broadband_sw = config%do_save_broadband_flux
    else
      do_spectral_lw = .false.
      do_broadband_sw = .false.
    end if

    ncol = canopy_props%ncol
    nlay = maxval(canopy_props%nlay)
    ninterface = nlay+1

    allocate(tmp(ninterface,ncol))

    ! Open the file
    call out_file%create(trim(file_name), iverbose=i_local_verbose, &
         &               is_hdf5_file=is_hdf5_file)
    if (radiation_error()) return

    ! Define dimensions
    call out_file%define_dimension("column", ncol)
    if (radiation_error()) return
    call out_file%define_dimension("layer", nlay)
    if (radiation_error()) return
    call out_file%define_dimension("layer_interface", ninterface)
    if (radiation_error()) return
    if (do_spectral_sw) then
      call out_file%define_dimension("band_sw", flux_sw%nspec)
      if (radiation_error()) return
    end if
    if (do_spectral_lw) then
      call out_file%define_dimension("band_lw", flux_lw%nspec)
      if (radiation_error()) return
    end if
    
    ! Put global attributes
    call out_file%put_global_attributes( &
         &  title_str="Radiative fluxes from the SPARTACUS-Surface radiation model", &
         &  references_str="Hogan, R. J., T. Quaife and R. Braghiere, 2018: Fast matrix treatment of 3-D radiative" &
         &  //" transfer in vegetation canopies: SPARTACUS-Vegetation 1.1. Geosci. Model Dev., 11, 339-350." &
         &  //NEW_LINE('A')//"Hogan, R. J., 2019b: Flexible treatment of radiative transfer in complex urban" &
         &  // " canopies for use in weather and climate models. Boundary-Layer Meteorol., 173, 53-78.", &
         &  source_str="SPARTACUS-Surface offline radiation model", &
         &  comment_str="All fluxes and absorption rates are in terms of power per unit horizontal area of the domain. " &
         &  //"Net fluxes are downwelling (or incoming) minus upwelling (or outgoing).")
    if (radiation_error()) return

    ! Define general variables
    call out_file%define_variable("height", &
         &  dim2_name="column", dim1_name="layer_interface", fill_value=-1.0_jprb, &
         &  units_str="m", long_name="Height of layer interfaces above ground", &
         &  standard_name="height")
    if (radiation_error()) return
    call out_file%define_variable("surface_type", data_type_name="short", &
         &  dim1_name="column", long_name="Surface type")
    if (radiation_error()) return
    call out_file%put_attribute("surface_type", "definition", &
         &    "0: Flat"//NEW_LINE('A') &
         &  //"1: Forest"//NEW_LINE('A') &
         &  //"2: Unvegetated urban"//NEW_LINE('A') &
         &  //"3: Vegetated urban")
    if (radiation_error()) return
    call out_file%define_variable("nlayer", data_type_name="short", &
         &  dim1_name="column", long_name="Number of active layers")
    if (radiation_error()) return

    ! Define shortwave variables
    if (config%do_sw) then
      call define_canopy_flux_variables(out_file, "sw", "shortwave", flux_sw, &
           &  do_broadband_sw, do_spectral_sw)
      if (radiation_error()) return
    end if

    ! Define longwave variables
    if (config%do_lw) then
      call define_canopy_flux_variables(out_file, "lw", "longwave", flux_lw, &
           &  do_broadband_lw, do_spectral_lw)
      if (radiation_error()) return
     end if

    ! Write general variables
    tmp = -1.0_jprb
    tmp(1,:) = 0.0_jprb ! Surface height is zero m
    itotlay = 1
    do jcol = 1,ncol
      do jlay = 1,canopy_props%nlay(jcol)
        tmp(jlay+1,jcol) = tmp(jlay,jcol) + canopy_props%dz(itotlay)
        itotlay = itotlay + 1
      end do
    end do
    call out_file%put("height", tmp)
    if (radiation_error()) return
    call out_file%put("surface_type", canopy_props%i_representation)
    if (radiation_error()) return
    call out_file%put("nlayer", canopy_props%nlay)
    if (radiation_error()) return

    ! Write shortwave variables
    if (config%do_sw) then
      call write_canopy_flux_variables(out_file, "sw", nlay, &
       &  canopy_props%nlay, flux_sw, do_broadband_sw, do_spectral_sw)
      if (radiation_error()) return
    end if

    ! Write longwave variables
    if (config%do_lw) then
      call write_canopy_flux_variables(out_file, "lw", nlay, &
       &  canopy_props%nlay, flux_lw, do_broadband_lw, do_spectral_lw)
      if (radiation_error()) return
    end if

    ! Close file
    call out_file%close()
    if (radiation_error()) return

  end subroutine save_canopy_fluxes

  subroutine define_canopy_flux_variables(out_file, band_name, band_long_name, &
       &  flux, do_broadband, do_spectral)

    use easy_netcdf
    use radsurf_canopy_flux,      only : canopy_flux_type

    type(netcdf_file),      intent(inout) :: out_file
    character(len=*),       intent(in)    :: band_name, band_long_name
    type(canopy_flux_type), intent(in)    :: flux
    logical,                intent(in)    :: do_broadband, do_spectral

    call out_file%define_variable("ground_flux_dn_"//band_name, units_str="W m-2", &
         &  long_name="Downwelling "//band_long_name//" flux at ground", &
         &  dim1_name="column")
    if (radiation_error()) return
    call out_file%define_variable("ground_flux_net_"//band_name, units_str="W m-2", &
         &  long_name="Net "//band_long_name//" flux at ground", &
         &  dim1_name="column")
    if (radiation_error()) return
    if (allocated(flux%ground_dn_dir)) then
      call out_file%define_variable("ground_flux_dn_direct_"//band_name, units_str="W m-2", &
           &  long_name="Downwelling direct "//band_long_name//" flux at ground", &
           &  dim1_name="column")
      if (radiation_error()) return
      call out_file%define_variable("ground_flux_vertical_diffuse_"//band_name, units_str="W m-2", &
           &  long_name="Diffuse "//band_long_name//" flux into a vertical surface at ground level", &
           &  dim1_name="column")
      if (radiation_error()) return
    else
      call out_file%define_variable("ground_flux_vertical_"//band_name, units_str="W m-2", &
           &  long_name="Flux in "//band_long_name//" into a vertical surface at ground level", &
           &  dim1_name="column")
      if (radiation_error()) return
    end if
    call out_file%define_variable("top_flux_dn_"//band_name, units_str="W m-2", &
         &  long_name="Downwelling "//band_long_name//" flux at top of canopy", &
         &  dim1_name="column")
    if (radiation_error()) return
    call out_file%define_variable("top_flux_net_"//band_name, units_str="W m-2", &
         &  long_name="Net "//band_long_name//" flux at top of canopy", &
         &  dim1_name="column")
    if (radiation_error()) return
    if (allocated(flux%top_dn_dir)) then
      call out_file%define_variable("top_flux_dn_direct_"//band_name, units_str="W m-2", &
           &  long_name="Downwelling direct "//band_long_name//" flux at top of canopy", &
           &  dim1_name="column")
      if (radiation_error()) return
    end if
    if (allocated(flux%roof_in)) then
     call out_file%define_variable("roof_flux_in_"//band_name, units_str="W m-2", &
         &  long_name="Incoming "//band_long_name//" flux at roofs", &
         &  dim2_name="column", dim1_name="layer", fill_value=FillValueFlux)
     if (radiation_error()) return
     call out_file%define_variable("roof_flux_net_"//band_name, units_str="W m-2", &
         &  long_name="Net "//band_long_name//" flux at roofs", &
         &  dim2_name="column", dim1_name="layer", fill_value=FillValueFlux)
     if (radiation_error()) return
     call out_file%define_variable("wall_flux_in_"//band_name, units_str="W m-2", &
         &  long_name="Incoming "//band_long_name//" flux at walls", &
         &  dim2_name="column", dim1_name="layer", fill_value=FillValueFlux)
     if (radiation_error()) return
     call out_file%define_variable("wall_flux_net_"//band_name, units_str="W m-2", &
         &  long_name="Net "//band_long_name//" flux at walls", &
         &  dim2_name="column", dim1_name="layer", fill_value=FillValueFlux)
     if (radiation_error()) return
    end if
    if (allocated(flux%clear_air_abs)) then
      call out_file%define_variable("clear_air_absorption_"//band_name, units_str="W m-2", &
         &  long_name="Absorbed "//band_long_name//" in clear air", &
         &  dim2_name="column", dim1_name="layer", fill_value=FillValueFlux)
      if (radiation_error()) return
    end if

    if (allocated(flux%veg_abs)) then
      call out_file%define_variable("veg_absorption_"//band_name, units_str="W m-2", &
         &  long_name="Absorbed "//band_long_name//" by vegetation", &
         &  dim2_name="column", dim1_name="layer", fill_value=FillValueFlux)
      if (radiation_error()) return
      call out_file%define_variable("veg_air_absorption_"//band_name, units_str="W m-2", &
         &  long_name="Absorbed "//band_long_name//" by air in vegetated regions", &
         &  dim2_name="column", dim1_name="layer", fill_value=FillValueFlux)
      if (radiation_error()) return
    end if

  end subroutine define_canopy_flux_variables


  subroutine write_canopy_flux_variables(out_file, band_name, nmaxlay, &
       &  nlay, flux, do_broadband, do_spectral)

    use easy_netcdf
    use radsurf_canopy_flux,      only : canopy_flux_type

    type(netcdf_file),      intent(inout) :: out_file
    character(len=*),       intent(in)    :: band_name
    integer(kind=jpim),     intent(in)    :: nmaxlay
    integer(kind=jpim),     intent(in)    :: nlay(:)
    type(canopy_flux_type), intent(in)    :: flux
    logical,                intent(in)    :: do_broadband, do_spectral

    ! Temporary variable at layer interfaces
    real(kind=jprb) :: tmp(nmaxlay, flux%ncol)
    
    call out_file%put("ground_flux_dn_"//band_name, sum(flux%ground_dn,1))
    if (radiation_error()) return
    call out_file%put("ground_flux_net_"//band_name, sum(flux%ground_net,1))
    if (radiation_error()) return
    if (allocated(flux%ground_dn_dir)) then
      call out_file%put("ground_flux_dn_direct_"//band_name, &
           &  sum(flux%ground_dn_dir,1))
      if (radiation_error()) return
      call out_file%put("ground_flux_vertical_diffuse_"//band_name, &
           &  sum(flux%ground_vertical_diff,1))
      if (radiation_error()) return
    else
      call out_file%put("ground_flux_vertical_"//band_name, &
           &  sum(flux%ground_vertical_diff,1))
      if (radiation_error()) return
    end if
    call out_file%put("top_flux_dn_"//band_name, sum(flux%top_dn,1))
    if (radiation_error()) return
    call out_file%put("top_flux_net_"//band_name, sum(flux%top_net,1))
    if (radiation_error()) return
    if (allocated(flux%top_dn_dir)) then
      call out_file%put("top_flux_dn_direct_"//band_name, &
           &  sum(flux%top_dn_dir,1))
      if (radiation_error()) return
    end if
    if (allocated(flux%roof_in)) then
      call unpack_variable_broadband(flux%ncol, nmaxlay, nlay, FillValueFlux, &
           &  flux%roof_in, tmp)
      call out_file%put("roof_flux_in_"//band_name, tmp)
      if (radiation_error()) return
      call unpack_variable_broadband(flux%ncol, nmaxlay, nlay, FillValueFlux, &
           &  flux%roof_net, tmp)
      call out_file%put("roof_flux_net_"//band_name, tmp)
      if (radiation_error()) return
      call unpack_variable_broadband(flux%ncol, nmaxlay, nlay, FillValueFlux, &
           &  flux%wall_in, tmp)
      call out_file%put("wall_flux_in_"//band_name, tmp)
      if (radiation_error()) return
      call unpack_variable_broadband(flux%ncol, nmaxlay, nlay, FillValueFlux, &
           &  flux%wall_net, tmp)
      call out_file%put("wall_flux_net_"//band_name, tmp)
      if (radiation_error()) return
    end if
    if (allocated(flux%clear_air_abs)) then
        call unpack_variable_broadband(flux%ncol, nmaxlay, nlay, FillValueFlux, &
           &  flux%clear_air_abs, tmp)
      call out_file%put("clear_air_absorption_"//band_name, tmp)    
      if (radiation_error()) return
    end if
    if (allocated(flux%veg_abs)) then
        call unpack_variable_broadband(flux%ncol, nmaxlay, nlay, FillValueFlux, &
           &  flux%veg_abs, tmp)
      call out_file%put("veg_absorption_"//band_name, tmp)
      if (radiation_error()) return
         call unpack_variable_broadband(flux%ncol, nmaxlay, nlay, FillValueFlux, &
           &  flux%veg_air_abs, tmp)
      call out_file%put("veg_air_absorption_"//band_name, tmp)
      if (radiation_error()) return
   end if

  end subroutine write_canopy_flux_variables

  subroutine unpack_variable_broadband(ncol, nmaxlay, nlay, fill_value, var_in, var_out)

    integer(kind=jpim), intent(in)  :: ncol, nmaxlay, nlay(:)
    real(kind=jprb),    intent(in)  :: fill_value
    real(kind=jprb),    intent(in)  :: var_in(:,:)
    real(kind=jprb),    intent(out) :: var_out(nmaxlay,ncol)
    
    integer :: jcol, jlay, itotlay

    var_out = fill_value
    
    itotlay = 1
    
    do jcol = 1,ncol
      do jlay = 1,nlay(jcol)
        var_out(jlay,jcol) = sum(var_in(:,itotlay),1)
        itotlay = itotlay + 1
      end do
    end do

  end subroutine unpack_variable_broadband


end module radsurf_save
