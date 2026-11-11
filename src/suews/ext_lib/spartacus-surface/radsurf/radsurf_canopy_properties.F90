! radsurf_canopy_properties.f90 - Derived type for packed canopy properties
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

module radsurf_canopy_properties

  use radiation_io, only : radiation_error

  use parkind1, only : jpim, jprb

  implicit none

  ! Number of tile types
  integer(kind=jpim), parameter :: NTileTypes = 4

  ! Codes for the different type of tile
  enum, bind(c)
    enumerator :: ITileFlat = 0, &
         &        ITileForest, &
         &        ITileUrban, &
         &        ITileVegetatedUrban
  end enum

  character(len=*), parameter :: TileRepresentationName(NTileTypes) &
       &  = (/ 'Flat          ', &
       &       'Forest        ', &
       &       'Urban         ', &
       &       'VegetatedUrban' /)


  !---------------------------------------------------------------------
  ! Derived type storing a physical, non spectral, description of the
  ! properties of the surface tiles.  The data are "packed" to reduce
  ! memory usage given that many columns will not use any layers:
  ! there is a single "ntotlay" dimension indicating the total number
  ! of layers used by all the columns, and then (for example) the
  ! profile of wall temperatures for column "icol" would be obtained
  ! from
  ! wall_temperature(istartlay(icol):istartlay(icol)+nlay(icol)-1)

  type canopy_properties_type

    ! Number of layers for each column
    integer(kind=jpim), allocatable :: nlay(:) ! (ncol)

    ! Start index for layers associated with each column
    integer(kind=jpim), allocatable :: istartlay(:) ! (ncol)

    ! Layer thickness (m)
    real(kind=jprb), allocatable :: dz(:) ! (ntotlay)

    ! Cosine of solar zenith angle
    real(kind=jprb), allocatable :: cos_sza(:) ! (ncol)

    ! Skin temperature of various surfaces (K)
    real(kind=jprb), allocatable :: ground_temperature(:) ! (ncol)
    real(kind=jprb), allocatable :: roof_temperature(:)   ! (ntotlay)
    real(kind=jprb), allocatable :: wall_temperature(:)   ! (ntotlay)

    ! Air temperature in canopy, separtely specifying the temperature
    ! of the air in the clear and vegetated part of a layer, and the
    ! leaves (K)
    real(kind=jprb), allocatable :: clear_air_temperature(:) ! (ntotlay)
    real(kind=jprb), allocatable :: veg_temperature(:)       ! (ntotlay)
    real(kind=jprb), allocatable :: veg_air_temperature(:)   ! (ntotlay)

    ! Fractional coverage of buildings and vegetation
    real(kind=jprb), allocatable :: building_fraction(:) ! (ntotlay)
    real(kind=jprb), allocatable :: veg_fraction(:)      ! (ntotlay)

    ! Horizontal scale of buildings and vegetation (m)
    real(kind=jprb), allocatable :: building_scale(:) ! (ntotlay)
    real(kind=jprb), allocatable :: veg_scale(:)      ! (ntotlay)

    ! Vegetation extinction coefficient, which is treated as
    ! wavelength independent (m-1)
    real(kind=jprb), allocatable :: veg_ext(:) ! ntotlay
    
    ! Fractional standard deviation of vegetation optical depth
    real(kind=jprb), allocatable :: veg_fsd(:) ! (ntotlay)

    ! Fraction of vegetation edge in contact with buildings rather
    ! than air
    real(kind=jprb), allocatable :: veg_contact_fraction(:) ! (ntotlay)

    ! Representation codes (ITileFlat etc) for each tile: dimensioning
    ! is (ncol)
    integer(kind=jpim), allocatable :: i_representation(:)

    ! Number of columns and total number of layers
    integer(kind=jpim) :: ncol, ntotlay

  contains
    procedure :: allocate   => allocate_canopy
    procedure :: deallocate => deallocate_canopy
!    procedure :: read => read_from_netcdf

  end type canopy_properties_type


contains

  !---------------------------------------------------------------------
  subroutine allocate_canopy(this, config, ncol, ntotlay, &
       &                    i_representation)

    use yomhook,        only : lhook, dr_hook
    use radsurf_config, only : config_type

    class(canopy_properties_type), intent(inout) :: this
    type(config_type),   intent(in)    :: config
    integer(kind=jpim),  intent(in)    :: ncol, ntotlay
    integer(kind=jpim),  intent(in), optional :: i_representation(:)

    logical :: do_vegetation, do_urban, do_canopy

    real(jprb) :: hook_handle

    if (lhook) call dr_hook('radiation_canopy_properties:allocate',0,hook_handle)

    call this%deallocate()

    this%ncol     = ncol
    this%ntotlay  = ntotlay
    do_vegetation = config%do_vegetation
    do_urban      = config%do_urban

    ! If the vector i_representation is present the only allocate
    ! vegetation or urban arrays if they will be needed
    if (present(i_representation)) then
      if (.not. any(i_representation == ITileForest &
           &        .or. i_representation == ITileVegetatedUrban)) then
        do_vegetation = .false.
      end if
      if (.not. any(i_representation == ITileUrban &
           &        .or. i_representation == ITileVegetatedUrban)) then
        do_urban = .false.
      end if
    end if

    do_canopy = (do_vegetation .or. do_urban)

    allocate(this%cos_sza(ncol))
    allocate(this%nlay(ncol))
    allocate(this%istartlay(ncol))
    if (do_canopy) then
      allocate(this%dz(ntotlay))
    end if
    allocate(this%ground_temperature(ncol))
    if (do_urban) then
      allocate(this%roof_temperature(ntotlay))
      allocate(this%wall_temperature(ntotlay))
      allocate(this%building_fraction(ntotlay))
      allocate(this%building_scale(ntotlay))
    end if
    if (do_canopy) then
      allocate(this%clear_air_temperature(ntotlay))
    end if
 
    if (do_vegetation) then
      allocate(this%veg_air_temperature(ntotlay))
      allocate(this%veg_temperature(ntotlay))
      allocate(this%veg_fraction(ntotlay))
      allocate(this%veg_scale(ntotlay))
      allocate(this%veg_ext(ntotlay))
    end if

    if (config%n_vegetation_region_forest > 1 &
         &  .or. config%n_vegetation_region_urban > 1) then
      allocate(this%veg_fsd(ntotlay))
    end if

    if (do_urban .and. do_vegetation) then
      allocate(this%veg_contact_fraction(ntotlay))
    end if

    ! Create and populate representation vector inside the canopy
    ! object
    if (.not. allocated(this%i_representation)) then
      allocate(this%i_representation(ncol))
      if (present(i_representation)) then
        this%i_representation = i_representation(1:ncol)
      else
        if (do_urban) then
          if (do_vegetation) then
            this%i_representation = ITileVegetatedUrban
          else
            this%i_representation = ITileUrban
          end if
        else 
          if (do_vegetation) then
            this%i_representation = ITileForest
          else
            this%i_representation = ITileFlat
          end if         
        end if
      end if
    end if

    if (lhook) call dr_hook('radiation_canopy_properties:allocate',1,hook_handle)

  end subroutine allocate_canopy


  !---------------------------------------------------------------------
  subroutine deallocate_canopy(this)

    use yomhook, only : lhook, dr_hook

    class(canopy_properties_type), intent(inout) :: this

    real(jprb) :: hook_handle

    if (lhook) call dr_hook('radiation_canopy_properties:deallocate',0,hook_handle)

    if (allocated(this%i_representation))       deallocate(this%i_representation)
    if (allocated(this%cos_sza))                deallocate(this%cos_sza)
    if (allocated(this%ground_temperature))     deallocate(this%ground_temperature)
    if (allocated(this%roof_temperature))       deallocate(this%roof_temperature)
    if (allocated(this%wall_temperature))       deallocate(this%wall_temperature)
    if (allocated(this%clear_air_temperature))  deallocate(this%clear_air_temperature)
    if (allocated(this%veg_air_temperature))    deallocate(this%veg_air_temperature)
    if (allocated(this%veg_temperature))        deallocate(this%veg_temperature)
    if (allocated(this%building_fraction))      deallocate(this%building_fraction)
    if (allocated(this%veg_fraction))           deallocate(this%veg_fraction)
    if (allocated(this%building_scale))         deallocate(this%building_scale)
    if (allocated(this%veg_scale))              deallocate(this%veg_scale)
    if (allocated(this%veg_ext))                deallocate(this%veg_ext)
    if (allocated(this%veg_fsd))                deallocate(this%veg_fsd)
    if (allocated(this%veg_contact_fraction))   deallocate(this%veg_contact_fraction)

    if (lhook) call dr_hook('radiation_canopy_properties:deallocate',1,hook_handle)

  end subroutine deallocate_canopy


  !---------------------------------------------------------------------
  ! Print a description of the surface tile types
  subroutine print_surface_representation(i_representation)

    use radiation_io, only : nulout

    integer(kind=jpim), dimension(:), allocatable, intent(in) :: i_representation

    integer :: ntile, jtile

    write(nulout,'(a)') 'Surface tile representation:'
    if (.not. allocated(i_representation)) then
      write(nulout,'(a)') '  Simple (one flat tile)'
    else
      ntile = size(i_representation,1)
      do jtile = 1,ntile
        write(nulout,'(a,i0,a,a)') '  Tile ', jtile, ': ', trim(TileRepresentationName(i_representation(jtile)))
      end do
    end if
    
  end subroutine print_surface_representation

#ifdef HAVE_READ_ROUTINE

  !---------------------------------------------------------------------
  subroutine read_from_netcdf(this, file)

    use parkind1,           only : jprb, jpim
    use easy_netcdf,        only : netcdf_file
    
    implicit none

    type(netcdf_file),  intent(in)     :: file
    class(surface_type), intent(inout) :: this

    real(kind=jprb), allocatable       :: data_1d(:)

    integer(kind=jpim), parameter :: ipermute(3) = [2,3,1]

    call this%deallocate

    this%is_simple = .false.

    call file%get('skin_temperature', this%skin_temperature, do_transp=.true.)
    if (radiation_error()) return
    call file%get('canopy_temperature', this%canopy_temperature, do_transp=.true.)
    if (radiation_error()) return
    call file%get('sw_albedo', this%sw_albedo, ipermute=ipermute)
    if (radiation_error()) return
    if (file%exists('sw_albedo_direct')) then
      call file%get('sw_albedo', this%sw_albedo_direct, ipermute=ipermute)
      if (radiation_error()) return
    end if
    call file%get('lw_emissivity', this%lw_emissivity, ipermute=ipermute)
    if (radiation_error()) return
    call file%get('tile_representation', data_1d)
    if (radiation_error()) return
    this%ntile = size(data_1d)
    allocate(this%i_representation(this%ntile))
    this%i_representation = int(data_1d)
    call file%get('tile_fraction', this%tile_fraction, do_transp=.true.)
    if (radiation_error()) return
    call file%get('canopy_depth', this%canopy_depth, do_transp=.true.)
    if (radiation_error()) return
    call file%get('building_fraction', this%building_fraction, do_transp=.true.)
    if (radiation_error()) return
    if (file%exists('building_normalized_perimeter')) then
      call file%get('building_normalized_perimeter', &
           &  this%building_normalized_perimeter, do_transp=.true.)
      if (radiation_error()) return
    else
      ! Convert building scale to normalized perimeter
      call file%get('building_scale', this%building_normalized_perimeter, do_transp=.true.)
      if (radiation_error()) return
      this%building_normalized_perimeter = 4.0_jprb * this%building_fraction &
           &  * (1.0_jprb-this%building_fraction) / max(1.0e-8_jprb,this%building_normalized_perimeter)
    end if
    call file%get('veg_optical_depth', this%veg_optical_depth, do_transp=.true.)
    if (radiation_error()) return
    call file%get('veg_sw_albedo', this%veg_sw_albedo, ipermute=ipermute)
    if (radiation_error()) return
    call file%get('veg_lw_emissivity', this%veg_lw_emissivity, ipermute=ipermute)
    if (radiation_error()) return

    this%nregion = sum(NTileRegions(this%i_representation))
    this%ncol    = size(this%skin_temperature,1)
    this%nfacet  = size(this%skin_temperature,2)

    this%nalbedobands = size(this%sw_albedo,2)
    this%nemissbands = size(this%lw_emissivity,2)

    call this%set_facet_indices

  end subroutine read_from_netcdf

#endif

end module radsurf_canopy_properties
