MODULE SPARTACUS_MODULE
   !==============================================================================
   !NET ALL WAVE RADIATION PARAMETERIZATION ROUTINES
   !B. OFFERLE
   !DEPT OF GEOGRAPHY
   !INDIANA UNIVERSITY
   !bofferle@indiana.edu
   !
   !MODIFIED: 19 DEC 2002
   !CURRENTLY THE SMITH GRID IS ONLY VALID FOR THE N. HEMISPHERE
   !
   !Thomas Loridan, May 2008
   !4.1: MODIFICATION FOR CLOUD FRACTION PARAMETERIZATION AT NIGHT USING THE RATE OF COOLING.
   !     EOSHIFT INTRINSIC FUNCTION WAS ALSO REMOVED BECAUSE IT IS COMPILER DEPENDENT.
   !
   !6.0  T. Loridan - June 2009
   !     Different longwave down options (ldown_option)
   ! 1 - LDOWN Observed (add data as last column in met file)
   ! 2 - LDOWN modelled from observed FCLD (add data as last column in met file)
   ! 3 - LDOWN modelled from FCLD(RH,TA)
   ! 4 - LDOWN modelled from FCLD(Kdown); i.e. day FCLD only
   !     cloud fraction is kept constant throught the night (Offerle et al. 2003, JAM)
   ! 5 - Option 3 at night and 4 during the day (might cause discontinuities in LDOWN)

   !SUEWS   L. Jarvi - Oct 2010
   !Currently LDOWN options 4 and 5 commented out in order to reduce input files.
   !Last modified:
   ! TS 06 Aug 2017 - interface modified to receive explict input and output arguments
   ! LJ 27 Jan 2016 - Removal of tabs, cleaning of the code
   ! FL July 2014 - Variables are moved to modules in NARP subroutine. Snow related should also in future.
   ! FL Nov 2013 - A new sun postion algorithm added
   ! LJ May 2013 - Main program NARP changed to take subsurfaces and snow into account here and not
   ! in the main program
   ! LJ Oct 2012 - Zenith angle change in the calculation of albedo added
   ! sg feb 2012 - Allocatable array module added

   !==============================================================================================
   USE allocateArray, ONLY: NSURF, NVegSurf, nspec, nsw, nlw, ncol, &
                            ConifSurf, DecidSurf, BldgSurf, PavSurf, GrassSurf, BSoilSurf, WaterSurf
   USE PhysConstants, ONLY: SBConst

   IMPLICIT NONE

CONTAINS
   SUBROUTINE SPARTACUS_Initialise
      USE data_in, ONLY: fileinputpath
      USE allocateArray
      IMPLICIT NONE
      ! INTEGER :: n_vegetation_region_urban, &
      !            n_stream_sw_urban, n_stream_lw_urban
      ! REAL(KIND(1D0)) :: sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
      !                    veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
      !                    veg_fsd_const, veg_contact_fraction_const, &
      !                    ground_albedo_dir_mult_fact
      ! LOGICAL :: use_sw_direct_albedo
      NAMELIST /Spartacus_Settings/ use_sw_direct_albedo, n_vegetation_region_urban, &
         n_stream_sw_urban, n_stream_lw_urban &
         /Spartacus_Constant_Parameters/ sw_dn_direct_frac, air_ext_sw, air_ssa_sw, veg_ssa_sw, air_ext_lw, &
         air_ssa_lw, veg_ssa_lw, veg_fsd_const, veg_contact_fraction_const, ground_albedo_dir_mult_fact

      ! Bring in SUEWS-SPARTACUS.nml settings and parameters
      OPEN (511, file=TRIM(FileInputPath)//'SUEWS_SPARTACUS.nml', status='old')
      READ (511, nml=Spartacus_Settings)
      READ (511, nml=Spartacus_Constant_Parameters)
      CLOSE (511)

   END SUBROUTINE SPARTACUS_Initialise

   SUBROUTINE SPARTACUS( &
      DiagQN, & !input:
      sfr_surf, zenith_deg, nlayer, & !input:
      tsfc_surf, tsfc_roof, tsfc_wall, &
      kdown, ldown, Tair_C, alb_surf, emis_surf, LAI_id, &
      n_vegetation_region_urban, &
      n_stream_sw_urban, n_stream_lw_urban, &
      sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
      veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
      veg_fsd_const, veg_contact_fraction_const, &
      ground_albedo_dir_mult_fact, use_sw_direct_albedo, &
      height, building_frac, veg_frac, sfr_roof, sfr_wall, &
      building_scale, veg_scale, & !input:
      alb_roof, emis_roof, alb_wall, emis_wall, &
      roof_albedo_dir_mult_fact, wall_specular_frac, &
      qn, kup, lup, qn_roof, qn_wall, qn_surf, & !output:
      roof_in_sw_spc, roof_in_lw_spc, &
      wall_in_sw_spc, wall_in_lw_spc, &
      dataOutLineSPARTACUS)
      USE parkind1, ONLY: jpim, jprb
      USE radsurf_interface, ONLY: radsurf
      USE radsurf_config, ONLY: config_type
      ! USE spartacus_surface_config, ONLY: read_config_from_namelist, driver_config_type
      USE radsurf_canopy_properties, ONLY: canopy_properties_type
      USE radsurf_sw_spectral_properties, ONLY: sw_spectral_properties_type
      USE radsurf_lw_spectral_properties, ONLY: lw_spectral_properties_type
      USE radsurf_boundary_conds_out, ONLY: boundary_conds_out_type
      USE radsurf_canopy_flux, ONLY: canopy_flux_type
      USE radsurf_simple_spectrum, ONLY: calc_simple_spectrum_lw
      ! USE data_in, ONLY: fileinputpath
      USE allocateArray, ONLY: ncolumnsDataOutSPARTACUS

      IMPLICIT NONE

      !!!!!!!!!!!!!! Set objects and variables !!!!!!!!!!!!!!

      ! Input parameters and variables from SUEWS
      REAL(KIND(1D0)), INTENT(IN) :: zenith_deg
      INTEGER, INTENT(IN) :: DiagQN
      INTEGER, INTENT(IN) :: nlayer

      ! TODO: tsurf_0 and temp_C need to be made vertically distributed
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: tsfc_roof, tsfc_wall
      REAL(KIND(1D0)), INTENT(IN) :: Tair_C

      REAL(KIND(1D0)), INTENT(IN) :: kdown
      REAL(KIND(1D0)), INTENT(IN) :: ldown
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: tsfc_surf

      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: sfr_surf, alb_surf, emis_surf
      REAL(KIND(1D0)), DIMENSION(NVegSurf), INTENT(IN) :: LAI_id

      ! SPARTACUS configuration parameters
      INTEGER, INTENT(IN) :: n_vegetation_region_urban, &
                             n_stream_sw_urban, n_stream_lw_urban
      REAL(KIND(1D0)), INTENT(IN) :: sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
                                     veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
                                     veg_fsd_const, veg_contact_fraction_const, &
                                     ground_albedo_dir_mult_fact
      ! INTEGER(kind=jpim) :: ncol
      ! INTEGER(kind=jpim) :: nlayer
      INTEGER(kind=jpim), ALLOCATABLE :: i_representation(:)
      INTEGER(kind=jpim), ALLOCATABLE :: nlay(:)
      INTEGER :: istartcol, iendcol
      INTEGER :: jrepeat, ilay, jlay, jcol

      ! --------------------------------------------------------------------------------
      ! output variables
      ! --------------------------------------------------------------------------------
      ! these will be used by other SUEWS calculations
      REAL(KIND(1D0)), INTENT(OUT) :: qn, kup, lup
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: qn_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: qn_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: qn_surf
      REAL(KIND(1D0)) :: sw_net_grnd
      REAL(KIND(1D0)) :: lw_net_grnd
      REAL(KIND(1D0)) :: sw_dn_grnd
      REAL(KIND(1D0)) :: lw_dn_grnd
      REAL(KIND(1D0)) :: lw_up_grnd
      REAL(KIND(1D0)), DIMENSION(NSURF - 1) :: qn_grnd_ind
      REAL(KIND(1D0)), DIMENSION(NSURF - 1) :: alb_grnd_ind
      REAL(KIND(1D0)), DIMENSION(NSURF - 1) :: emis_grnd_ind
      REAL(KIND(1D0)), DIMENSION(NSURF - 1) :: sfr_grnd_ind
      REAL(KIND(1D0)), DIMENSION(nsurf - 1) :: sw_net_grnd_ind
      REAL(KIND(1D0)), DIMENSION(nsurf - 1) :: lw_net_grnd_ind
      REAL(KIND(1D0)), DIMENSION(NSURF - 1) :: tsfc_grnd_ind_K
      ! --------------------------------------------------------------------------------
      ! these will be in the SPARTACUS output array
      REAL(KIND(1D0)) :: alb_spc, emis_spc, lw_emission_spc, lw_up_spc, sw_up_spc, qn_spc
      REAL(KIND(1D0)) :: top_net_lw_spc
      REAL(KIND(1D0)) :: grnd_net_lw_spc
      REAL(KIND(1D0)) :: top_dn_lw_spc
      REAL(KIND(1D0)) :: top_dn_dir_sw_spc
      REAL(KIND(1D0)) :: top_net_sw_spc
      REAL(KIND(1D0)) :: grnd_dn_dir_sw_spc
      REAL(KIND(1D0)) :: grnd_net_sw_spc
      REAL(KIND(1D0)) :: grnd_vertical_diff
      REAL(KIND(1D0)), DIMENSION(15) :: clear_air_abs_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: clear_air_abs_sw_spc
      REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: roof_in_sw_spc
      REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: roof_in_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: roof_net_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: roof_net_lw_spc
      REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: wall_in_sw_spc
      REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: wall_in_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: wall_net_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: wall_net_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: sfr_roof_spc
      REAL(KIND(1D0)), DIMENSION(15) :: sfr_wall_spc
      ! --------------------------------------------------------------------------------

      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5), INTENT(OUT) :: dataOutLineSPARTACUS

      ! Derived types for the inputs to the radiation scheme
      TYPE(config_type) :: config
      TYPE(canopy_properties_type) :: canopy_props
      TYPE(sw_spectral_properties_type) :: sw_spectral_props
      TYPE(lw_spectral_properties_type) :: lw_spectral_props
      TYPE(boundary_conds_out_type) :: bc_out
      TYPE(canopy_flux_type) :: sw_norm_dir ! SW fluxes normalized by top-of-canopy direct
      TYPE(canopy_flux_type) :: sw_norm_diff ! SW fluxes normalized by top-of-canopy diffuse
      TYPE(canopy_flux_type) :: lw_internal ! LW fluxes from internal emission
      TYPE(canopy_flux_type) :: lw_norm ! LW fluxes normalized by top-of-canopy down
      TYPE(canopy_flux_type) :: lw_flux ! Total lw canopy fluxes
      TYPE(canopy_flux_type) :: sw_flux ! Total sw canopy fluxes

      ! Top-of-canopy downward radiation, all dimensioned (nspec, ncol)
      REAL(KIND(1D0)), ALLOCATABLE :: top_flux_dn_sw(:, :) ! Total shortwave (direct+diffuse)
      REAL(KIND(1D0)), ALLOCATABLE :: top_flux_dn_direct_sw(:, :) ! ...diffuse only
      REAL(KIND(1D0)), ALLOCATABLE :: top_flux_dn_lw(:, :) ! longwave

      ! surface temperature and air temperature in Kelvin
      REAL(KIND(1D0)), DIMENSION(nlayer) :: tsfc_roof_K, tsfc_wall_K
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_surf_K
      REAL(KIND(1D0)) :: tair_K
      ! top-of-canopy diffuse sw downward
      REAL(KIND(1D0)) :: top_flux_dn_diffuse_sw
      ! plan area weighted albedo and emissivity of surfaces not including buildings and trees
      REAL(KIND(1D0)) :: alb_no_tree_bldg, emis_no_tree_bldg
      ! vegetation emissivity
      ! REAL(KIND(1D0)) :: veg_emis
      ! area weighted LAI of trees
      REAL(KIND(1D0)), ALLOCATABLE :: LAI_av(:)
      ! area weighted LAI of trees in each layer
      REAL(KIND(1D0)), ALLOCATABLE :: LAI_av_z(:)
      REAL(KIND(1D0)), ALLOCATABLE :: veg_ext(:)
      ! depth of the vegetated layer
      REAL(KIND(1D0)), ALLOCATABLE :: veg_depth(:)

      LOGICAL, INTENT(IN) :: use_sw_direct_albedo

      REAL(KIND(1D0)), DIMENSION(nlayer + 1), INTENT(IN) :: height
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac ! cumulative building fraction at each layer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_frac
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: sfr_roof ! individual surface fraction of roofs at each layer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: sfr_wall ! individual surface fraction of walls at each layer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_scale
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_wall
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: roof_albedo_dir_mult_fact
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: wall_specular_frac

      ! parameters to pass into the radiation scheme
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer) :: roof_albedo
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer) :: wall_albedo
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer) :: roof_emissivity
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer) :: wall_emissivity
      REAL(KIND(1D0)), DIMENSION(nlayer) :: veg_fsd, veg_contact_fraction
      ! INTEGER :: i
      ! REAL(KIND(1D0)) :: debug1, debug2

      IF (DiagQN == 1) PRINT *, 'in SPARTACUS, starting ...'
      ! initialize the output variables
      dataOutLineSPARTACUS = -999.

      sfr_roof_spc = -999.
      sfr_roof_spc(1:nlayer) = sfr_roof
      sfr_wall_spc = -999.
      sfr_wall_spc(1:nlayer) = sfr_wall

      ALLOCATE (nlay(ncol))
      ! nlay = [nlayers] ! modified to follow ESTM_ext convention
      nlay = [nlayer]
      ALLOCATE (veg_ext(nlayer))

      !Set the values of profiles that are implemented as being constant with height
      ! veg_frac(:) = veg_frac_const
      veg_fsd(:) = veg_fsd_const
      veg_contact_fraction(:) = veg_contact_fraction_const

      ! Set the values of the albedo/emissivity profiles; note the dimension
      roof_albedo(nspec, :) = alb_roof
      wall_albedo(nspec, :) = alb_wall
      roof_emissivity(nspec, :) = emis_roof
      wall_emissivity(nspec, :) = emis_wall
      ! print *, 'emis_wall in su', emis_wall
      ! print *, 'wall_emissivity(nspec, :) in su', wall_emissivity(nspec, :)

      !!!!!!!!!!!!!! Model configuration !!!!!!!!!!!!!!
      IF (DiagQN == 1) PRINT *, 'in SPARTACUS, setting up model ...'
      ! CALL config%READ(file_name=TRIM(FileInputPath)//'SUEWS_SPARTACUS.nml')
      config%do_sw = .TRUE.
      config%do_lw = .TRUE.
      config%use_sw_direct_albedo = use_sw_direct_albedo
      ALLOCATE (i_representation(ncol))
      IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > 0.0 .AND. sfr_surf(BldgSurf) > 0.0) THEN
         config%do_vegetation = .TRUE.
         i_representation = [3]
         config%do_urban = .TRUE.
      ELSE IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) == 0.0 .AND. sfr_surf(BldgSurf) > 0.0) THEN
         config%do_vegetation = .FALSE.
         i_representation = [2]
         config%do_urban = .TRUE.
      ELSE IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > 0.0 .AND. sfr_surf(BldgSurf) == 0.0) THEN
         config%do_vegetation = .TRUE.
         i_representation = [1]
         config%do_urban = .FALSE.
      ELSE
         config%do_vegetation = .FALSE.
         i_representation = [0]
         config%do_urban = .FALSE.
      END IF
      config%iverbose = 3
      config%n_vegetation_region_urban = n_vegetation_region_urban
      config%n_vegetation_region_forest = n_vegetation_region_urban ! use the same complexity for urban as forests
      config%nsw = nsw
      config%nlw = nlw
      config%n_stream_sw_urban = n_stream_sw_urban
      config%n_stream_lw_urban = n_stream_lw_urban
      config%n_stream_sw_forest = n_stream_sw_urban ! use the same complexity for urban as forests
      config%n_stream_lw_forest = n_stream_lw_urban ! use the same complexity for urban as forests
      CALL config%consolidate()

      !!!!!!!!!!!!!! allocate and set canopy_props !!!!!!!!!!!!!!

      ! allocate
      CALL canopy_props%DEALLOCATE()
      CALL canopy_props%ALLOCATE(config, ncol, nlayer, i_representation)

      ! set cos_sza, nlay, ncol, ntotlay
      canopy_props%cos_sza = COS(zenith_deg*3.1415927/180)
      canopy_props%nlay = nlay
      canopy_props%ncol = ncol
      canopy_props%ntotlay = nlayer

      IF (DiagQN == 1) PRINT *, 'in SPARTACUS, calculating dz array ...'
      ! calculate dz array
      ilay = 1
      DO jcol = 1, ncol
         canopy_props%dz(ilay:ilay + canopy_props%nlay(jcol) - 1) &
              &  = height(ilay + 1:ilay + canopy_props%nlay(jcol)) &
              &   - height(ilay:ilay + canopy_props%nlay(jcol) - 1)
         canopy_props%istartlay(jcol) = ilay
         ilay = ilay + canopy_props%nlay(jcol)
      END DO

      ALLOCATE (LAI_av(ncol))
      ALLOCATE (veg_depth(ncol))
      ALLOCATE (LAI_av_z(nlayer))
      !Calculate the area weighted LAI of trees
      DO jcol = 1, ncol
         ! the 10.**-10 stops the equation blowing up when there are no trees
         LAI_av(jcol) = &
            (sfr_surf(ConifSurf)*LAI_id(1) + sfr_surf(DecidSurf)*LAI_id(2)) &
            /(sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + 10.**(-10))
      END DO
      ! print *, 'height in spartacus', height
      ! print *, 'building_frac in spartacus', building_frac
      ! print *, 'veg_frac in spartacus', veg_frac
      ! print *, 'building_scale in spartacus', building_scale
      ! print *, 'veg_scale in spartacus', veg_scale
      ! find veg_depth
      DO jcol = 1, ncol
         ilay = canopy_props%istartlay(jcol)
         veg_depth(jcol) = 0.
         DO jlay = 0, nlay(jcol) - 1
            IF (veg_frac(ilay + jlay) > 0.) THEN
               veg_depth(jcol) = veg_depth(jcol) + canopy_props%dz(ilay + jlay)
            END IF
         END DO
      END DO
      ! find LAV_av_z and veg_ext. Assume the LAI is uniform with height within the vegetation layer.
      DO jcol = 1, ncol
         ilay = canopy_props%istartlay(jcol)
         ! PRINT *, 'jcol', jcol
         ! PRINT *, 'ilay', ilay
         DO jlay = 0, nlay(jcol) - 1
            ! PRINT *, 'jlay', jlay
            ! PRINT *, 'veg_frac(ilay + jlay)', veg_frac(ilay + jlay)
            ! PRINT *, 'LAI_av(jcol)', LAI_av(jcol)
            ! PRINT *, 'canopy_props%dz(ilay + jlay)', canopy_props%dz(ilay + jlay)
            IF (veg_frac(ilay + jlay) > 0.) THEN
               LAI_av_z(ilay + jlay) = LAI_av(jcol)*canopy_props%dz(ilay + jlay)/veg_depth(jcol)
               veg_ext(ilay + jlay) = LAI_av_z(ilay + jlay)/(2*canopy_props%dz(ilay))
            END IF
         END DO
      END DO

      ! set temperature
      tsfc_surf_K = tsfc_surf + 273.15 ! convert surface temperature to Kelvin
      tsfc_roof_K = tsfc_roof + 273.15 ! convert surface temperature to Kelvin
      tsfc_wall_K = tsfc_wall + 273.15 ! convert surface temperature to Kelvin
      tair_K = Tair_C + 273.15 ! convert air temperature to Kelvin

      ! set ground temperature as the area-weighted average of the surface temperature of all land covers but buildings
      canopy_props%ground_temperature = (DOT_PRODUCT(tsfc_surf_K, sfr_surf) - tsfc_surf_K(BldgSurf)*sfr_surf(BldgSurf)) &
                                        /(1 - sfr_surf(BldgSurf))

      canopy_props%roof_temperature = tsfc_roof_K
      canopy_props%wall_temperature = tsfc_wall_K
      canopy_props%clear_air_temperature = tair_K
      IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > 0.0) THEN
         canopy_props%veg_temperature = DOT_PRODUCT(tsfc_surf_K(ConifSurf:DecidSurf), sfr_surf(ConifSurf:DecidSurf))
         canopy_props%veg_air_temperature = tair_K
      END IF

      ! set building and vegetation properties
      canopy_props%i_representation = i_representation
      canopy_props%building_scale = building_scale(:) ! diameter of buildings (m). The only L method for buildings is Eq. 19 Hogan et al. 2018.
      canopy_props%building_fraction = building_frac(:) ! building fraction
      IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > 0.0) THEN
         canopy_props%veg_fraction = veg_frac(:) ! evergreen + deciduous fractions
         canopy_props%veg_scale = veg_scale(:) ! scale of tree crowns (m). Using the default use_symmetric_vegetation_scale_urban=.TRUE. so that Eq. 20 Hogan et al. 2018 is used for L.
         canopy_props%veg_ext = veg_ext(:)
         canopy_props%veg_fsd = veg_fsd(:)
         canopy_props%veg_contact_fraction = veg_contact_fraction(:)
      END IF

      !!!!!!!!!!!!!! allocate and set canopy top forcing !!!!!!!!!!!!!!
      IF (DiagQN == 1) PRINT *, 'in SPARTACUS, setting canopy top forcing ...'
      ALLOCATE (top_flux_dn_sw(nspec, ncol))
      ALLOCATE (top_flux_dn_direct_sw(nspec, ncol))
      ALLOCATE (top_flux_dn_lw(nspec, ncol))
      top_flux_dn_sw = kdown ! diffuse + direct
      top_flux_dn_direct_sw = sw_dn_direct_frac*kdown ! Berrizbeitia et al. 2020 say the ratio diffuse/direct is 0.55 for Berlin and Brussels on av annually
      top_flux_dn_diffuse_sw = top_flux_dn_sw(nspec, ncol) - top_flux_dn_direct_sw(nspec, ncol)
      top_flux_dn_lw = ldown

      !!!!!!!!!!!!!! allocate and set sw_spectral_props !!!!!!!!!!!!!!

      CALL sw_spectral_props%DEALLOCATE()
      CALL sw_spectral_props%ALLOCATE(config, ncol, nlayer, nspec, canopy_props%i_representation)

      ! albedo of the ground
      alb_no_tree_bldg = (alb_surf(1)*sfr_surf(PavSurf) + alb_surf(5)*sfr_surf(GrassSurf) + &
                          alb_surf(6)*sfr_surf(BSoilSurf) + alb_surf(7)*sfr_surf(WaterSurf))/ &
                         (sfr_surf(PavSurf) + sfr_surf(GrassSurf) + sfr_surf(BSoilSurf) + sfr_surf(WaterSurf))
      sw_spectral_props%air_ext = air_ext_sw
      sw_spectral_props%air_ssa = air_ssa_sw
      IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > 0.0) THEN
         sw_spectral_props%veg_ssa = veg_ssa_sw
      END IF
      sw_spectral_props%ground_albedo = alb_no_tree_bldg ! albedo excluding buildings and trees
      sw_spectral_props%roof_albedo = roof_albedo(nspec, ncol) ! albedo of buildings
      sw_spectral_props%wall_albedo = wall_albedo(nspec, ncol) ! albedo of buildings
      sw_spectral_props%wall_specular_frac = wall_specular_frac(nspec, ncol)
      IF (config%use_sw_direct_albedo) THEN
         sw_spectral_props%ground_albedo_dir = alb_no_tree_bldg*ground_albedo_dir_mult_fact
         sw_spectral_props%roof_albedo_dir = roof_albedo(nspec, ncol)*roof_albedo_dir_mult_fact(nspec, ncol)
      END IF

      !!!!!!!!!!!!!! allocate and set lw_spectral_props !!!!!!!!!!!!!!

      CALL lw_spectral_props%DEALLOCATE()
      CALL lw_spectral_props%ALLOCATE(config, nspec, ncol, nlayer, canopy_props%i_representation)

      emis_no_tree_bldg = (emis_surf(1)*sfr_surf(PavSurf) + emis_surf(5)*sfr_surf(GrassSurf) + &
                           emis_surf(6)*sfr_surf(BSoilSurf) + emis_surf(7)*sfr_surf(WaterSurf))/ &
                          (sfr_surf(PavSurf) + sfr_surf(GrassSurf) + sfr_surf(BSoilSurf) + sfr_surf(WaterSurf)) ! emissivity of the ground
      lw_spectral_props%air_ext = air_ext_lw
      lw_spectral_props%air_ssa = air_ssa_lw
      IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > 0.0) THEN
         lw_spectral_props%veg_ssa = veg_ssa_lw
      END IF
      lw_spectral_props%ground_emissivity = emis_no_tree_bldg ! emissivity excluding buildings and trees
      lw_spectral_props%roof_emissivity = roof_emissivity(nspec, ncol) ! emissivity of buildings
      lw_spectral_props%wall_emissivity = wall_emissivity(nspec, ncol) ! emissivity of buildings

      ! print *, 'roof_emissivity in suews-su ', roof_emissivity(nspec,:)
      ! print *, 'wall_emissivity in suews-su ', wall_emissivity(nspec,:)
      ! print *, 'lw_spectral_props%wall_emissivity in suews-su ', lw_spectral_props%wall_emissivity
      ! print *, 'lw_spectral_props%wall_emissivity in suews-su ', lw_spectral_props%wall_emissivity

      !!!!!!!!!!!!!! allocate sw !!!!!!!!!!!!!!

      IF (config%do_sw) THEN
         CALL sw_norm_dir%ALLOCATE(config, ncol, nlayer, config%nsw, use_direct=.TRUE.)
         CALL sw_norm_diff%ALLOCATE(config, ncol, nlayer, config%nsw, use_direct=.TRUE.)

         CALL sw_norm_dir%zero_all()
         CALL sw_norm_diff%zero_all()

         CALL sw_flux%ALLOCATE(config, ncol, nlayer, config%nsw, use_direct=.TRUE.)
      END IF

      !!!!!!!!!!!!!! allocate lw !!!!!!!!!!!!!!

      IF (config%do_lw) THEN
         CALL lw_internal%ALLOCATE(config, ncol, nlayer, config%nlw, use_direct=.TRUE.)
         CALL lw_norm%ALLOCATE(config, ncol, nlayer, config%nlw, use_direct=.TRUE.)

         CALL lw_internal%zero_all()
         CALL lw_norm%zero_all()

         CALL lw_flux%ALLOCATE(config, ncol, nlayer, config%nlw, use_direct=.TRUE.)
      END IF

      !!!!!!!!!!!!!! allocate bc_out !!!!!!!!!!!!!!

      CALL bc_out%ALLOCATE(ncol, config%nsw, config%nlw)

      !!!!!!!!!!!!!! run calc_monochromatic_emission !!!!!!!!!!!!!!

      CALL lw_spectral_props%calc_monochromatic_emission(canopy_props)

      !!!!!!!!!!!!!! CALL radsurf !!!!!!!!!!!!!!

      istartcol = 1
      iendcol = 1
      ! Option of repeating calculation multiple time for more accurate profiling
      DO jrepeat = 1, 3
         IF (config%do_lw) THEN
            ! Gas optics and spectral emission
            CALL calc_simple_spectrum_lw(config, canopy_props, lw_spectral_props, &
                 &                       istartcol, iendcol)
         END IF
         ! Call the SPARTACUS-Surface radiation scheme
         CALL radsurf(config, canopy_props, &
              &       sw_spectral_props, lw_spectral_props, &
              &       bc_out, &
              &       istartcol, iendcol, &
              &       sw_norm_dir, sw_norm_diff, &
              &       lw_internal, lw_norm)
         IF (config%do_sw) THEN
            ! Scale the normalized fluxes
            CALL sw_norm_dir%SCALE(canopy_props%nlay, &
            &  top_flux_dn_direct_sw)
            CALL sw_norm_diff%SCALE(canopy_props%nlay, &
            &  top_flux_dn_sw - top_flux_dn_direct_sw)
            CALL sw_flux%SUM(sw_norm_dir, sw_norm_diff)
         END IF
         IF (config%do_lw) THEN
            CALL lw_norm%SCALE(canopy_props%nlay, top_flux_dn_lw)
            CALL lw_flux%SUM(lw_internal, lw_norm)
         END IF
      END DO

      ! albedo
      IF (top_flux_dn_diffuse_sw + top_flux_dn_direct_sw(nspec, ncol) > 0.1) THEN
         alb_spc = ((top_flux_dn_diffuse_sw + 10.**(-10))*(bc_out%sw_albedo(nspec, ncol)) & ! the 10.**-10 stops the equation blowing up when kdwn=0
                    + (top_flux_dn_direct_sw(nspec, ncol) + 10.**(-10))*(bc_out%sw_albedo_dir(nspec, ncol))) &
                   /(top_flux_dn_diffuse_sw + top_flux_dn_direct_sw(nspec, ncol) + 10.**(-10))
         IF (alb_spc < 0.0) alb_spc = 0
      ELSE
         alb_spc = 0.0
      END IF

      !!! Output arrays !!!

      ! emissivity
      emis_spc = bc_out%lw_emissivity(nspec, ncol)
      ! longwave emission
      lw_emission_spc = bc_out%lw_emission(nspec, ncol)
      ! lowngwave upward = emitted as blackbody + reflected
      lw_up_spc = lw_emission_spc + (1 - emis_spc)*ldown
      ! shortwave upward = downward diffuse * diffuse albedo + downward direct * direct albedo
      sw_up_spc = 0.0
      sw_up_spc = kdown*alb_spc ! or more simply: alb_spc*avKdn
      ! net all = net sw + net lw
      qn_spc = sw_flux%top_net(nspec, ncol) + lw_flux%top_net(nspec, ncol)

      ! lw arrays
      clear_air_abs_lw_spc = -999
      clear_air_abs_lw_spc(:nlayer) = lw_flux%clear_air_abs(nspec, :nlayer)
      wall_net_lw_spc = -999
      wall_net_lw_spc(:nlayer) = lw_flux%wall_net(nspec, :nlayer)
      wall_in_lw_spc = -999
      wall_in_lw_spc(:nlayer) = lw_flux%wall_in(nspec, :nlayer)
      ! PRINT *, 'wall_net_lw_spc in suews-su', lw_flux%wall_net
      roof_net_lw_spc = -999
      roof_net_lw_spc(:nlayer) = lw_flux%roof_net(nspec, :nlayer)
      ! PRINT *, 'roof_net_lw_spc in suews-su', lw_flux%roof_net
      roof_in_lw_spc = -999
      roof_in_lw_spc(:nlayer) = lw_flux%roof_in(nspec, :nlayer)
      top_net_lw_spc = lw_flux%top_net(nspec, ncol)
      grnd_net_lw_spc = lw_flux%ground_net(nspec, ncol)
      top_dn_lw_spc = lw_flux%top_dn(nspec, ncol)

      ! sw arrays
      clear_air_abs_sw_spc = -999
      clear_air_abs_sw_spc(:nlayer) = sw_flux%clear_air_abs(nspec, :nlayer)
      wall_net_sw_spc = -999
      wall_net_sw_spc(:nlayer) = sw_flux%wall_net(nspec, :nlayer)
      wall_in_sw_spc = -999
      wall_in_sw_spc(:nlayer) = sw_flux%wall_in(nspec, :nlayer)
      ! PRINT *, 'wall_net_sw_spc in suews-su', wall_net_sw_spc(:nlayer), sw_flux%wall_net
      roof_net_sw_spc = -999
      roof_net_sw_spc(:nlayer) = sw_flux%roof_net(nspec, :nlayer)
      ! PRINT *, 'roof_net_sw_spc in suews-su', roof_net_sw_spc(:nlayer)
      ! PRINT *, 'roof_net_sw_spc in suews-su', sw_flux%roof_net
      roof_in_sw_spc = -999
      roof_in_sw_spc(:nlayer) = sw_flux%roof_in(nspec, :nlayer)
      ! PRINT *, 'roof sw in in suews-su', roof_in_sw_spc(:nlayer)
      ! PRINT *, 'roof sw in in suews-su', sw_flux%roof_in
      ! print *, ''
      top_dn_dir_sw_spc = sw_flux%top_dn_dir(nspec, ncol)
      top_net_sw_spc = sw_flux%top_net(nspec, ncol)
      grnd_dn_dir_sw_spc = sw_flux%ground_dn_dir(nspec, ncol)
      grnd_net_sw_spc = sw_flux%ground_net(nspec, ncol)
      grnd_vertical_diff = sw_flux%ground_vertical_diff(nspec, ncol)

      ! De-normalise the fluxes
      wall_in_sw_spc(:nlayer) = wall_in_sw_spc(:nlayer)/sfr_wall_spc(:nlayer)
      wall_net_sw_spc(:nlayer) = wall_net_sw_spc(:nlayer)/sfr_wall_spc(:nlayer)
      roof_in_sw_spc(:nlayer) = roof_in_sw_spc(:nlayer)/sfr_roof_spc(:nlayer)
      roof_net_sw_spc(:nlayer) = roof_net_sw_spc(:nlayer)/sfr_roof_spc(:nlayer)

      wall_in_lw_spc(:nlayer) = wall_in_lw_spc(:nlayer)/sfr_wall_spc(:nlayer)
      wall_net_lw_spc(:nlayer) = wall_net_lw_spc(:nlayer)/sfr_wall_spc(:nlayer)
      roof_in_lw_spc(:nlayer) = roof_in_lw_spc(:nlayer)/sfr_roof_spc(:nlayer)
      roof_net_lw_spc(:nlayer) = roof_net_lw_spc(:nlayer)/sfr_roof_spc(:nlayer)

      !!!!!!!!!!!!!! Bulk KUP, LUP, QSTAR for SUEWS !!!!!!!!!!!!!!

      lup = lw_up_spc
      ! print *, 'lw_up_spc', lw_up_spc
      kup = sw_up_spc
      ! print *, 'sw_up_spc', sw_up_spc
      ! limit the lower limit of qn to avoid issue when used with OHM
      qn = MAX(qn_spc, -600D0)
      ! print *, 'qn_spc', qn_spc

      ! ============================================================
      ! net radiation for roof/wall
      ! note these fluxes are NOT de-normalised
      qn_roof = roof_net_lw_spc(:nlayer) + roof_net_sw_spc(:nlayer)
      qn_wall = wall_net_lw_spc(:nlayer) + wall_net_sw_spc(:nlayer)

      ! de-normalise net radiation for roof/wall - these will be used in other SUEWS calculations
      ! note the orignal results from above SS calcuations are normalised by the whole grid area
      ! MP 03/06/25: denormalised lw and sw separately - no need for qn now
      ! roof: need to de-normalise by dividing the building/roof fraction
      ! qn_roof = qn_roof/sfr_roof(:nlayer)
      ! wall: need to de-normalise by dividing the building/wall fraction
      ! qn_wall = qn_wall/sfr_wall(:nlayer)

      ! ============================================================
      ! net radiation for ground surfaces
      ! retrieve the surface temperatures/properties of all ground land covers except for buildings
      sfr_grnd_ind = sfr_surf([PavSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf])
      tsfc_grnd_ind_K = tsfc_surf_K([PavSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf])
      emis_grnd_ind = emis_surf([PavSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf])
      alb_grnd_ind = alb_surf([PavSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf])

      ! note the ground here includes all surfaces that are not roof/wall
      ! de-normalise net radiation for ground surfaces - these will be used in other SUEWS calculations:
      sw_net_grnd = grnd_net_sw_spc/(1 - building_frac(1))
      lw_net_grnd = grnd_net_lw_spc/(1 - building_frac(1))

      ! net shortwave radiation for individual ground surfaces
      sw_dn_grnd = sw_net_grnd/DOT_PRODUCT(alb_grnd_ind, sfr_grnd_ind)/SUM(sfr_grnd_ind)
      sw_net_grnd_ind = sw_net_grnd*(1 - alb_surf([PavSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf]))

      ! net longwave radiation for individual ground surfaces
      lw_up_grnd = SBConst*DOT_PRODUCT(emis_grnd_ind*tsfc_grnd_ind_K**4, sfr_grnd_ind)/SUM(sfr_grnd_ind)

      ! assume that the downward longwave radiation incident on the ground is the same between all surfaces
      lw_dn_grnd = lw_up_grnd + lw_net_grnd
      lw_net_grnd_ind = lw_dn_grnd - SBConst*emis_grnd_ind*tsfc_grnd_ind_K**4

      ! net all-wave radiation for individual ground surfaces
      qn_grnd_ind = lw_net_grnd_ind + sw_net_grnd_ind

      ! combine with all surfaces
      qn_surf([PavSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf]) = qn_grnd_ind

      ! average between roof and wall for the building surface: a simple treatment
      ! qn_surf(BldgSurf) = (DOT_PRODUCT(qn_roof, sfr_roof)/SUM(sfr_roof) + DOT_PRODUCT(qn_wall, sfr_wall)/SUM(sfr_wall))
      qn_surf(BldgSurf) = (qn_spc - DOT_PRODUCT(qn_grnd_ind, sfr_grnd_ind))/sfr_surf(BldgSurf)

      dataOutLineSPARTACUS = &
         [alb_spc, emis_spc, &
          top_dn_dir_sw_spc, &
          sw_up_spc, &
          top_dn_lw_spc, &
          lw_up_spc, &
          qn_spc, &
          top_net_sw_spc, &
          top_net_lw_spc, &
          lw_emission_spc, &
          grnd_dn_dir_sw_spc, &
          grnd_vertical_diff, &
          grnd_net_sw_spc, &
          grnd_net_lw_spc, &
          roof_in_sw_spc, &
          roof_net_sw_spc, &
          wall_in_sw_spc, &
          wall_net_sw_spc, &
          clear_air_abs_sw_spc, &
          roof_in_lw_spc, &
          roof_net_lw_spc, &
          wall_in_lw_spc, &
          wall_net_lw_spc, &
          sfr_roof_spc, &
          sfr_wall_spc, &
          clear_air_abs_lw_spc &
          ]

      !!!!!!!!!!!!!! Clear from memory !!!!!!!!!!!!!

      CALL canopy_props%DEALLOCATE()
      CALL sw_spectral_props%DEALLOCATE()
      CALL lw_spectral_props%DEALLOCATE()
      CALL bc_out%DEALLOCATE()
      CALL sw_norm_dir%DEALLOCATE()
      CALL sw_norm_diff%DEALLOCATE()
      CALL lw_internal%DEALLOCATE()
      CALL lw_norm%DEALLOCATE()
      CALL sw_flux%DEALLOCATE()
      CALL lw_flux%DEALLOCATE()

      ! DEALLOCATE (height)
      DEALLOCATE (top_flux_dn_sw)
      DEALLOCATE (top_flux_dn_direct_sw)
      DEALLOCATE (top_flux_dn_lw)
      ! DEALLOCATE (building_frac)
      ! DEALLOCATE (veg_frac)
      ! DEALLOCATE (building_scale)
      ! DEALLOCATE (veg_scale)
      DEALLOCATE (veg_depth)
      DEALLOCATE (veg_ext)
      DEALLOCATE (LAI_av)
      DEALLOCATE (LAI_av_z)
      ! DEALLOCATE (veg_fsd)
      ! DEALLOCATE (veg_contact_fraction)
      ! DEALLOCATE (roof_albedo)
      ! DEALLOCATE (wall_albedo)
      ! DEALLOCATE (roof_albedo_dir_mult_fact)
      ! DEALLOCATE (wall_specular_frac)
      ! DEALLOCATE (roof_emissivity)
      ! DEALLOCATE (wall_emissivity)

   END SUBROUTINE SPARTACUS

END MODULE SPARTACUS_MODULE
