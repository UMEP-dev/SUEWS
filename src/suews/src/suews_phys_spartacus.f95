! Main module following naming standard: matches filename
MODULE module_phys_spartacus
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
   USE module_ctrl_const_allocate, ONLY: NSURF, NVegSurf, nspec, nsw, nlw, ncol, &
                            ConifSurf, DecidSurf, BldgSurf, PavSurf, GrassSurf, BSoilSurf, WaterSurf
   USE module_ctrl_const_physconst, ONLY: SBConst, eps_fp
   USE, INTRINSIC :: ieee_arithmetic, ONLY: IEEE_IS_NAN

   IMPLICIT NONE

CONTAINS
   SUBROUTINE SPARTACUS_Initialise
      USE module_ctrl_const_datain, ONLY: fileinputpath
      USE module_ctrl_const_allocate
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

   ELEMENTAL LOGICAL FUNCTION invalid_real(value)
      REAL(KIND(1D0)), INTENT(IN) :: value
      invalid_real = IEEE_IS_NAN(value) .OR. value /= value
   END FUNCTION invalid_real

   FUNCTION interp_profile_z(z_x, z, v) RESULT(v_x)
      REAL(KIND(1D0)), INTENT(IN) :: z_x
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: z
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: v
      REAL(KIND(1D0)) :: v_x
      REAL(KIND(1D0)) :: dz
      INTEGER :: i
      INTEGER :: n

      n = SIZE(z)
      v_x = 0D0
      IF (n < 1 .OR. SIZE(v) /= n) RETURN
      IF (z_x <= z(1)) THEN
         v_x = v(1)
      ELSE IF (z_x >= z(n)) THEN
         v_x = v(n)
      ELSE
         DO i = 1, n - 1
            IF (z_x >= z(i) .AND. z_x <= z(i + 1)) THEN
               dz = z(i + 1) - z(i)
               IF (ABS(dz) <= eps_fp) THEN
                  v_x = v(i)
               ELSE
                  v_x = v(i) + (z_x - z(i))*(v(i + 1) - v(i))/dz
               END IF
               RETURN
            END IF
         END DO
         v_x = v(n)
      END IF
   END FUNCTION interp_profile_z

   SUBROUTINE SPARTACUS( &
      DiagQN, & !input:
      sfr_surf, zenith_deg, nlayer, & !input:
      tsfc_surf, tsfc_roof, tsfc_wall, &
      kdown, kdown_direct, kdown_diffuse, ldown, Tair_C, rsl_z, rsl_t_C, alb_surf, emis_surf, LAI_id, &
      n_vegetation_region_urban, &
      n_vegetation_region_forest, &
      n_stream_sw_urban, n_stream_sw_forest, n_stream_lw_urban, n_stream_lw_forest, &
      air_ext_sw, air_ssa_sw, &
      veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
      veg_fsd_const, veg_contact_fraction_const, &
      ground_albedo_dir_mult_fact, use_sw_direct_albedo, &
      height, bldgH, sfr_evetr, sfr_dectr, EveTreeH, DecTreeH, &
      building_frac, veg_frac, sfr_roof, sfr_wall, &
      building_scale, veg_scale, veg_ext_input, & !input:
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
      ! USE module_ctrl_const_datain, ONLY: fileinputpath
      USE module_ctrl_const_allocate, ONLY: ncolumnsDataOutSPARTACUS
      USE module_ctrl_error_state, ONLY: add_supy_warning

      IMPLICIT NONE

      !!!!!!!!!!!!!! Set objects and variables !!!!!!!!!!!!!!

      ! Input parameters and variables from SUEWS
      REAL(KIND(1D0)), INTENT(IN) :: zenith_deg
      INTEGER, INTENT(IN) :: DiagQN
      INTEGER, INTENT(IN) :: nlayer

      ! TODO: tsurf_0 and temp_C need to be made vertically distributed
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: tsfc_roof, tsfc_wall
      REAL(KIND(1D0)), INTENT(IN) :: Tair_C
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: rsl_z
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: rsl_t_C

      REAL(KIND(1D0)), INTENT(IN) :: kdown
      REAL(KIND(1D0)), INTENT(IN) :: kdown_direct
      REAL(KIND(1D0)), INTENT(IN) :: kdown_diffuse
      REAL(KIND(1D0)), INTENT(IN) :: ldown
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: tsfc_surf

      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: sfr_surf, alb_surf, emis_surf
      REAL(KIND(1D0)), DIMENSION(NVegSurf), INTENT(IN) :: LAI_id

      ! SPARTACUS configuration parameters
      INTEGER, INTENT(IN) :: n_vegetation_region_urban, &
                             n_vegetation_region_forest, &
                             n_stream_sw_urban, n_stream_sw_forest, n_stream_lw_urban, n_stream_lw_forest
      REAL(KIND(1D0)), INTENT(IN) :: air_ext_sw, air_ssa_sw, &
                                     veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
                                     veg_fsd_const, veg_contact_fraction_const, &
                                     ground_albedo_dir_mult_fact
      ! INTEGER(kind=jpim) :: ncol
      ! INTEGER(kind=jpim) :: nlayer
      INTEGER(kind=jpim), ALLOCATABLE :: i_representation(:)
      INTEGER(kind=jpim), ALLOCATABLE :: nlay(:)
      INTEGER :: istartcol, iendcol
      INTEGER :: jrepeat, ilay, jlay, jcol
      INTEGER :: nlay_veg, nlay_flux, max_idx

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
      REAL(KIND(1D0)) :: lw_flat_net_spc, sw_flat_net_spc, lw_up_grnd_fallback
      REAL(KIND(1D0)) :: top_net_lw_spc
      REAL(KIND(1D0)) :: grnd_net_lw_spc
      REAL(KIND(1D0)) :: top_dn_lw_spc
      REAL(KIND(1D0)) :: top_dn_dir_sw_spc
      REAL(KIND(1D0)) :: top_flux_dn_diffuse_sw
      REAL(KIND(1D0)) :: top_net_sw_spc
      REAL(KIND(1D0)) :: grnd_dn_dir_sw_spc
      REAL(KIND(1D0)) :: grnd_net_sw_spc
      REAL(KIND(1D0)) :: grnd_vertical_diff
      REAL(KIND(1D0)), DIMENSION(15) :: clear_air_abs_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: clear_air_abs_sw_spc
      REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: roof_in_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: roof_in_sw_dir_spc
      REAL(KIND(1D0)), DIMENSION(15) :: roof_in_sw_diff_spc
      REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: roof_in_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: roof_net_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: roof_net_lw_spc
      REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: wall_in_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: wall_in_sw_dir_spc
      REAL(KIND(1D0)), DIMENSION(15) :: wall_in_sw_diff_spc
      REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: wall_in_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: wall_net_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: wall_net_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: sfr_roof_spc
      REAL(KIND(1D0)), DIMENSION(15) :: sfr_wall_spc
      REAL(KIND(1D0)) :: grnd_dn_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: veg_abs_sw_spc ! SW vegetation absorption per layer
      REAL(KIND(1D0)), DIMENSION(15) :: veg_abs_lw_spc ! LW vegetation absorption per layer
      REAL(KIND(1D0)), DIMENSION(15) :: flux_dn_layer_top_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: flux_up_layer_top_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: flux_dn_layer_base_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: flux_up_layer_base_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: flux_dn_layer_top_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: flux_up_layer_top_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: flux_dn_layer_base_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: flux_up_layer_base_lw_spc
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
      REAL(KIND(1D0)), ALLOCATABLE :: top_flux_dn_direct_sw(:, :) ! direct shortwave
      REAL(KIND(1D0)), ALLOCATABLE :: top_flux_dn_lw(:, :) ! longwave

      ! surface temperature and air temperature in Kelvin
      REAL(KIND(1D0)), DIMENSION(nlayer) :: tsfc_roof_K, tsfc_wall_K
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_surf_K
      REAL(KIND(1D0)) :: tair_K
      REAL(KIND(1D0)) :: tair_veg_K
      ! top-of-canopy diffuse sw downward

      REAL(KIND(1D0)) :: ground_frac_spc, surface_frac_sum
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
      REAL(KIND(1D0)), INTENT(IN) :: bldgH
      REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
      REAL(KIND(1D0)), INTENT(IN) :: EveTreeH
      REAL(KIND(1D0)), INTENT(IN) :: DecTreeH
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac ! cumulative building fraction at each layer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_frac
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: sfr_roof ! individual surface fraction of roofs at each layer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: sfr_wall ! individual surface fraction of walls at each layer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_scale
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_ext_input
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
      REAL(KIND(1D0)) :: bldgH_use
      REAL(KIND(1D0)) :: EveTreeH_use
      REAL(KIND(1D0)) :: DecTreeH_use
      REAL(KIND(1D0)) :: sfr_evetr_use
      REAL(KIND(1D0)) :: sfr_dectr_use
      REAL(KIND(1D0)) :: tree_frac_sum
      REAL(KIND(1D0)) :: veg_air_height
      REAL(KIND(1D0)) :: Tair_hbh_C
      REAL(KIND(1D0)) :: Tair_veg_C
      LOGICAL :: use_evetr
      LOGICAL :: use_dectr
      LOGICAL :: rsl_profile_valid
      bldgH_use = bldgH
      IF (invalid_real(bldgH_use) .OR. bldgH_use <= 0D0) bldgH_use = height(nlayer + 1)

      sfr_evetr_use = sfr_evetr
      sfr_dectr_use = sfr_dectr
      EveTreeH_use = EveTreeH
      DecTreeH_use = DecTreeH
      IF (invalid_real(sfr_evetr_use) .OR. sfr_evetr_use <= 0D0) sfr_evetr_use = 0D0
      IF (invalid_real(sfr_dectr_use) .OR. sfr_dectr_use <= 0D0) sfr_dectr_use = 0D0
      IF (invalid_real(EveTreeH_use) .OR. EveTreeH_use <= 0D0) EveTreeH_use = 0D0
      IF (invalid_real(DecTreeH_use) .OR. DecTreeH_use <= 0D0) DecTreeH_use = 0D0

      use_evetr = sfr_evetr_use > eps_fp .AND. EveTreeH_use > eps_fp
      use_dectr = sfr_dectr_use > eps_fp .AND. DecTreeH_use > eps_fp
      tree_frac_sum = sfr_evetr_use + sfr_dectr_use
      IF (use_evetr .AND. use_dectr) THEN
         veg_air_height = ((sfr_evetr_use*EveTreeH_use + sfr_dectr_use*DecTreeH_use)/tree_frac_sum)/2D0
      ELSEIF (use_evetr) THEN
         veg_air_height = EveTreeH_use/2D0
      ELSEIF (use_dectr) THEN
         veg_air_height = DecTreeH_use/2D0
      ELSE
         veg_air_height = bldgH_use/2D0
      END IF
      IF (invalid_real(veg_air_height) .OR. veg_air_height <= 0D0) veg_air_height = bldgH_use/2D0

      rsl_profile_valid = SIZE(rsl_z) > 1 .AND. SIZE(rsl_z) == SIZE(rsl_t_C)
      IF (rsl_profile_valid) THEN
         rsl_profile_valid = ALL(rsl_z > -900D0) .AND. ANY(rsl_t_C > -900D0) &
                             .AND. .NOT. ANY(invalid_real(rsl_z)) &
                             .AND. .NOT. ANY(invalid_real(rsl_t_C))
      END IF

      Tair_hbh_C = Tair_C
      Tair_veg_C = Tair_C
      IF (rsl_profile_valid) THEN
         Tair_hbh_C = interp_profile_z(bldgH_use/2D0, rsl_z, rsl_t_C)
         Tair_veg_C = interp_profile_z(veg_air_height, rsl_z, rsl_t_C)
         IF (invalid_real(Tair_hbh_C) .OR. Tair_hbh_C <= -900D0) Tair_hbh_C = Tair_C
         IF (invalid_real(Tair_veg_C) .OR. Tair_veg_C <= -900D0) Tair_veg_C = Tair_C
      END IF

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
      veg_ext = 0.0D0

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
      ! Always run SW: radsurf already guards cos_sza=0 per-column,
      ! and scaling by zero kdown produces zero SW contribution.
      config%do_sw = .TRUE.
      config%do_lw = .TRUE.
      config%use_sw_direct_albedo = use_sw_direct_albedo
      ALLOCATE (i_representation(ncol))
      IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > eps_fp .AND. sfr_surf(BldgSurf) > eps_fp) THEN
         config%do_vegetation = .TRUE.
         i_representation = [3]
         config%do_urban = .TRUE.
      ELSE IF (ABS(sfr_surf(ConifSurf) + sfr_surf(DecidSurf)) <= eps_fp .AND. sfr_surf(BldgSurf) > eps_fp) THEN
         config%do_vegetation = .FALSE.
         i_representation = [2]
         config%do_urban = .TRUE.
      ELSE IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > eps_fp .AND. ABS(sfr_surf(BldgSurf)) <= eps_fp) THEN
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
      config%n_vegetation_region_forest = n_vegetation_region_forest
      config%nsw = nsw
      config%nlw = nlw
      config%n_stream_sw_urban = n_stream_sw_urban
      config%n_stream_lw_urban = n_stream_lw_urban
      config%n_stream_sw_forest = n_stream_sw_forest
      config%n_stream_lw_forest = n_stream_lw_forest
      CALL config%consolidate()

      !!!!!!!!!!!!!! allocate and set canopy_props !!!!!!!!!!!!!!

      ! allocate
      CALL canopy_props%DEALLOCATE()
      CALL canopy_props%ALLOCATE(config, ncol, nlayer, i_representation)

      ! set cos_sza, nlay, ncol, ntotlay
      ! Guard against negative night-time values to keep radiation geometry physical.
      canopy_props%cos_sza = MAX(0.0D0, COS(zenith_deg*3.1415927/180))
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
      ! Initialise to avoid platform-dependent values when no vegetation is
      ! present in some (or all) layers.
      LAI_av = 0.0D0
      veg_depth = 0.0D0
      LAI_av_z = 0.0D0
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

      IF (ANY(veg_ext_input > -900.0D0)) THEN
         DO jlay = 1, nlayer
            IF (veg_ext_input(jlay) > -900.0D0) veg_ext(jlay) = veg_ext_input(jlay)
         END DO
      END IF

      ! set temperature
      tsfc_surf_K = tsfc_surf + 273.15 ! convert surface temperature to Kelvin
      tsfc_roof_K = tsfc_roof + 273.15 ! convert surface temperature to Kelvin
      tsfc_wall_K = tsfc_wall + 273.15 ! convert surface temperature to Kelvin
      tair_K = Tair_hbh_C + 273.15 ! convert air temperature to Kelvin
      tair_veg_K = Tair_veg_C + 273.15 ! convert vegetation air temperature to Kelvin

      ! set ground temperature as the area-weighted average of the surface temperature of all land covers but buildings
      IF (1.0D0 - sfr_surf(BldgSurf) > eps_fp) THEN
         canopy_props%ground_temperature = (DOT_PRODUCT(tsfc_surf_K, sfr_surf) - tsfc_surf_K(BldgSurf)*sfr_surf(BldgSurf)) &
                                           /(1 - sfr_surf(BldgSurf))
      ELSE
         canopy_props%ground_temperature = tsfc_surf_K(BldgSurf)
      END IF

      canopy_props%roof_temperature = tsfc_roof_K
      canopy_props%wall_temperature = tsfc_wall_K
      canopy_props%clear_air_temperature = tair_veg_K
      ! allocate_canopy only allocates veg_* when do_vegetation=.TRUE.,
      ! but downstream radsurf code may still access them.  Ensure they
      ! exist (zeroed) so that assignments and reads are always valid.
      IF (.NOT. ALLOCATED(canopy_props%veg_temperature)) &
         ALLOCATE(canopy_props%veg_temperature(canopy_props%ntotlay))
      IF (.NOT. ALLOCATED(canopy_props%veg_air_temperature)) &
         ALLOCATE(canopy_props%veg_air_temperature(canopy_props%ntotlay))
      IF (.NOT. ALLOCATED(canopy_props%veg_fraction)) &
         ALLOCATE(canopy_props%veg_fraction(canopy_props%ntotlay))
      IF (.NOT. ALLOCATED(canopy_props%veg_scale)) &
         ALLOCATE(canopy_props%veg_scale(canopy_props%ntotlay))
      IF (.NOT. ALLOCATED(canopy_props%veg_ext)) &
         ALLOCATE(canopy_props%veg_ext(canopy_props%ntotlay))
      IF (.NOT. ALLOCATED(canopy_props%veg_fsd)) &
         ALLOCATE(canopy_props%veg_fsd(canopy_props%ntotlay))
      IF (.NOT. ALLOCATED(canopy_props%veg_contact_fraction)) &
         ALLOCATE(canopy_props%veg_contact_fraction(canopy_props%ntotlay))
      canopy_props%veg_temperature = tair_veg_K
      canopy_props%veg_air_temperature = tair_veg_K
      IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > 0.0) THEN
         canopy_props%veg_temperature = tair_veg_K
         canopy_props%veg_air_temperature = tair_veg_K
      END IF

      ! set building and vegetation properties
      canopy_props%i_representation = i_representation
      canopy_props%building_scale = building_scale(:) ! diameter of buildings (m). The only L method for buildings is Eq. 19 Hogan et al. 2018.
      canopy_props%building_fraction = building_frac(:) ! building fraction
      canopy_props%veg_fraction = 0.0D0
      canopy_props%veg_scale = 0.0D0
      canopy_props%veg_ext = 0.0D0
      canopy_props%veg_fsd = 0.0D0
      canopy_props%veg_contact_fraction = 0.0D0
      IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > 0.0) THEN
         canopy_props%veg_fraction = veg_frac(:)
         canopy_props%veg_scale = veg_scale(:)
         canopy_props%veg_ext = veg_ext(:)
         canopy_props%veg_fsd = veg_fsd(:)
         canopy_props%veg_contact_fraction = veg_contact_fraction(:)
         ! Zero veg_fraction where veg_ext is zero to prevent
         ! calc_matrices_sw_eig receiving all-zero gamma matrices -> NaN.
         ! When ext=0 the layer is transparent: the vegetation-free
         ! shortcut in spartacus_forest_sw handles this correctly.
         !DO jlay = 1, nlayer
         !   IF (veg_ext(jlay) <= 0.0D0) canopy_props%veg_fraction(jlay) = 0.0D0
         !END DO
      END IF

      !!!!!!!!!!!!!! allocate and set canopy top forcing !!!!!!!!!!!!!!
      IF (DiagQN == 1) PRINT *, 'in SPARTACUS, setting canopy top forcing ...'
      ALLOCATE (top_flux_dn_sw(nspec, ncol))
      ALLOCATE (top_flux_dn_direct_sw(nspec, ncol))
      ALLOCATE (top_flux_dn_lw(nspec, ncol))
      top_flux_dn_sw = kdown ! diffuse + direct
      top_flux_dn_direct_sw = kdown_direct
      top_flux_dn_diffuse_sw = kdown_diffuse
      top_flux_dn_lw = ldown

      !!!!!!!!!!!!!! allocate and set sw_spectral_props !!!!!!!!!!!!!!

      CALL sw_spectral_props%DEALLOCATE()
      CALL sw_spectral_props%ALLOCATE(config, ncol, nlayer, nspec, canopy_props%i_representation)

      ! albedo of the ground
      ground_frac_spc = sfr_surf(PavSurf) + sfr_surf(GrassSurf) + sfr_surf(BSoilSurf) + sfr_surf(WaterSurf)
      surface_frac_sum = SUM(sfr_surf)
      IF (ground_frac_spc > eps_fp) THEN
         alb_no_tree_bldg = (alb_surf(PavSurf)*sfr_surf(PavSurf) + alb_surf(GrassSurf)*sfr_surf(GrassSurf) + &
                             alb_surf(BSoilSurf)*sfr_surf(BSoilSurf) + alb_surf(WaterSurf)*sfr_surf(WaterSurf))/ &
                            ground_frac_spc
      ELSE IF (surface_frac_sum > eps_fp) THEN
         alb_no_tree_bldg = DOT_PRODUCT(alb_surf, sfr_surf)/surface_frac_sum
      ELSE
         alb_no_tree_bldg = 0.0D0
      END IF
      IF (invalid_real(alb_no_tree_bldg)) alb_no_tree_bldg = 0.0D0
      sw_spectral_props%air_ext = air_ext_sw
      sw_spectral_props%air_ssa = air_ssa_sw
      ! Ensure conditionally-allocated arrays exist for downstream radsurf.
      IF (.NOT. ALLOCATED(sw_spectral_props%veg_ssa)) &
         ALLOCATE(sw_spectral_props%veg_ssa(nspec, nlayer))
      IF (.NOT. ALLOCATED(sw_spectral_props%ground_albedo_dir)) &
         ALLOCATE(sw_spectral_props%ground_albedo_dir(nspec, ncol))
      IF (.NOT. ALLOCATED(sw_spectral_props%roof_albedo_dir)) &
         ALLOCATE(sw_spectral_props%roof_albedo_dir(nspec, nlayer))
      sw_spectral_props%veg_ssa = 0.0D0
      IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > 0.0) THEN
         sw_spectral_props%veg_ssa = veg_ssa_sw
      END IF
      sw_spectral_props%ground_albedo = alb_no_tree_bldg ! albedo excluding buildings and trees
      ! Only set roof and wall albedo if building surface fraction is present
      IF (config%do_urban) THEN
         sw_spectral_props%roof_albedo = roof_albedo(nspec, ncol) ! albedo of buildings
         sw_spectral_props%wall_albedo = wall_albedo(nspec, ncol) ! albedo of buildings
         sw_spectral_props%wall_specular_frac = wall_specular_frac(nspec, ncol)
      END IF
      sw_spectral_props%ground_albedo_dir = sw_spectral_props%ground_albedo
      IF (config%do_urban) THEN
         sw_spectral_props%roof_albedo_dir = sw_spectral_props%roof_albedo
      END IF
      IF (config%use_sw_direct_albedo) THEN
         sw_spectral_props%ground_albedo_dir = alb_no_tree_bldg*ground_albedo_dir_mult_fact
         sw_spectral_props%roof_albedo_dir = roof_albedo(nspec, ncol)*roof_albedo_dir_mult_fact(nspec, ncol)
      END IF

      !!!!!!!!!!!!!! allocate and set lw_spectral_props !!!!!!!!!!!!!!

      CALL lw_spectral_props%DEALLOCATE()
      CALL lw_spectral_props%ALLOCATE(config, nspec, ncol, nlayer, canopy_props%i_representation)

      IF (ground_frac_spc > eps_fp) THEN
         emis_no_tree_bldg = (emis_surf(PavSurf)*sfr_surf(PavSurf) + emis_surf(GrassSurf)*sfr_surf(GrassSurf) + &
                              emis_surf(BSoilSurf)*sfr_surf(BSoilSurf) + emis_surf(WaterSurf)*sfr_surf(WaterSurf))/ &
                             ground_frac_spc
      ELSE IF (surface_frac_sum > eps_fp) THEN
         emis_no_tree_bldg = DOT_PRODUCT(emis_surf, sfr_surf)/surface_frac_sum
      ELSE
         emis_no_tree_bldg = 1.0D0
      END IF
      IF (invalid_real(emis_no_tree_bldg)) emis_no_tree_bldg = 1.0D0
      lw_spectral_props%air_ext = air_ext_lw
      lw_spectral_props%air_ssa = air_ssa_lw
      ! Ensure conditionally-allocated veg arrays exist for downstream radsurf.
      IF (.NOT. ALLOCATED(lw_spectral_props%veg_ssa)) &
         ALLOCATE(lw_spectral_props%veg_ssa(nspec, nlayer))
      IF (.NOT. ALLOCATED(lw_spectral_props%veg_air_planck)) &
         ALLOCATE(lw_spectral_props%veg_air_planck(nspec, nlayer))
      IF (.NOT. ALLOCATED(lw_spectral_props%veg_planck)) &
         ALLOCATE(lw_spectral_props%veg_planck(nspec, nlayer))
      lw_spectral_props%veg_ssa = 0.0D0
      lw_spectral_props%veg_air_planck = 0.0D0
      lw_spectral_props%veg_planck = 0.0D0
      IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) > 0.0) THEN
         lw_spectral_props%veg_ssa = veg_ssa_lw
      END IF
      lw_spectral_props%ground_emissivity = emis_no_tree_bldg ! emissivity excluding buildings and trees
      IF (config%do_urban) THEN
         lw_spectral_props%roof_emissivity = roof_emissivity(nspec, ncol) ! emissivity of buildings
         lw_spectral_props%wall_emissivity = wall_emissivity(nspec, ncol) ! emissivity of buildings
      END IF

      ! print *, 'roof_emissivity in suews-su ', roof_emissivity(nspec,:)
      ! print *, 'wall_emissivity in suews-su ', wall_emissivity(nspec,:)
      ! print *, 'lw_spectral_props%wall_emissivity in suews-su ', lw_spectral_props%wall_emissivity
      ! print *, 'lw_spectral_props%wall_emissivity in suews-su ', lw_spectral_props%wall_emissivity

      !!!!!!!!!!!!!! allocate sw !!!!!!!!!!!!!!

      CALL sw_norm_dir%ALLOCATE(config, ncol, nlayer, config%nsw, use_direct=.TRUE.)
      CALL sw_norm_diff%ALLOCATE(config, ncol, nlayer, config%nsw, use_direct=.TRUE.)

      CALL sw_norm_dir%zero_all()
      CALL sw_norm_diff%zero_all()

      CALL sw_flux%ALLOCATE(config, ncol, nlayer, config%nsw, use_direct=.TRUE.)
      CALL sw_flux%zero_all()

      !!!!!!!!!!!!!! allocate lw !!!!!!!!!!!!!!

      IF (config%do_lw) THEN
         CALL lw_internal%ALLOCATE(config, ncol, nlayer, config%nlw, use_direct=.TRUE.)
         CALL lw_norm%ALLOCATE(config, ncol, nlayer, config%nlw, use_direct=.TRUE.)

         CALL lw_internal%zero_all()
         CALL lw_norm%zero_all()

         CALL lw_flux%ALLOCATE(config, ncol, nlayer, config%nlw, use_direct=.TRUE.)
         CALL lw_flux%zero_all()
      END IF

      !!!!!!!!!!!!!! allocate bc_out !!!!!!!!!!!!!!

      CALL bc_out%ALLOCATE(ncol, config%nsw, config%nlw)
      ! Zero bc_out arrays: Windows ALLOCATE may return recycled memory
      ! containing NaN bit patterns, unlike macOS/Linux which zero-fill pages.
      bc_out%sw_albedo = 0.0_jprb
      bc_out%sw_albedo_dir = 0.0_jprb
      bc_out%lw_emissivity = 0.0_jprb
      bc_out%lw_emission = 0.0_jprb

      !!!!!!!!!!!!!! run calc_monochromatic_emission !!!!!!!!!!!!!!

      CALL lw_spectral_props%calc_monochromatic_emission(canopy_props)
      ! Keep the crash fallback independent of SPARTACUS ground emission:
      ! if the radiation solve fails, air-temperature Planck emission gives
      ! a finite flat-tile longwave term for the OHM handoff.
      lw_up_grnd_fallback = emis_no_tree_bldg*SBConst*tair_K**4
      IF (invalid_real(lw_up_grnd_fallback)) lw_up_grnd_fallback = 0.0D0
      IF (invalid_real(lw_spectral_props%ground_emission(nspec, ncol))) THEN
         lw_spectral_props%ground_emission(nspec, ncol) = lw_up_grnd_fallback
      END IF
      lw_flat_net_spc = emis_no_tree_bldg*ldown - lw_up_grnd_fallback
      sw_flat_net_spc = MAX(kdown, 0.0D0)*(1.0D0 - alb_no_tree_bldg)
      IF (invalid_real(lw_flat_net_spc)) lw_flat_net_spc = 0.0D0
      IF (invalid_real(sw_flat_net_spc)) sw_flat_net_spc = 0.0D0

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

      ! At night the normalised SW solver may retain NaNs that survive
      ! multiplication by zero incoming shortwave on some x86 runners.
      ! No incoming SW means the physical contribution is exactly zero.
      IF (config%do_sw .AND. top_flux_dn_sw(nspec, ncol) <= eps_fp) THEN
         CALL sw_flux%zero_all()
      END IF

      ! Guard: the SPARTACUS LW eigenvalue solver can produce NaN for
      ! certain urban canopy geometries due to matrix singularity.
      ! Detect NaN in the critical outputs and replace with a simple
      ! flat-tile LW radiation approximation to prevent downstream
      ! crashes (e.g. OHM error code 21 from NaN qn).
      IF (config%do_lw) THEN
         IF (invalid_real(lw_flux%top_net(nspec, ncol))) THEN
            ! Full NaN from solver source-term singularity: replace all
            ! LW outputs with flat-tile approximation (net = absorbed
            ! incoming minus emitted upward).
            CALL add_supy_warning('SPARTACUS: LW full NaN detected -- using flat-tile fallback')
            CALL lw_flux%zero_all()
            lw_flux%top_net(nspec, ncol) = lw_flat_net_spc
            lw_flux%top_dn(nspec, ncol) = ldown
            lw_flux%ground_net(nspec, ncol) = lw_flat_net_spc
            lw_flux%ground_dn(nspec, ncol) = ldown
            bc_out%lw_emission(nspec, ncol) = lw_up_grnd_fallback
            bc_out%lw_emissivity(nspec, ncol) = emis_no_tree_bldg
         ELSE IF (ANY(invalid_real(lw_flux%wall_net(nspec, :nlayer))) &
                  .OR. ANY(invalid_real(lw_flux%roof_net(nspec, :nlayer)))) THEN
            ! Partial NaN from integrated-flux singularity: per-layer
            ! fields (wall, roof, clear-air) are contaminated; top-level
            ! fluxes remain valid.
            ! NOTE: zeroing per-layer absorption is non-conservative
            ! (surface energy budget not closed) but acceptable as a
            ! crash guard; the top-level net fluxes driving qn are kept.
            CALL add_supy_warning('SPARTACUS: LW partial NaN detected -- zeroing per-layer fields')
            lw_flux%clear_air_abs(nspec, :nlayer) = 0.0D0
            lw_flux%wall_net(nspec, :nlayer) = 0.0D0
            lw_flux%wall_in(nspec, :nlayer) = 0.0D0
            lw_flux%roof_net(nspec, :nlayer) = 0.0D0
            lw_flux%roof_in(nspec, :nlayer) = 0.0D0
         END IF
      END IF

      ! A final guard at the SUEWS handoff keeps invalid SPARTACUS
      ! intermediates from reaching OHM/STEBBS as NaN all-wave radiation.
      IF (config%do_sw) THEN
         qn_spc = sw_flux%top_net(nspec, ncol) + lw_flux%top_net(nspec, ncol)
      ELSE
         qn_spc = lw_flux%top_net(nspec, ncol)
      END IF
      IF (invalid_real(qn_spc)) THEN
         CALL add_supy_warning('SPARTACUS: all-wave NaN detected -- using flat-tile fallback')
         CALL sw_flux%zero_all()
         sw_flux%top_dn(nspec, ncol) = MAX(kdown, 0.0D0)
         sw_flux%top_dn_dir(nspec, ncol) = MAX(top_flux_dn_direct_sw(nspec, ncol), 0.0D0)
         sw_flux%ground_dn(nspec, ncol) = MAX(kdown, 0.0D0)
         sw_flux%ground_dn_dir(nspec, ncol) = MAX(top_flux_dn_direct_sw(nspec, ncol), 0.0D0)
         sw_flux%top_net(nspec, ncol) = sw_flat_net_spc
         sw_flux%ground_net(nspec, ncol) = sw_flat_net_spc
         bc_out%sw_albedo(nspec, ncol) = alb_no_tree_bldg
         bc_out%sw_albedo_dir(nspec, ncol) = alb_no_tree_bldg
         CALL lw_flux%zero_all()
         lw_flux%top_net(nspec, ncol) = lw_flat_net_spc
         lw_flux%top_dn(nspec, ncol) = ldown
         lw_flux%ground_net(nspec, ncol) = lw_flat_net_spc
         lw_flux%ground_dn(nspec, ncol) = ldown
         bc_out%lw_emission(nspec, ncol) = lw_up_grnd_fallback
         bc_out%lw_emissivity(nspec, ncol) = emis_no_tree_bldg
         qn_spc = sw_flat_net_spc + lw_flat_net_spc
      END IF

      !-----------------------------------------------------------------
      ! Shortwave vegetation absorption per layer (W m-2), from sw_flux - beginning - SR
      !-----------------------------------------------------------------
      veg_abs_sw_spc = -999.0D0

      ! Only attempt extraction when SW is active and there is at least one SPARTACUS layer
      IF (config%do_sw .AND. canopy_props%nlay(1) > 0) THEN
         ! Some configurations do not allocate sw_flux%veg_abs at all:
         ! in that case, skip extraction and keep fill values.
         IF (ALLOCATED(sw_flux%veg_abs)) THEN
            ! Number of available layers in veg_abs (second dimension)
            nlay_veg = SIZE(sw_flux%veg_abs, 2)

            ! Required maximum index in veg_abs for this column
            max_idx = canopy_props%istartlay(1) + MIN(canopy_props%nlay(1), 15) - 1

            IF (nlay_veg >= max_idx) THEN
               ilay = canopy_props%istartlay(1)
               DO jlay = 1, MIN(canopy_props%nlay(1), 15)
                  ! Use the scaled direct+diffuse components directly so this
                  ! diagnostic is independent of optional profile-flux storage.
                  veg_abs_sw_spc(jlay) = sw_norm_dir%veg_abs(1, ilay + jlay - 1) &
                                         + sw_norm_diff%veg_abs(1, ilay + jlay - 1)
               END DO
            END IF
         END IF
      END IF

      !-----------------------------------------------------------------
      ! Shortwave vegetation absorption per layer (W m-2), from sw_flux - end - SR
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      ! Longwave vegetation absorption per layer (W m-2), from lw_flux
      !-----------------------------------------------------------------
      veg_abs_lw_spc = -999.0D0

      ! Only attempt extraction when LW is active and there is at least one SPARTACUS layer
      IF (config%do_lw .AND. canopy_props%nlay(1) > 0) THEN
         ! Some configurations do not allocate lw_flux%veg_abs at all:
         ! in that case, skip extraction and keep fill values.
         IF (ALLOCATED(lw_flux%veg_abs)) THEN
            ! Number of available layers in veg_abs (second dimension)
            nlay_veg = SIZE(lw_flux%veg_abs, 2)

            ! Required maximum index in veg_abs for this column
            max_idx = canopy_props%istartlay(1) + MIN(canopy_props%nlay(1), 15) - 1

            IF (nlay_veg >= max_idx) THEN
               ilay = canopy_props%istartlay(1)
               DO jlay = 1, MIN(canopy_props%nlay(1), 15)
                  ! nspec=1, so use first spectral index
                  veg_abs_lw_spc(jlay) = lw_flux%veg_abs(1, ilay + jlay - 1)
               END DO
            END IF
         END IF
      END IF

      ! albedo
      IF (config%do_sw) THEN
         IF (top_flux_dn_diffuse_sw + top_flux_dn_direct_sw(nspec, ncol) > 0.1) THEN
            alb_spc = ((top_flux_dn_diffuse_sw + 10.**(-10))*(bc_out%sw_albedo(nspec, ncol)) & ! the 10.**-10 stops the equation blowing up when kdwn=0
                       + (top_flux_dn_direct_sw(nspec, ncol) + 10.**(-10))*(bc_out%sw_albedo_dir(nspec, ncol))) &
                      /(top_flux_dn_diffuse_sw + top_flux_dn_direct_sw(nspec, ncol) + 10.**(-10))
            IF (alb_spc < 0.0) alb_spc = 0
         ELSE
            alb_spc = 0.0
         END IF
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
      ! lw arrays
      clear_air_abs_lw_spc = -999
      clear_air_abs_lw_spc(:nlayer) = lw_flux%clear_air_abs(nspec, :nlayer)
      wall_net_lw_spc = -999.0D0
      wall_in_lw_spc  = -999.0D0
      roof_net_lw_spc = -999.0D0
      roof_in_lw_spc  = -999.0D0

      IF (config%do_urban) THEN
         ! Urban facets exist: safe to read wall/roof fields
         wall_net_lw_spc(:nlayer) = lw_flux%wall_net(nspec, :nlayer)
         wall_in_lw_spc(:nlayer)  = lw_flux%wall_in(nspec, :nlayer)
         roof_net_lw_spc(:nlayer) = lw_flux%roof_net(nspec, :nlayer)
         roof_in_lw_spc(:nlayer)  = lw_flux%roof_in(nspec, :nlayer)
      ELSE
         ! No buildings: define wall/roof contributions as zero (not -999) so downstream math is safe
         wall_net_lw_spc(:nlayer) = 0.0D0
         wall_in_lw_spc(:nlayer)  = 0.0D0
         roof_net_lw_spc(:nlayer) = 0.0D0
         roof_in_lw_spc(:nlayer)  = 0.0D0
      END IF
      top_net_lw_spc = lw_flux%top_net(nspec, ncol)
      grnd_net_lw_spc = lw_flux%ground_net(nspec, ncol)
      top_dn_lw_spc = lw_flux%top_dn(nspec, ncol)

      flux_dn_layer_top_lw_spc = -999.0D0
      flux_up_layer_top_lw_spc = -999.0D0
      flux_dn_layer_base_lw_spc = -999.0D0
      flux_up_layer_base_lw_spc = -999.0D0
      IF (config%do_lw .AND. canopy_props%nlay(1) > 0) THEN
         IF (ALLOCATED(lw_flux%flux_dn_layer_top)) THEN
            nlay_flux = SIZE(lw_flux%flux_dn_layer_top, 2)
            max_idx = canopy_props%istartlay(1) + MIN(canopy_props%nlay(1), 15) - 1
            IF (nlay_flux >= max_idx) THEN
               ilay = canopy_props%istartlay(1)
               DO jlay = 1, MIN(canopy_props%nlay(1), 15)
                  flux_dn_layer_top_lw_spc(jlay) = lw_flux%flux_dn_layer_top(nspec, ilay + jlay - 1)
                  flux_up_layer_top_lw_spc(jlay) = lw_flux%flux_up_layer_top(nspec, ilay + jlay - 1)
                  flux_dn_layer_base_lw_spc(jlay) = lw_flux%flux_dn_layer_base(nspec, ilay + jlay - 1)
                  flux_up_layer_base_lw_spc(jlay) = lw_flux%flux_up_layer_base(nspec, ilay + jlay - 1)
               END DO
            END IF
         END IF
      ELSE
         flux_dn_layer_top_lw_spc = 0.0D0
         flux_up_layer_top_lw_spc = 0.0D0
         flux_dn_layer_base_lw_spc = 0.0D0
         flux_up_layer_base_lw_spc = 0.0D0
      END IF

      ! sw arrays — only read from sw_flux when SW was computed
      clear_air_abs_sw_spc = -999
      wall_net_sw_spc = -999
      wall_in_sw_spc = -999
      wall_in_sw_dir_spc = -999
      wall_in_sw_diff_spc = -999
      roof_net_sw_spc = -999
      roof_in_sw_spc = -999
      roof_in_sw_dir_spc = -999
      roof_in_sw_diff_spc = -999
      flux_dn_layer_top_sw_spc = -999.0D0
      flux_up_layer_top_sw_spc = -999.0D0
      flux_dn_layer_base_sw_spc = -999.0D0
      flux_up_layer_base_sw_spc = -999.0D0
      IF (config%do_sw) THEN
         clear_air_abs_sw_spc(:nlayer) = sw_flux%clear_air_abs(nspec, :nlayer)
         IF (config%do_urban) THEN
             wall_net_sw_spc(:nlayer) = sw_flux%wall_net(nspec, :nlayer)
             wall_in_sw_spc(:nlayer) = sw_flux%wall_in(nspec, :nlayer)
             wall_in_sw_dir_spc(:nlayer) = sw_norm_dir%wall_in(nspec, :nlayer)
             wall_in_sw_diff_spc(:nlayer) = sw_norm_diff%wall_in(nspec, :nlayer)
             roof_net_sw_spc(:nlayer) = sw_flux%roof_net(nspec, :nlayer)
             roof_in_sw_spc(:nlayer) = sw_flux%roof_in(nspec, :nlayer)
             roof_in_sw_dir_spc(:nlayer) = sw_norm_dir%roof_in(nspec, :nlayer)
             roof_in_sw_diff_spc(:nlayer) = sw_norm_diff%roof_in(nspec, :nlayer)
         ELSE
             wall_net_sw_spc(:nlayer) = 0.0D0
             wall_in_sw_spc(:nlayer) = 0.0D0
             wall_in_sw_dir_spc(:nlayer) = 0.0D0
             wall_in_sw_diff_spc(:nlayer) = 0.0D0
             roof_net_sw_spc(:nlayer) = 0.0D0
             roof_in_sw_spc(:nlayer) = 0.0D0
             roof_in_sw_dir_spc(:nlayer) = 0.0D0
             roof_in_sw_diff_spc(:nlayer) = 0.0D0
         END IF
         top_dn_dir_sw_spc = sw_flux%top_dn_dir(nspec, ncol)
         top_net_sw_spc = sw_flux%top_net(nspec, ncol)
         grnd_dn_dir_sw_spc = sw_flux%ground_dn_dir(nspec, ncol)
         grnd_net_sw_spc = sw_flux%ground_net(nspec, ncol)
         grnd_vertical_diff = sw_flux%ground_vertical_diff(nspec, ncol)
         grnd_dn_sw_spc = sw_flux%ground_dn(nspec, ncol)
         IF (ALLOCATED(sw_flux%flux_dn_layer_top) .AND. canopy_props%nlay(1) > 0) THEN
            nlay_flux = SIZE(sw_flux%flux_dn_layer_top, 2)
            max_idx = canopy_props%istartlay(1) + MIN(canopy_props%nlay(1), 15) - 1
            IF (nlay_flux >= max_idx) THEN
               ilay = canopy_props%istartlay(1)
               DO jlay = 1, MIN(canopy_props%nlay(1), 15)
                  flux_dn_layer_top_sw_spc(jlay) = sw_flux%flux_dn_layer_top(nspec, ilay + jlay - 1)
                  flux_up_layer_top_sw_spc(jlay) = sw_flux%flux_up_layer_top(nspec, ilay + jlay - 1)
                  flux_dn_layer_base_sw_spc(jlay) = sw_flux%flux_dn_layer_base(nspec, ilay + jlay - 1)
                  flux_up_layer_base_sw_spc(jlay) = sw_flux%flux_up_layer_base(nspec, ilay + jlay - 1)
               END DO
            END IF
         END IF
      ELSE
         clear_air_abs_sw_spc(:nlayer) = 0.0
         wall_net_sw_spc(:nlayer) = 0.0
         wall_in_sw_spc(:nlayer) = 0.0
         wall_in_sw_dir_spc(:nlayer) = 0.0
         wall_in_sw_diff_spc(:nlayer) = 0.0
         roof_net_sw_spc(:nlayer) = 0.0
         roof_in_sw_spc(:nlayer) = 0.0
         roof_in_sw_dir_spc(:nlayer) = 0.0
         roof_in_sw_diff_spc(:nlayer) = 0.0
         top_dn_dir_sw_spc = 0.0
         top_net_sw_spc = 0.0
         grnd_dn_dir_sw_spc = 0.0
         grnd_net_sw_spc = 0.0
         grnd_vertical_diff = 0.0
         grnd_dn_sw_spc = 0.0
         flux_dn_layer_top_sw_spc = 0.0D0
         flux_up_layer_top_sw_spc = 0.0D0
         flux_dn_layer_base_sw_spc = 0.0D0
         flux_up_layer_base_sw_spc = 0.0D0
      END IF

      ! Normalise roof/wall fluxes to their facet area by dividing by surface fraction.
      IF (config%do_sw) THEN
         WHERE (sfr_wall_spc(:nlayer) > eps_fp)
            wall_in_sw_spc(:nlayer) = wall_in_sw_spc(:nlayer)/sfr_wall_spc(:nlayer)
            wall_in_sw_dir_spc(:nlayer) = wall_in_sw_dir_spc(:nlayer)/sfr_wall_spc(:nlayer)
            wall_in_sw_diff_spc(:nlayer) = wall_in_sw_diff_spc(:nlayer)/sfr_wall_spc(:nlayer)
            wall_net_sw_spc(:nlayer) = wall_net_sw_spc(:nlayer)/sfr_wall_spc(:nlayer)
         ELSEWHERE
            wall_in_sw_spc(:nlayer) = 0.0D0
            wall_in_sw_dir_spc(:nlayer) = 0.0D0
            wall_in_sw_diff_spc(:nlayer) = 0.0D0
            wall_net_sw_spc(:nlayer) = 0.0D0
         END WHERE
         WHERE (sfr_roof_spc(:nlayer) > eps_fp)
            roof_in_sw_spc(:nlayer) = roof_in_sw_spc(:nlayer)/sfr_roof_spc(:nlayer)
            roof_in_sw_dir_spc(:nlayer) = roof_in_sw_dir_spc(:nlayer)/sfr_roof_spc(:nlayer)
            roof_in_sw_diff_spc(:nlayer) = roof_in_sw_diff_spc(:nlayer)/sfr_roof_spc(:nlayer)
            roof_net_sw_spc(:nlayer) = roof_net_sw_spc(:nlayer)/sfr_roof_spc(:nlayer)
         ELSEWHERE
            roof_in_sw_spc(:nlayer) = 0.0D0
            roof_in_sw_dir_spc(:nlayer) = 0.0D0
            roof_in_sw_diff_spc(:nlayer) = 0.0D0
            roof_net_sw_spc(:nlayer) = 0.0D0
         END WHERE
      END IF

      WHERE (sfr_wall_spc(:nlayer) > eps_fp)
         wall_in_lw_spc(:nlayer) = wall_in_lw_spc(:nlayer)/sfr_wall_spc(:nlayer)
         wall_net_lw_spc(:nlayer) = wall_net_lw_spc(:nlayer)/sfr_wall_spc(:nlayer)
      ELSEWHERE
         wall_in_lw_spc(:nlayer) = 0.0D0
         wall_net_lw_spc(:nlayer) = 0.0D0
      END WHERE
      WHERE (sfr_roof_spc(:nlayer) > eps_fp)
         roof_in_lw_spc(:nlayer) = roof_in_lw_spc(:nlayer)/sfr_roof_spc(:nlayer)
         roof_net_lw_spc(:nlayer) = roof_net_lw_spc(:nlayer)/sfr_roof_spc(:nlayer)
      ELSEWHERE
         roof_in_lw_spc(:nlayer) = 0.0D0
         roof_net_lw_spc(:nlayer) = 0.0D0
      END WHERE

      !!!!!!!!!!!!!! Bulk KUP, LUP, QSTAR for SUEWS !!!!!!!!!!!!!!

      lup = lw_up_spc
      ! print *, 'lw_up_spc', lw_up_spc
      kup = sw_up_spc
      ! print *, 'sw_up_spc', sw_up_spc
      ! limit the lower limit of qn to avoid issue when used with OHM
      IF (invalid_real(qn_spc)) qn_spc = -600D0
      qn = MAX(qn_spc, -600D0)
      IF (invalid_real(qn)) qn = -600D0
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
      IF (sfr_surf(BldgSurf) > eps_fp) THEN
         qn_surf(BldgSurf) = (qn_spc - DOT_PRODUCT(qn_grnd_ind, sfr_grnd_ind))/sfr_surf(BldgSurf)
      ELSE
         qn_surf(BldgSurf) = 0.0D0
      END IF

      IF (ANY(invalid_real(qn_surf))) THEN
         CALL add_supy_warning('SPARTACUS: surface radiation NaN detected -- using bulk fallback')
         qn_surf = qn
      END IF

      dataOutLineSPARTACUS = &
         [alb_spc, emis_spc, &
          top_dn_dir_sw_spc, &
          top_flux_dn_diffuse_sw, &
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
          lw_dn_grnd, &
          lw_up_grnd, &
          roof_in_sw_spc, &
          roof_in_sw_dir_spc, &
          roof_in_sw_diff_spc, &
          roof_net_sw_spc, &
          wall_in_sw_spc, &
          wall_in_sw_dir_spc, &
          wall_in_sw_diff_spc, &
          wall_net_sw_spc, &
          clear_air_abs_sw_spc, &
          roof_in_lw_spc, &
          roof_net_lw_spc, &
          wall_in_lw_spc, &
          wall_net_lw_spc, &
          sfr_roof_spc, &
          sfr_wall_spc, &
          clear_air_abs_lw_spc, &
          grnd_dn_sw_spc, &
          flux_dn_layer_top_sw_spc, &
          flux_up_layer_top_sw_spc, &
          flux_dn_layer_base_sw_spc, &
          flux_up_layer_base_sw_spc, &
          flux_dn_layer_top_lw_spc, &
          flux_up_layer_top_lw_spc, &
          flux_dn_layer_base_lw_spc, &
          flux_up_layer_base_lw_spc, &
          veg_abs_sw_spc, &
          veg_abs_lw_spc &
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

   SUBROUTINE split_shortwave_epw_disc(kdown, doy, zenith_deg, Tair_C, RH, Press_hPa, fallback_direct_frac, kdirect)
      ! Estimate direct horizontal SW from global horizontal SW using the
      ! DISC/Perez core used by the EPW writer through pvlib DIRINT.
      !
      ! Full pvlib DIRINT uses neighbouring timesteps to correct kt'.  Here
      ! SPARTACUS is called one timestep at a time, so we use pressure-corrected
      ! DISC plus the static DIRINT dew-point/precipitable-water correction.
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: doy
      REAL(KIND(1D0)), INTENT(IN) :: kdown
      REAL(KIND(1D0)), INTENT(IN) :: zenith_deg
      REAL(KIND(1D0)), INTENT(IN) :: Tair_C
      REAL(KIND(1D0)), INTENT(IN) :: RH
      REAL(KIND(1D0)), INTENT(IN) :: Press_hPa
      REAL(KIND(1D0)), INTENT(IN) :: fallback_direct_frac
      REAL(KIND(1D0)), INTENT(OUT) :: kdirect

      REAL(KIND(1D0)) :: a
      REAL(KIND(1D0)) :: abs_airmass
      REAL(KIND(1D0)) :: b
      REAL(KIND(1D0)) :: c
      REAL(KIND(1D0)) :: cos_sza
      REAL(KIND(1D0)) :: dewpoint_c
      REAL(KIND(1D0)) :: delta_kn
      REAL(KIND(1D0)) :: dirint_coeff
      REAL(KIND(1D0)) :: kdir
      REAL(KIND(1D0)) :: ecc
      REAL(KIND(1D0)) :: gamma
      REAL(KIND(1D0)) :: i0_normal
      REAL(KIND(1D0)) :: kn
      REAL(KIND(1D0)) :: kn_clear
      REAL(KIND(1D0)) :: kt
      REAL(KIND(1D0)) :: kt_prime
      REAL(KIND(1D0)) :: kt_prime_factor
      REAL(KIND(1D0)) :: press_pa
      REAL(KIND(1D0)) :: rel_airmass
      REAL(KIND(1D0)) :: rh_safe
      REAL(KIND(1D0)) :: water_cm
      REAL(KIND(1D0)), PARAMETER :: min_cos_zenith = 0.065D0
      REAL(KIND(1D0)), PARAMETER :: max_airmass = 12.0D0
      REAL(KIND(1D0)), PARAMETER :: max_zenith = 87.0D0
      REAL(KIND(1D0)), PARAMETER :: pi = 3.141592653589793D0

      kdirect = MAX(0.0D0, MIN(1.0D0, fallback_direct_frac))*MAX(0.0D0, kdown)
      IF (kdown <= 0.0D0) RETURN
      IF (zenith_deg >= max_zenith) THEN
         kdirect = 0.0D0
         RETURN
      END IF

      cos_sza = COS(zenith_deg*pi/180.0D0)
      IF (cos_sza <= 0.0D0) THEN
         kdirect = 0.0D0
         RETURN
      END IF

      CALL spartacus_spencer_eccentricity(doy, ecc)
      i0_normal = 1370.0D0*ecc
      kt = kdown/(i0_normal*MAX(cos_sza, min_cos_zenith))
      kt = MIN(1.0D0, MAX(0.0D0, kt))

      rel_airmass = 1.0D0/(cos_sza + 0.15D0*(93.885D0 - zenith_deg)**(-1.253D0))
      IF (Press_hPa > 2000.0D0) THEN
         press_pa = Press_hPa
      ELSE IF (Press_hPa > 100.0D0) THEN
         press_pa = Press_hPa*100.0D0
      ELSE
         press_pa = 101325.0D0
      END IF
      abs_airmass = MIN(max_airmass, rel_airmass*press_pa/101325.0D0)
      kt_prime_factor = 1.031D0*EXP(-1.4D0/(0.9D0 + 9.4D0/abs_airmass)) + 0.1D0
      kt_prime = kt/kt_prime_factor
      kt_prime = MIN(1.0D0, MAX(0.0D0, kt_prime))

      water_cm = -1.0D0
      IF (Tair_C > -99.0D0 .AND. RH > 0.0D0) THEN
         rh_safe = MIN(100.0D0, MAX(1.0D-6, RH))
         gamma = LOG(rh_safe/100.0D0) + 17.625D0*Tair_C/(243.04D0 + Tair_C)
         dewpoint_c = 243.04D0*gamma/(17.625D0 - gamma)
         water_cm = EXP(0.07D0*dewpoint_c - 0.075D0)
      END IF

      IF (kt <= 0.6D0) THEN
         a = 0.512D0 + kt*(-1.56D0 + kt*(2.286D0 - 2.222D0*kt))
         b = 0.37D0 + 0.962D0*kt
         c = -0.28D0 + kt*(0.932D0 - 2.048D0*kt)
      ELSE
         a = -5.743D0 + kt*(21.77D0 + kt*(-27.49D0 + 11.56D0*kt))
         b = 41.4D0 + kt*(-118.5D0 + kt*(66.05D0 + 31.9D0*kt))
         c = -47.01D0 + kt*(184.2D0 + kt*(-222.0D0 + 73.81D0*kt))
      END IF

      delta_kn = a + b*EXP(c*abs_airmass)
      kn_clear = 0.866D0 + abs_airmass*(-0.122D0 + abs_airmass*(0.0121D0 &
                 + abs_airmass*(-0.000653D0 + 1.4D-5*abs_airmass)))
      kn = kn_clear - delta_kn
      CALL spartacus_dirint_static_coeff(kt_prime, zenith_deg, water_cm, dirint_coeff)
      kdir = MAX(0.0D0, kn*i0_normal*dirint_coeff)
      kdirect = MIN(kdown, MAX(0.0D0, kdir*cos_sza))

   END SUBROUTINE split_shortwave_epw_disc

   SUBROUTINE split_shortwave_forcing(kdown, kdiff, kdir, zenith_deg, kdirect, kdiffuse, valid)
      ! Convert observed direct-normal and diffuse-horizontal forcing to energy-conserving horizontal
      ! components. Small closure differences are normalized while preserving
      ! the observed direct/diffuse ratio.
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: kdown
      REAL(KIND(1D0)), INTENT(IN) :: kdiff
      REAL(KIND(1D0)), INTENT(IN) :: kdir
      REAL(KIND(1D0)), INTENT(IN) :: zenith_deg
      REAL(KIND(1D0)), INTENT(OUT) :: kdirect
      REAL(KIND(1D0)), INTENT(OUT) :: kdiffuse
      LOGICAL, INTENT(OUT) :: valid

      REAL(KIND(1D0)) :: closure_error
      REAL(KIND(1D0)) :: component_sum
      REAL(KIND(1D0)) :: cos_sza
      REAL(KIND(1D0)) :: scale
      REAL(KIND(1D0)), PARAMETER :: pi = 3.141592653589793D0

      kdirect = 0.0D0
      kdiffuse = 0.0D0
      valid = .FALSE.

      IF (kdown <= 0.0D0) THEN
         valid = .TRUE.
         RETURN
      END IF
      IF (kdiff < 0.0D0 .OR. kdir < 0.0D0) RETURN
      IF (kdiff /= kdiff .OR. kdir /= kdir) RETURN

      cos_sza = MAX(0.0D0, COS(zenith_deg*pi/180.0D0))
      kdirect = kdir*cos_sza
      kdiffuse = kdiff
      component_sum = kdirect + kdiffuse
      IF (component_sum <= 0.0D0) RETURN

      closure_error = ABS(component_sum - kdown)
      IF (kdown > 50.0D0) THEN
         valid = closure_error/kdown <= 0.05D0
      ELSE
         valid = closure_error <= 10.0D0
      END IF

      IF (valid) THEN
         scale = kdown/component_sum
         kdirect = kdirect*scale
         kdiffuse = kdiffuse*scale
      ELSE
         kdirect = 0.0D0
         kdiffuse = 0.0D0
      END IF

   END SUBROUTINE split_shortwave_forcing

   SUBROUTINE spartacus_dirint_static_coeff(kt_prime, zenith_deg, water_cm, coeff)
      ! Static DIRINT coefficient from pvlib's Perez table for the
      ! no-time-series-correction bin (delta kt' bin 7).
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: kt_prime
      REAL(KIND(1D0)), INTENT(IN) :: zenith_deg
      REAL(KIND(1D0)), INTENT(IN) :: water_cm
      REAL(KIND(1D0)), INTENT(OUT) :: coeff

      INTEGER :: i_kt
      INTEGER :: i_w
      INTEGER :: i_z
      REAL(KIND(1D0)), PARAMETER :: coeffs(6, 6, 5) = RESHAPE([ &
         0.582690D0, 0.337440D0, 1.018450D0, 1.071570D0, 1.038590D0, 1.038270D0, &
         0.090100D0, 0.665190D0, 1.288220D0, 0.926800D0, 0.973700D0, 1.010090D0, &
         0.392080D0, 0.424660D0, 0.817410D0, 0.856580D0, 0.957630D0, 0.971740D0, &
         3.241680D0, 0.491640D0, 0.725420D0, 0.794920D0, 0.934760D0, 0.942160D0, &
         3.241680D0, 0.530790D0, 0.700560D0, 0.835570D0, 0.926940D0, 0.904770D0, &
         3.241680D0, 0.204340D0, 0.653880D0, 0.843540D0, 0.960430D0, 0.743440D0, &
         0.582690D0, 0.337440D0, 1.018450D0, 1.071570D0, 1.063200D0, 0.920180D0, &
         0.237000D0, 0.678910D0, 1.082810D0, 0.965030D0, 1.006240D0, 0.895270D0, &
         0.493290D0, 0.529550D0, 0.976160D0, 0.928270D0, 0.985480D0, 0.940560D0, &
         12.494170D0, 0.677610D0, 0.869970D0, 0.912780D0, 0.957870D0, 0.919100D0, &
         12.494170D0, 0.745850D0, 0.801440D0, 0.946150D0, 0.953350D0, 0.852650D0, &
         12.494170D0, 1.157740D0, 0.793120D0, 0.882330D0, 0.881630D0, 0.592190D0, &
         0.229720D0, 0.969110D0, 1.153600D0, 0.958070D0, 1.034440D0, 0.910930D0, &
         0.300040D0, 1.012360D0, 1.286370D0, 0.968520D0, 1.026190D0, 0.773030D0, &
         0.651560D0, 0.966910D0, 0.861300D0, 0.946820D0, 0.991790D0, 0.714880D0, &
         1.620760D0, 0.685610D0, 0.868810D0, 0.960830D0, 0.959640D0, 0.770340D0, &
         1.620760D0, 0.693050D0, 0.961970D0, 0.977090D0, 0.959050D0, 0.708370D0, &
         1.620760D0, 2.003080D0, 0.903320D0, 0.911760D0, 0.775640D0, 0.603060D0, &
         0.892710D0, 1.145730D0, 1.321890D0, 1.114130D0, 1.112780D0, 0.821140D0, &
         0.812470D0, 1.199940D0, 1.166170D0, 1.044910D0, 1.071960D0, 0.816280D0, &
         1.932780D0, 1.033460D0, 0.974780D0, 1.032260D0, 1.050220D0, 0.864380D0, &
         1.375250D0, 1.082400D0, 0.951190D0, 1.057110D0, 0.972510D0, 0.731170D0, &
         1.375250D0, 1.458040D0, 0.906140D0, 1.049350D0, 0.876210D0, 0.493730D0, &
         1.375250D0, 2.622080D0, 0.944070D0, 0.898420D0, 0.596350D0, 0.316930D0, &
         0.569950D0, 1.476400D0, 1.294670D0, 1.127110D0, 1.037800D0, 1.034560D0, &
         0.664970D0, 0.986580D0, 1.119330D0, 1.032310D0, 1.017240D0, 1.011680D0, &
         0.898730D0, 0.958730D0, 1.004580D0, 0.972990D0, 0.987900D0, 1.001650D0, &
         2.331620D0, 0.735410D0, 0.829220D0, 0.947950D0, 0.981640D0, 0.995180D0, &
         2.331620D0, 0.804500D0, 0.823880D0, 0.979970D0, 0.991490D0, 0.949030D0, &
         2.331620D0, 1.409380D0, 0.796130D0, 0.960210D0, 0.937680D0, 0.794390D0 &
         ], [6, 6, 5])

      IF (kt_prime < 0.24D0) THEN
         i_kt = 1
      ELSE IF (kt_prime < 0.40D0) THEN
         i_kt = 2
      ELSE IF (kt_prime < 0.56D0) THEN
         i_kt = 3
      ELSE IF (kt_prime < 0.70D0) THEN
         i_kt = 4
      ELSE IF (kt_prime < 0.80D0) THEN
         i_kt = 5
      ELSE
         i_kt = 6
      END IF

      IF (zenith_deg < 25.0D0) THEN
         i_z = 1
      ELSE IF (zenith_deg < 40.0D0) THEN
         i_z = 2
      ELSE IF (zenith_deg < 55.0D0) THEN
         i_z = 3
      ELSE IF (zenith_deg < 70.0D0) THEN
         i_z = 4
      ELSE IF (zenith_deg < 80.0D0) THEN
         i_z = 5
      ELSE
         i_z = 6
      END IF

      IF (water_cm < 0.0D0) THEN
         i_w = 5
      ELSE IF (water_cm < 1.0D0) THEN
         i_w = 1
      ELSE IF (water_cm < 2.0D0) THEN
         i_w = 2
      ELSE IF (water_cm < 3.0D0) THEN
         i_w = 3
      ELSE
         i_w = 4
      END IF

      coeff = coeffs(i_kt, i_z, i_w)

   END SUBROUTINE spartacus_dirint_static_coeff

   SUBROUTINE spartacus_spencer_eccentricity(jday, ecc)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: jday
      REAL(KIND(1D0)), INTENT(OUT) :: ecc

      REAL(KIND(1D0)) :: b
      REAL(KIND(1D0)), PARAMETER :: pi = 3.141592653589793D0

      b = 2.0D0*pi*jday/365.0D0
      ecc = 1.00011D0 + 0.034221D0*COS(b) + 0.001280D0*SIN(b) &
               + 0.000719D0*COS(2.0D0*b) + 0.000077D0*SIN(2.0D0*b)

   END SUBROUTINE spartacus_spencer_eccentricity

END MODULE module_phys_spartacus

! Backward compatibility alias (deprecated - will be removed in future version)
! TODO: Remove in version 2026.1.0 (deprecated since 2025.10.0)
MODULE SPARTACUS_MODULE
   USE module_phys_spartacus
END MODULE SPARTACUS_MODULE
