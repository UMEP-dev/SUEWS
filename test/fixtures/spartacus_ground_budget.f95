! One-layer radiation probe: run the production routine at fixed temperatures.
PROGRAM ground_budget
   USE module_phys_spartacus, ONLY: SPARTACUS, allocate_surface_shortwave
   USE module_ctrl_const_allocate, ONLY: ncolumnsDataOutSPARTACUS
   USE module_ctrl_type, ONLY: error_state
   USE module_ctrl_error_state, ONLY: reset_supy_error, get_supy_error, supy_error_flag
   IMPLICIT NONE
   TYPE(error_state) :: errors
   REAL(KIND(1D0)) :: fractions(7), albedo(7), temperature(7), emissivity(7)
   REAL(KIND(1D0)) :: qn, kup, lup, qn_roof(1), qn_wall(1), qn_surf(7)
   REAL(KIND(1D0)) :: roof_sw(15), roof_lw(15), wall_sw(15), wall_lw(15)
   REAL(KIND(1D0)) :: diagnostic(ncolumnsDataOutSPARTACUS - 5)
   REAL(KIND(1D0)) :: multiplier, incoming, direct_fraction, roof_multiplier
   INTEGER :: direct_albedo, allocation_only, error_code
   REAL(KIND(1D0)) :: ground_budget_input
   CHARACTER(LEN=512) :: error_message

   READ (*, *) fractions
   READ (*, *) albedo
   READ (*, *) incoming, direct_fraction, direct_albedo, multiplier, roof_multiplier
   READ (*, *) allocation_only, ground_budget_input
   CALL reset_supy_error()
   CALL errors%reset()
   temperature = 20.0D0
   emissivity = 0.95D0
   IF (allocation_only == 1) THEN
      diagnostic = 0.0D0
      qn = 0.0D0
      qn_roof = 0.0D0
      qn_wall = 0.0D0
      CALL allocate_surface_shortwave(fractions, albedo, direct_albedo == 1, multiplier, &
         incoming*direct_fraction, incoming, ground_budget_input, 0.0D0, qn_surf, errors)
   ELSE
      CALL SPARTACUS( &
         DiagQN=0, sfr_surf=fractions, zenith_deg=45.0D0, nlayer=1, &
         tsfc_surf=temperature, tsfc_roof=[20.0D0], tsfc_wall=[20.0D0], &
         kdown=incoming, kdown_direct=incoming*direct_fraction, &
         kdown_diffuse=incoming*(1.0D0 - direct_fraction), ldown=350.0D0, Tair_C=20.0D0, &
         rsl_z=[0.0D0, 20.0D0], rsl_t_C=[20.0D0, 20.0D0], &
         alb_surf=albedo, emis_surf=emissivity, LAI_id=[2.0D0, 2.0D0, 2.0D0], &
         n_vegetation_region_urban=1, n_vegetation_region_forest=1, &
         n_stream_sw_urban=4, n_stream_sw_forest=1, n_stream_lw_urban=4, n_stream_lw_forest=4, &
         air_ext_sw=0.0D0, air_ssa_sw=0.0D0, veg_ssa_sw=0.46D0, &
         air_ext_lw=0.0D0, air_ssa_lw=0.0D0, veg_ssa_lw=0.06D0, &
         veg_fsd_const=0.75D0, veg_contact_fraction_const=0.0D0, &
         ground_albedo_dir_mult_fact=multiplier, use_sw_direct_albedo=direct_albedo == 1, &
         height=[0.0D0, 10.0D0], bldgH=10.0D0, &
         sfr_evetr=fractions(3), sfr_dectr=fractions(4), EveTreeH=10.0D0, DecTreeH=10.0D0, &
         building_frac=[fractions(2)], veg_frac=[SUM(fractions(3:4))], &
         sfr_roof=[fractions(2)], sfr_wall=[4.0D0*fractions(2)], &
         building_scale=[10.0D0], veg_scale=[10.0D0], veg_ext_input=[-999.0D0], &
         alb_roof=[0.2D0], emis_roof=[0.95D0], alb_wall=[0.2D0], emis_wall=[0.95D0], &
         roof_albedo_dir_mult_fact=RESHAPE([roof_multiplier], [1, 1]), wall_specular_frac=RESHAPE([0.0D0], [1, 1]), &
         qn=qn, kup=kup, lup=lup, qn_roof=qn_roof, qn_wall=qn_wall, qn_surf=qn_surf, &
         roof_in_sw_spc=roof_sw, roof_in_lw_spc=roof_lw, wall_in_sw_spc=wall_sw, wall_in_lw_spc=wall_lw, &
         dataOutLineSPARTACUS=diagnostic, errorState=errors)
   END IF
   CALL get_supy_error(error_code, error_message)
   ! Registry slots: KNet_Grnd=14, LNet_Grnd=15, KDnDir_Grnd=12, ground total=258.
   WRITE (*, '(21(ES24.16,1X))') qn_surf, qn, diagnostic(14:15), diagnostic(12), &
      diagnostic(258), qn_roof, qn_wall, diagnostic(379), &
      diagnostic(9:10), diagnostic(63), diagnostic(123), diagnostic(138), diagnostic(243)
   WRITE (*, '(3(I0,1X))') MERGE(1, 0, supy_error_flag()), error_code, errors%count
   WRITE (*, '(A)') TRIM(error_message)
   IF (errors%count > 0) WRITE (*, '(A)') TRIM(errors%log(1)%message)
END PROGRAM ground_budget
