! One-layer radiation probe: run the production routine at fixed temperatures.
PROGRAM ground_budget
   USE module_phys_spartacus, ONLY: SPARTACUS
   USE module_ctrl_const_allocate, ONLY: ncolumnsDataOutSPARTACUS
   USE module_ctrl_type, ONLY: error_state
   IMPLICIT NONE
   TYPE(error_state) :: errors
   REAL(KIND(1D0)) :: fractions(7), albedo(7), temperature(7), emissivity(7)
   REAL(KIND(1D0)) :: qn, kup, lup, qn_roof(1), qn_wall(1), qn_surf(7)
   REAL(KIND(1D0)) :: roof_sw(15), roof_lw(15), wall_sw(15), wall_lw(15)
   REAL(KIND(1D0)) :: diagnostic(ncolumnsDataOutSPARTACUS - 5)
   REAL(KIND(1D0)) :: multiplier, incoming, direct_fraction
   INTEGER :: direct_albedo

   READ (*, *) fractions
   READ (*, *) albedo
   READ (*, *) incoming, direct_fraction, direct_albedo, multiplier
   temperature = 20.0D0
   emissivity = 0.95D0
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
      sfr_roof=[fractions(2)], sfr_wall=[fractions(2)], &
      building_scale=[10.0D0], veg_scale=[10.0D0], veg_ext_input=[-999.0D0], &
      alb_roof=[0.2D0], emis_roof=[0.95D0], alb_wall=[0.2D0], emis_wall=[0.95D0], &
      roof_albedo_dir_mult_fact=RESHAPE([1.0D0], [1, 1]), wall_specular_frac=RESHAPE([0.0D0], [1, 1]), &
      qn=qn, kup=kup, lup=lup, qn_roof=qn_roof, qn_wall=qn_wall, qn_surf=qn_surf, &
      roof_in_sw_spc=roof_sw, roof_in_lw_spc=roof_lw, wall_in_sw_spc=wall_sw, wall_in_lw_spc=wall_lw, &
      dataOutLineSPARTACUS=diagnostic, errorState=errors)
   ! Registry slots: KNet_Grnd=14, LNet_Grnd=15, KDnDir_Grnd=12, ground total=258.
   WRITE (*, '(14(ES24.16,1X))') qn_surf, qn, diagnostic(14:15), diagnostic(12), &
      diagnostic(258), qn_roof, qn_wall
END PROGRAM ground_budget
