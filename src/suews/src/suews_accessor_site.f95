!> @file suews_accessor_site.f95
!> @brief Accessor functions for nested SUEWS_SITE parameters
!>
!> Provides getter and setter functions to access nested site parameters
!> (conductance, etc.) from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_site
   USE module_ctrl_type, ONLY: SUEWS_SITE
   IMPLICIT NONE

CONTAINS

   !> Get conductance parameters from site
   SUBROUTINE get_site_conductance(site, gsmodel, g_max, g_k, g_q_base, g_q_shape, &
         g_t, g_sm, kmax, s1, s2, TH, TL)
      TYPE(SUEWS_SITE), INTENT(IN) :: site
      INTEGER, INTENT(OUT) :: gsmodel
      REAL(KIND(1D0)), INTENT(OUT) :: g_max, g_k, g_q_base, g_q_shape
      REAL(KIND(1D0)), INTENT(OUT) :: g_t, g_sm, kmax, s1, s2, TH, TL

      gsmodel = site%conductance%gsmodel
      g_max = site%conductance%g_max
      g_k = site%conductance%g_k
      g_q_base = site%conductance%g_q_base
      g_q_shape = site%conductance%g_q_shape
      g_t = site%conductance%g_t
      g_sm = site%conductance%g_sm
      kmax = site%conductance%kmax
      s1 = site%conductance%s1
      s2 = site%conductance%s2
      TH = site%conductance%TH
      TL = site%conductance%TL
   END SUBROUTINE get_site_conductance

   !> Set conductance parameters on site
   SUBROUTINE set_site_conductance(site, gsmodel, g_max, g_k, g_q_base, g_q_shape, &
         g_t, g_sm, kmax, s1, s2, TH, TL)
      TYPE(SUEWS_SITE), INTENT(INOUT) :: site
      INTEGER, INTENT(IN) :: gsmodel
      REAL(KIND(1D0)), INTENT(IN) :: g_max, g_k, g_q_base, g_q_shape
      REAL(KIND(1D0)), INTENT(IN) :: g_t, g_sm, kmax, s1, s2, TH, TL

      site%conductance%gsmodel = gsmodel
      site%conductance%g_max = g_max
      site%conductance%g_k = g_k
      site%conductance%g_q_base = g_q_base
      site%conductance%g_q_shape = g_q_shape
      site%conductance%g_t = g_t
      site%conductance%g_sm = g_sm
      site%conductance%kmax = kmax
      site%conductance%s1 = s1
      site%conductance%s2 = s2
      site%conductance%TH = TH
      site%conductance%TL = TL
   END SUBROUTINE set_site_conductance

   !> Get soil parameters for all surfaces
   SUBROUTINE get_site_soil_params(site, soildepth, soilstorecap, sathydraulicconduct)
      TYPE(SUEWS_SITE), INTENT(IN) :: site
      REAL(KIND(1D0)), DIMENSION(7), INTENT(OUT) :: soildepth
      REAL(KIND(1D0)), DIMENSION(7), INTENT(OUT) :: soilstorecap
      REAL(KIND(1D0)), DIMENSION(7), INTENT(OUT) :: sathydraulicconduct

      ! Surface order: paved=1, bldg=2, evetr=3, dectr=4, grass=5, bsoil=6, water=7
      soildepth(1) = site%lc_paved%soil%soildepth
      soildepth(2) = site%lc_bldg%soil%soildepth
      soildepth(3) = site%lc_evetr%soil%soildepth
      soildepth(4) = site%lc_dectr%soil%soildepth
      soildepth(5) = site%lc_grass%soil%soildepth
      soildepth(6) = site%lc_bsoil%soil%soildepth
      soildepth(7) = site%lc_water%soil%soildepth

      soilstorecap(1) = site%lc_paved%soil%soilstorecap
      soilstorecap(2) = site%lc_bldg%soil%soilstorecap
      soilstorecap(3) = site%lc_evetr%soil%soilstorecap
      soilstorecap(4) = site%lc_dectr%soil%soilstorecap
      soilstorecap(5) = site%lc_grass%soil%soilstorecap
      soilstorecap(6) = site%lc_bsoil%soil%soilstorecap
      soilstorecap(7) = site%lc_water%soil%soilstorecap

      sathydraulicconduct(1) = site%lc_paved%soil%sathydraulicconduct
      sathydraulicconduct(2) = site%lc_bldg%soil%sathydraulicconduct
      sathydraulicconduct(3) = site%lc_evetr%soil%sathydraulicconduct
      sathydraulicconduct(4) = site%lc_dectr%soil%sathydraulicconduct
      sathydraulicconduct(5) = site%lc_grass%soil%sathydraulicconduct
      sathydraulicconduct(6) = site%lc_bsoil%soil%sathydraulicconduct
      sathydraulicconduct(7) = site%lc_water%soil%sathydraulicconduct
   END SUBROUTINE get_site_soil_params

   !> Set soil parameters for all surfaces
   SUBROUTINE set_site_soil_params(site, soildepth, soilstorecap, sathydraulicconduct)
      TYPE(SUEWS_SITE), INTENT(INOUT) :: site
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: soildepth
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: soilstorecap
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: sathydraulicconduct

      ! Surface order: paved=1, bldg=2, evetr=3, dectr=4, grass=5, bsoil=6, water=7
      site%lc_paved%soil%soildepth = soildepth(1)
      site%lc_bldg%soil%soildepth = soildepth(2)
      site%lc_evetr%soil%soildepth = soildepth(3)
      site%lc_dectr%soil%soildepth = soildepth(4)
      site%lc_grass%soil%soildepth = soildepth(5)
      site%lc_bsoil%soil%soildepth = soildepth(6)
      site%lc_water%soil%soildepth = soildepth(7)

      site%lc_paved%soil%soilstorecap = soilstorecap(1)
      site%lc_bldg%soil%soilstorecap = soilstorecap(2)
      site%lc_evetr%soil%soilstorecap = soilstorecap(3)
      site%lc_dectr%soil%soilstorecap = soilstorecap(4)
      site%lc_grass%soil%soilstorecap = soilstorecap(5)
      site%lc_bsoil%soil%soilstorecap = soilstorecap(6)
      site%lc_water%soil%soilstorecap = soilstorecap(7)

      site%lc_paved%soil%sathydraulicconduct = sathydraulicconduct(1)
      site%lc_bldg%soil%sathydraulicconduct = sathydraulicconduct(2)
      site%lc_evetr%soil%sathydraulicconduct = sathydraulicconduct(3)
      site%lc_dectr%soil%sathydraulicconduct = sathydraulicconduct(4)
      site%lc_grass%soil%sathydraulicconduct = sathydraulicconduct(5)
      site%lc_bsoil%soil%sathydraulicconduct = sathydraulicconduct(6)
      site%lc_water%soil%sathydraulicconduct = sathydraulicconduct(7)
   END SUBROUTINE set_site_soil_params

   !> Get water limit parameters for all surfaces
   SUBROUTINE get_site_water_limits(site, statelimit, wetthresh)
      TYPE(SUEWS_SITE), INTENT(IN) :: site
      REAL(KIND(1D0)), DIMENSION(7), INTENT(OUT) :: statelimit
      REAL(KIND(1D0)), DIMENSION(7), INTENT(OUT) :: wetthresh

      ! Surface order: paved=1, bldg=2, evetr=3, dectr=4, grass=5, bsoil=6, water=7
      statelimit(1) = site%lc_paved%statelimit
      statelimit(2) = site%lc_bldg%statelimit
      statelimit(3) = site%lc_evetr%statelimit
      statelimit(4) = site%lc_dectr%statelimit
      statelimit(5) = site%lc_grass%statelimit
      statelimit(6) = site%lc_bsoil%statelimit
      statelimit(7) = site%lc_water%statelimit

      wetthresh(1) = site%lc_paved%wetthresh
      wetthresh(2) = site%lc_bldg%wetthresh
      wetthresh(3) = site%lc_evetr%wetthresh
      wetthresh(4) = site%lc_dectr%wetthresh
      wetthresh(5) = site%lc_grass%wetthresh
      wetthresh(6) = site%lc_bsoil%wetthresh
      wetthresh(7) = site%lc_water%wetthresh
   END SUBROUTINE get_site_water_limits

   !> Set water limit parameters for all surfaces
   SUBROUTINE set_site_water_limits(site, statelimit, wetthresh)
      TYPE(SUEWS_SITE), INTENT(INOUT) :: site
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: statelimit
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: wetthresh

      ! Surface order: paved=1, bldg=2, evetr=3, dectr=4, grass=5, bsoil=6, water=7
      site%lc_paved%statelimit = statelimit(1)
      site%lc_bldg%statelimit = statelimit(2)
      site%lc_evetr%statelimit = statelimit(3)
      site%lc_dectr%statelimit = statelimit(4)
      site%lc_grass%statelimit = statelimit(5)
      site%lc_bsoil%statelimit = statelimit(6)
      site%lc_water%statelimit = statelimit(7)

      site%lc_paved%wetthresh = wetthresh(1)
      site%lc_bldg%wetthresh = wetthresh(2)
      site%lc_evetr%wetthresh = wetthresh(3)
      site%lc_dectr%wetthresh = wetthresh(4)
      site%lc_grass%wetthresh = wetthresh(5)
      site%lc_bsoil%wetthresh = wetthresh(6)
      site%lc_water%wetthresh = wetthresh(7)
   END SUBROUTINE set_site_water_limits

   !> Get surface fractions for all land cover types
   SUBROUTINE get_site_sfr(site, sfr)
      TYPE(SUEWS_SITE), INTENT(IN) :: site
      REAL(KIND(1D0)), DIMENSION(7), INTENT(OUT) :: sfr

      ! Surface order: paved=1, bldg=2, evetr=3, dectr=4, grass=5, bsoil=6, water=7
      sfr(1) = site%lc_paved%sfr
      sfr(2) = site%lc_bldg%sfr
      sfr(3) = site%lc_evetr%sfr
      sfr(4) = site%lc_dectr%sfr
      sfr(5) = site%lc_grass%sfr
      sfr(6) = site%lc_bsoil%sfr
      sfr(7) = site%lc_water%sfr
   END SUBROUTINE get_site_sfr

   !> Set surface fractions for all land cover types
   SUBROUTINE set_site_sfr(site, sfr)
      TYPE(SUEWS_SITE), INTENT(INOUT) :: site
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: sfr

      ! Surface order: paved=1, bldg=2, evetr=3, dectr=4, grass=5, bsoil=6, water=7
      site%lc_paved%sfr = sfr(1)
      site%lc_bldg%sfr = sfr(2)
      site%lc_evetr%sfr = sfr(3)
      site%lc_dectr%sfr = sfr(4)
      site%lc_grass%sfr = sfr(5)
      site%lc_bsoil%sfr = sfr(6)
      site%lc_water%sfr = sfr(7)

      ! Also update the site sfr_surf array for consistency
      site%sfr_surf(1) = sfr(1)
      site%sfr_surf(2) = sfr(2)
      site%sfr_surf(3) = sfr(3)
      site%sfr_surf(4) = sfr(4)
      site%sfr_surf(5) = sfr(5)
      site%sfr_surf(6) = sfr(6)
      site%sfr_surf(7) = sfr(7)
   END SUBROUTINE set_site_sfr

   !> Get emissivity for all land cover types
   SUBROUTINE get_site_emis(site, emis)
      TYPE(SUEWS_SITE), INTENT(IN) :: site
      REAL(KIND(1D0)), DIMENSION(7), INTENT(OUT) :: emis

      ! Surface order: paved=1, bldg=2, evetr=3, dectr=4, grass=5, bsoil=6, water=7
      emis(1) = site%lc_paved%emis
      emis(2) = site%lc_bldg%emis
      emis(3) = site%lc_evetr%emis
      emis(4) = site%lc_dectr%emis
      emis(5) = site%lc_grass%emis
      emis(6) = site%lc_bsoil%emis
      emis(7) = site%lc_water%emis
   END SUBROUTINE get_site_emis

   !> Set emissivity for all land cover types
   SUBROUTINE set_site_emis(site, emis)
      TYPE(SUEWS_SITE), INTENT(INOUT) :: site
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: emis

      ! Surface order: paved=1, bldg=2, evetr=3, dectr=4, grass=5, bsoil=6, water=7
      site%lc_paved%emis = emis(1)
      site%lc_bldg%emis = emis(2)
      site%lc_evetr%emis = emis(3)
      site%lc_dectr%emis = emis(4)
      site%lc_grass%emis = emis(5)
      site%lc_bsoil%emis = emis(6)
      site%lc_water%emis = emis(7)
   END SUBROUTINE set_site_emis

END MODULE module_accessor_site
