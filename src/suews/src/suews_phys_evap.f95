! Main module following naming standard: matches filename
MODULE suews_phys_evap
   IMPLICIT NONE

CONTAINS
   SUBROUTINE cal_evap( &
      EvapMethod, state_is, WetThresh_is, capStore_is, & !input
      vpd_hPa, avdens, avcp, qn_e, s_hPa, psyc_hPa, RS, RA, RB, tlv, &
      RSS, ev, qe) !output
      ! RSS, ev, qe) !output
      !------------------------------------------------------------------------------
      !-Calculates evaporation for each surface from modified Penman-Monteith eqn
      !-State determines whether each surface type is dry or wet (wet/transition)
      !-Wet surfaces below storage capacity are in transition
      ! and QE depends on the state and storage capacity (i.e. varies with surface);
      ! for wet or dry surfaces QE does not vary between surface types
      !-See Sect 2.4 of Jarvi et al. (2011) Ja11
      !
      !Last modified:
      !  HCW 06 Jul 2016
      !   Moved rss declaration to LUMPS_Module_Constants so it can be written out
      !  HCW 11 Jun 2015
      !   Added WetThresh to distinguish wet/partially wet surfaces from the storage capacities used in SUEWS_drain
      !  HCW 30 Jan 2015
      !   Removed StorCap input because it is provided by module allocateArray
      !   Tidied and commented code
      !  LJ 10/2010
      !------------------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER, INTENT(in) :: EvapMethod !Evaporation calculated according to Rutter (1) or Shuttleworth (2)

      REAL(KIND(1D0)), INTENT(in) :: state_is ! wetness status
      REAL(KIND(1D0)), INTENT(in) :: WetThresh_is !When State > WetThresh, RS=0 limit in SUEWS_evap [mm] (specified in input files)
      REAL(KIND(1D0)), INTENT(in) :: capStore_is ! = StoreDrainPrm(6,is), current storage capacity [mm]

      REAL(KIND(1D0)), INTENT(in) :: vpd_hPa ! vapour pressure deficit [hPa]
      REAL(KIND(1D0)), INTENT(in) :: avdens ! air density
      REAL(KIND(1D0)), INTENT(in) :: avcp ! air heat capacity
      REAL(KIND(1D0)), INTENT(in) :: qn_e !net available energy for evaporation
      REAL(KIND(1D0)), INTENT(in) :: s_hPa !Vapour pressure versus temperature slope in hPa
      REAL(KIND(1D0)), INTENT(in) :: psyc_hPa !Psychometric constant in hPa
      REAL(KIND(1D0)), INTENT(in) :: RS !Surface resistance
      ! REAL(KIND(1d0)),INTENT(in)::sp!Term in calculation of E
      REAL(KIND(1D0)), INTENT(in) :: RA !Aerodynamic resistance
      REAL(KIND(1D0)), INTENT(in) :: RB !Boundary layer resistance
      REAL(KIND(1D0)), INTENT(in) :: tlv !Latent heat of vaporization per timestep [J kg-1 s-1], (tlv=lv_J_kg/tstep_real)

      REAL(KIND(1D0)), INTENT(out) :: RSS !Redefined surface resistance for wet
      REAL(KIND(1D0)), INTENT(out) :: ev ! evapotranspiration [mm]
      REAL(KIND(1D0)), INTENT(out) :: qe ! latent heat flux [W m-2]

      REAL(KIND(1D0)) :: numPM !numerator of P-M eqn
      REAL(KIND(1D0)) :: RB_SG !Boundary-layer resistance x (slope/psychrometric const + 1) [s m-1]
      REAL(KIND(1D0)) :: rsrbsg !RS + rbsg [s m-1]
      REAL(KIND(1D0)) :: flag_dry
      REAL(KIND(1D0)) :: W !Depends on the amount of water on the canopy [-]
      REAL(KIND(1D0)) :: x
      REAL(KIND(1D0)) :: r

      REAL(KIND(1D0)), PARAMETER :: NAN = -999

      ! Use Penman-Monteith eqn modified for urban areas (Eq6, Jarvi et al. 2011)
      ! Calculation independent of surface characteristics
      ! Uses value of RS for whole area (calculated based on LAI of veg surfaces in SUEWS_SurfaceResistance.f95)

      !numerator of P-M eqn, refer to Eq6, Jarvi et al. 2011
      numPM = s_hPa*qn_e + vpd_hPa*avdens*avcp/RA !s_haPa - slope of svp vs t curve.

      IF (state_is <= 0.001) THEN
         ! Dry surface ---------------------------------------------------------------
         qe = numPM/(s_hPa + psyc_hPa*(1 + RS/RA)) !QE [W m-2] (numPM = numerator of P-M eqn)
         ev = qe/tlv !Ev [mm] (qe[W m-2]/tlv[J kg-1 s-1]*1/density_water[1000 kg m-3])
         W = NAN !W not needed for dry surfaces (set to -999)
         flag_dry = 1 !Set flag indicating dry surface(1)
         RSS = RS

      ELSE
         ! Wet surface ---------------------------------------------------------------
         flag_dry = 0 !Set flag=0 indicating wet surface(0)

         ! Evaporation calculated according to Rutter(EvapMethod=1) or Shuttleworth(EvapMethod=2).
         !Set in SUEWS_initial (so not an input to the model)
         IF (EvapMethod == 2) THEN !-- Shuttleworth (1978): https://doi.org/10.1007/bf00123986 --
            RB_SG = RB*(s_hPa/psyc_hPa + 1) !Boundary-layer resistance x (slope/psychro + 1)
            rsrbsg = RS + RB_SG !RS + rsbg

            ! If surface is completely wet, set RS to zero -------------------
            !if(state(is)>=StoreDrainPrm(6,is).or.ResistSurf<25) then   !If at storage capacity or RS is small
            IF (state_is >= WetThresh_is .OR. RS < 25) THEN !If at storage capacity or RS is small
               W = 1 !So that RS=0 (Eq7, Jarvi et al. 2011)
               ! If surface is in transition, use rss ---------------------------
            ELSE !if((state(is)<StorCap).and.(state(is)>0.001).or.(ResistSurf<50)) then
               r = (RS/RA)*(RA - RB)/rsrbsg
               W = (r - 1)/(r - (WetThresh_is/state_is))
            END IF

            ! PRINT*, 'r',r
            ! PRINT*, 'W',W

            RSS = (1/((W/RB_SG) + ((1 - W)/rsrbsg))) - RB_SG !Redefined surface resistance for wet
            ! PRINT*, 'resistances:',rbsg,rsrbsg,rss
            !surfaces (zero if W=1). Eq7, Jarvi et al. (2011)
            qe = numPM/(s_hPa + psyc_hPa*(1 + RSS/RA)) !QE [W m-2]
            ev = qe/tlv !Ev [mm]
            ! PRINT*, 'numPM',numPM
            ! PRINT*, 'qe',qe

         ELSEIF (EvapMethod == 1) THEN !-- Rutter --
            qe = numPM/(s_hPa + psyc_hPa)
            ev = qe/tlv

            x = MERGE(1D0, state_is/capStore_is, state_is > capStore_is)
            ev = ev*x !QE [W m-2]
            qe = ev*tlv !Ev [mm]
         END IF !Rutter/Shuttleworth calculation
      END IF !Wet/dry surface

   END SUBROUTINE cal_evap

   SUBROUTINE cal_evap_multi( &
      EvapMethod, & !input
      sfr_multi, state_multi, WetThresh_multi, capStore_multi, & !input
      vpd_hPa, avdens, avcp, qn_e_multi, s_hPa, psyc_hPa, RS, RA, RB, tlv, &
      RSS_multi, ev_multi, qe_multi) !output
      IMPLICIT NONE
      INTEGER, INTENT(in) :: EvapMethod !Evaporation calculated according to Rutter (1) or Shuttleworth (2)

      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: sfr_multi ! facet fraction of surface
      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: state_multi ! wetness status
      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: WetThresh_multi !When State > WetThresh, RS=0 limit in SUEWS_evap [mm] (specified in input files)
      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: capStore_multi ! = StoreDrainPrm(6,is), current storage capacity [mm]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: qn_e_multi !net available energy for evaporation [W m-2]

      REAL(KIND(1D0)), INTENT(in) :: vpd_hPa ! vapour pressure deficit [hPa]
      REAL(KIND(1D0)), INTENT(in) :: avdens ! air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: avcp ! air heat capacity [J kg-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: s_hPa !Vapour pressure versus temperature slope [hPa K-1]]
      REAL(KIND(1D0)), INTENT(in) :: psyc_hPa !Psychometric constant [hPa]
      REAL(KIND(1D0)), INTENT(in) :: RS !Surface resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: RA !Aerodynamic resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: RB !Boundary layer resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: tlv !Latent heat of vaporization per timestep [J kg-1 s-1], (tlv=lv_J_kg/tstep_real)

      REAL(KIND(1D0)), DIMENSION(:), INTENT(out) :: RSS_multi !Redefined surface resistance for wet surfaces [s m-1]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(out) :: ev_multi ! evapotranspiration [mm]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(out) :: qe_multi ! latent heat flux [W m-2]
      ! REAL(KIND(1D0)), INTENT(out) :: qe_total ! latent heat flux [W m-2]

      ! REAL(KIND(1D0)) :: numPM !numerator of P-M eqn
      ! REAL(KIND(1D0)) :: RB_SG !Boundary-layer resistance x (slope/psychrometric const + 1) [s m-1]
      ! REAL(KIND(1D0)) :: rsrbsg !RS + rbsg [s m-1]
      ! REAL(KIND(1D0)) :: flag_dry
      ! REAL(KIND(1D0)) :: W !Depends on the amount of water on the canopy [-]
      ! REAL(KIND(1D0)) :: x
      INTEGER :: n_facet !number of facets
      INTEGER :: i

      REAL(KIND(1D0)), PARAMETER :: NAN = -999

      n_facet = SIZE(sfr_multi)

      DO i = 1, n_facet
         CALL cal_evap( &
            EvapMethod, state_multi(i), WetThresh_multi(i), capStore_multi(i), &
            vpd_hPa, avdens, avcp, qn_e_multi(i), s_hPa, psyc_hPa, RS, RA, RB, tlv, &
            RSS_multi(i), ev_multi(i), qe_multi(i))
      END DO

      ! Sum latent heat flux from different surfaces to find total latent heat flux
      ! qe_total = DOT_PRODUCT(qe_multi, sfr_multi)

   END SUBROUTINE cal_evap_multi

END MODULE suews_phys_evap

! Backward compatibility alias (deprecated - will be removed in future version)
! TODO: Remove in version 2026.1.0 (deprecated since 2025.10.0)
MODULE evap_module
   USE suews_phys_evap
END MODULE evap_module
