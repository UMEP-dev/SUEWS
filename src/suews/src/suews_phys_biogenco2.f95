! Main module following naming standard: matches filename
MODULE module_phys_biogenco2
   IMPLICIT NONE

CONTAINS
!========================================================================================
! Created by HCW Aug 2016 to calculate biogenic component of CO2 flux.
! This subroutine is still under development and in the equations there might be bugs and
! the code is not well commented.
!
! Last modified:
! MH 18 Feb 2019  - Added new method to calculate photosynthesis with conductance parameters and added option to model with
!                   2 meter temperature
! MH 20 Jun 2017  - Tidied and renamed from SUEWS_CO2.f95 to SUEWS_CO2Biogen.f95
! HCW 11 Apr 2017 - Tidied and merged with LJ code
! LJ   6 Apr 2017 - Minimum limit for soil respiration with BiogenCO2Choice = 2 was set to 0.6 umol m-2 s-1
!                 - Choice for non-rectancular hyperbola to calculate the biogenic CO2 flux added (BiogenCO2Choice = 2)
!                    (Bellucco et al. 2017, Agric. Forest. Met. 236, 113-122).
!                 - Both the "local Helsinki model" (BiogenCO2Choice = 2)) and "general model" BiogenCO2Choice = 3) are implemented
!                 - Snow fraction added to the calculation of active vegetation fraction and the soil respiration
!
! To Do:
!  - Active vegetation goes to zero with LAI minimum, but this needs to be changed so some minimum value
!    especially in the case of evergreentrees
!  - Move some of the parameters to input files
!  - Now on weekend nighttime population is used throughout the day. Do we need extra column in SiteSelect for daytime weekend population?

! EmissionsMethod:
!  11-16 - Rectangular hyperbola (Ruimy, Schmid, Flanagan)
!  21-26 - Non-rectangular hyperbola, Helsinki (Bellucco et al. 2017)
!  31-36 - Non-rectangular hyperbola, general  (Bellucco et al. 2017)
!  41-46 - Rectangular hyperbola, calculated with conductance parameters (Järvi et al., 2019)
!========================================================================================
   SUBROUTINE CO2_biogen( &
      alpha_bioCO2, alpha_enh_bioCO2, avkdn, beta_bioCO2, beta_enh_bioCO2, BSoilSurf, & ! input:
      ConifSurf, DecidSurf, dectime, EmissionsMethod, gfunc, gfunc2, GrassSurf, gsmodel, &
      id, it, ivConif, ivDecid, ivGrass, LAI_id, LAIMin, LAIMax, min_res_bioCO2, nsurf, &
      NVegSurf, resp_a, resp_b, sfr_surf, SnowFrac, t2, Temp_C, theta_bioCO2, &
      Fc_biogen, Fc_photo, Fc_respi) ! output:

      IMPLICIT NONE

      INTEGER, INTENT(in) :: &
         EmissionsMethod, id, it, ivConif, ivDecid, ivGrass, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, &
         nsurf, nvegSurf, gsmodel

      REAL(KIND(1D0)), INTENT(in) :: &
         avkdn, &
         dectime, &
         gfunc, &
         gfunc2, &
         Temp_C, &
         t2

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: &
         sfr_surf, & !Surface fractions [-]
         SnowFrac

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: &
         LAIMin, LAIMax, & ! [m2 m-2]
         alpha_bioCO2, &
         beta_bioCO2, &
         theta_bioCO2, &
         alpha_enh_bioCO2, &
         beta_enh_bioCO2, &
         resp_a, &
         resp_b, &
         min_res_bioCO2, &
         LAI_id

      REAL(KIND(1D0)), INTENT(out) :: &
         Fc_biogen, &
         Fc_respi, Fc_photo

      INTEGER :: iv ! counter

      REAL(KIND(1D0)) :: &
         PAR_umolm2s1, &
         Bellucco2017_Pho, & ! Photosynthesis (Bellucco et al. 2016)
         Bellucco2017_Res, & ! Respiration (Bellucco et al. 2016)
         Bellucco2017_Res_surf, & ! Respiration for each vegetated surface
         VegFracSum ! Sum of vegetation fractions without water. Could be moved elsewhere later.

      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: &
         active_veg_fr, & ! Active vegetation fraction
         active_veg_fr0, & ! Active vegetation fraction without LAI
         Fc_photo_surf, & ! Photosynthesis for each vegetated surface
         Bellucco2017_Pho_surf, & ! Photosynthesis for each vegetated surface
         alpha_bioCO2_v2, &
         beta_bioCO2_v2, &
         theta_bioCO2_v2

      REAL(KIND(1D0)), PARAMETER :: &
         JtoumolPAR = 4.6, &
         KdntoPAR = 0.46

      !-----------------------------------------------------------------------
      ! Calculate PAR from Kdown ----------------------------------
      PAR_umolm2s1 = JtoumolPAR*KdntoPAR*avKdn
      VegFracSum = sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf)

      ! Calculate active vegetation surface -----------------------
      !Now this is zero always when LAI in its minimum values. This needs to vary between the summer time maximum and some minimum value
      !especially in the case of evergreen trees (i.e. in early March LAI can be in its minimum value, but air temperature and radiation
      !such that uptake can take place)
      DO iv = ivConif, ivGrass !For vegetated surfaces. Snow included although quite often LAI will be in its minimum when snow on ground

         active_veg_fr(iv) = (sfr_surf(iv + 2)*(1 - SnowFrac(iv + 2)))*(LAI_id(iv)/LAIMax(iv))
         active_veg_fr0(iv) = (sfr_surf(iv + 2)*(1 - SnowFrac(iv + 2)))*LAI_id(iv)

      END DO

      IF (EmissionsMethod >= 11 .AND. EmissionsMethod <= 16) THEN ! Rectangular hyperbola
         ! Calculate carbon uptake due to photosynthesis -------------
         Fc_photo = 0
         DO iv = ivConif, ivGrass
            Fc_photo_surf(iv) = -beta_bioCO2(iv)*alpha_bioCO2(iv)*PAR_umolm2s1/(alpha_bioCO2(iv)*PAR_umolm2s1 + beta_bioCO2(iv))
            ! For active vegetation fraction only
            Fc_photo = Fc_photo + Fc_photo_surf(iv)*active_veg_fr(iv) !umol m-2 s-1
         END DO

      ELSEIF (EmissionsMethod >= 21 .AND. EmissionsMethod <= 26) THEN !Local model, Bellucco et al. (2017)
         ! Calculate carbon uptake due to photosynthesis -------------
         Bellucco2017_Pho = 0
         DO iv = ivConif, ivGrass
            Bellucco2017_Pho_surf(iv) = -(1/(2*theta_bioCO2(iv)) &
                                          *(alpha_bioCO2(iv)*PAR_umolm2s1 + beta_bioCO2(iv) &
                                            - SQRT((alpha_bioCO2(iv)*PAR_umolm2s1 + beta_bioCO2(iv))**2 &
                                                   - 4*alpha_bioCO2(iv)*beta_bioCO2(iv)*theta_bioCO2(iv)*PAR_umolm2s1)))

            ! For active vegetation fraction only
            Bellucco2017_Pho = Bellucco2017_Pho + Bellucco2017_Pho_surf(iv)*active_veg_fr(iv)
         END DO
         Fc_photo = Bellucco2017_Pho

      ELSEIF (EmissionsMethod >= 31 .AND. EmissionsMethod <= 36) THEN !General model, Bellucco et al. (2017)
         ! Not currently recommended as includes also some anthropogenic impacts. Should maybe be separate from other biogen choices?
         ! Alpha and beta calculated as a function of vegetation cover fraction

         !Different alpha, beta and theta vegetation cover values should be same in BiogenCO2Method = 3.
         IF (alpha_bioCO2(ivConif) == alpha_bioCO2(ivDecid) .AND. alpha_bioCO2(ivConif) == alpha_bioCO2(ivGrass) .AND. &
             beta_bioCO2(ivConif) == beta_bioCO2(ivDecid) .AND. beta_bioCO2(ivConif) == beta_bioCO2(ivGrass) .AND. &
             theta_bioCO2(ivConif) == theta_bioCO2(ivDecid) .AND. theta_bioCO2(ivConif) == theta_bioCO2(ivGrass)) THEN

            !Because different alpha, beta and theta values are same - only one vegetation type is needed.
            alpha_bioCO2_v2(ivConif) = alpha_bioCO2(ivConif) + alpha_enh_bioCO2(ivConif)* &
                                       (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf) + sfr_surf(BSoilSurf)) !umol CO2 umol photons-1
            beta_bioCO2_v2(ivConif) = -beta_bioCO2(ivConif) + beta_enh_bioCO2(ivConif)* &
                                      (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf) + sfr_surf(BSoilSurf)) !umol m^-2 s^-1

            !Photosynthesis
            Bellucco2017_Pho = -(1/(2*theta_bioCO2(ivConif)) &
                                 *(alpha_bioCO2_v2(ivConif)*PAR_umolm2s1 &
                                   + beta_bioCO2_v2(ivConif) &
                                   - SQRT((alpha_bioCO2_v2(ivConif)*PAR_umolm2s1 + beta_bioCO2_v2(ivConif))**2 - 4* &
                                          alpha_bioCO2_v2(ivConif)*beta_bioCO2_v2(ivConif)*theta_bioCO2(ivConif)*PAR_umolm2s1)))

         ELSE !If values are not same, then weighted average is calculated.
            ! CALL ErrorHint(74, 'Check values in SUEWS_BiogenCO2.txt: ', notUsed, notUsed, notUsedI)

            ! Weighted averages
            alpha_bioCO2_v2(ivConif) = (alpha_bioCO2(ivConif)*sfr_surf(ConifSurf)/VegFracSum + &
                                        alpha_bioCO2(ivDecid)*sfr_surf(DecidSurf)/VegFracSum &
                                        + alpha_bioCO2(ivGrass)*sfr_surf(GrassSurf)/VegFracSum) &
                                       /(alpha_bioCO2(ivConif) + alpha_bioCO2(ivDecid) + alpha_bioCO2(ivGrass))
            beta_bioCO2_v2(ivConif) = (beta_bioCO2(ivConif)*sfr_surf(ConifSurf)/VegFracSum + &
                                       beta_bioCO2(ivDecid)*sfr_surf(DecidSurf)/VegFracSum &
                                       + beta_bioCO2(ivGrass)*sfr_surf(GrassSurf)/VegFracSum) &
                                      /(beta_bioCO2(ivConif) + beta_bioCO2(ivDecid) + beta_bioCO2(ivGrass))
            theta_bioCO2_v2(ivConif) = (theta_bioCO2(ivConif)*sfr_surf(ConifSurf)/VegFracSum + &
                                        theta_bioCO2(ivDecid)*sfr_surf(DecidSurf)/VegFracSum &
                                        + theta_bioCO2(ivGrass)*sfr_surf(GrassSurf)/VegFracSum) &
                                       /(theta_bioCO2(ivConif) + theta_bioCO2(ivDecid) + theta_bioCO2(ivGrass))

            ! Using weighted average values to calculate alpha and beta as a function of vegetation cover fraction
            alpha_bioCO2_v2(ivConif) = alpha_bioCO2_v2(ivConif) + alpha_enh_bioCO2(ivConif)* &
                                       (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf) + sfr_surf(BSoilSurf)) !umol CO2 umol photons-1
            beta_bioCO2_v2(ivConif) = -beta_bioCO2_v2(ivConif) + beta_enh_bioCO2(ivConif)* &
                                      (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf) + sfr_surf(BSoilSurf)) !umol m^-2 s^-1

            !Photosynthesis
            Bellucco2017_Pho = -(1/(2*theta_bioCO2_v2(ivConif))*( &
                                 alpha_bioCO2_v2(ivConif)*PAR_umolm2s1 + beta_bioCO2_v2(ivConif) &
                                 - SQRT((alpha_bioCO2_v2(ivConif)*PAR_umolm2s1 + beta_bioCO2_v2(ivConif))**2 &
                                        - 4*alpha_bioCO2_v2(ivConif)*beta_bioCO2_v2(ivConif)*theta_bioCO2_v2(ivConif) &
                                        *PAR_umolm2s1)))

         END IF
         ! Calculate carbon uptake due to photosynthesis -------------
         Fc_photo = Bellucco2017_Pho*active_veg_fr(ConifSurf - 2) + &
                    Bellucco2017_Pho*active_veg_fr(DecidSurf - 2) + &
                    Bellucco2017_Pho*active_veg_fr(GrassSurf - 2)

      ELSEIF (EmissionsMethod >= 41 .AND. EmissionsMethod <= 46) THEN ! Conductance parameters
         !Dependance to incoming shortwave radiation, specific humidity deficit, air temperature and soil moisture deficit
         Fc_photo = 0
         DO iv = ivConif, ivGrass
            Fc_photo = Fc_photo + active_veg_fr0(iv)*beta_bioCO2(iv)
         END DO

         IF (gsmodel == 1 .OR. gsmodel == 2) THEN !With air temperature
            Fc_photo = -Fc_Photo*gfunc
         ELSEIF (gsmodel == 3 .OR. gsmodel == 4) THEN !With modelled 2 meter temperature
            Fc_photo = -Fc_Photo*gfunc2
         END IF

      END IF

      ! Calculate carbon emissions due to respiration -------------
      Bellucco2017_Res = 0.0
      Bellucco2017_Res_surf = 0.0
      IF (VegFracSum > 0.01) THEN
         DO iv = ivConif, ivGrass
            IF (sfr_surf(2 + iv) > 0.005) THEN
               IF (gsmodel == 1 .OR. gsmodel == 2) THEN !With air temperature
                  Bellucco2017_Res_surf = MAX(min_res_bioCO2(iv), resp_a(iv)*EXP(resp_b(iv)*Temp_C))
               ELSEIF (gsmodel == 3 .OR. gsmodel == 4) THEN !With modelled 2 meter temperature
                  Bellucco2017_Res_surf = MAX(min_res_bioCO2(iv), resp_a(iv)*EXP(resp_b(iv)*t2))
               END IF
               ! Only for vegetation fraction
               Bellucco2017_Res = Bellucco2017_Res + Bellucco2017_Res_surf*sfr_surf(2 + iv)/VegFracSum
            END IF
         END DO
      END IF
      Fc_respi = Bellucco2017_Res*(sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf) + sfr_surf(BSoilSurf))
      ! Combine to find biogenic CO2 flux
      Fc_biogen = Fc_photo + Fc_respi

      RETURN

   END SUBROUTINE CO2_biogen
!========================================================================================

END MODULE module_phys_biogenco2

! Backward compatibility alias
MODULE CO2_module
   USE module_phys_biogenco2
END MODULE CO2_module
