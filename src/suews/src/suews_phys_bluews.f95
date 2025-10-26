!======================================================================================================
! Main module following naming standard: matches filename
MODULE module_phys_bluews_cbl

   INTEGER :: EntrainmentType, & ! Entrainment type choice
              CO2_included, & ! CO2 included
              InitialData_use, & ! 1 read initial data, 0 do not
              !  qh_choice,&        ! selection of qh use to drive CBL growth 1=Suews 2=lumps 3=obs  ! moved to suews_data
              sondeflag, & ! 1 read sonde or vertical profile data in 0 do not
              isubs ! 1 include subsidence in equations

   INTEGER, DIMENSION(366) :: cblday = 0

   CHARACTER(len=200), DIMENSION(366) :: FileSonde = ""
   CHARACTER(len=200) :: InitialDataFileName
   REAL(KIND(1D0)) :: wsb ! subsidence velocity
   REAL(KIND(1D0)), DIMENSION(1:10) :: cbldata
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: IniCBLdata

   !Parameters in CBL code
   INTEGER :: zmax, &
              nEqn = 6, & !NT changed from 4 to 6
              iCBLcount, &
              nlineInData
   REAL(KIND(1D0)) :: C2K = 273.16

   REAL(KIND(1D0)) :: usbl, ftbl, fqbl, fcbl, gamt, gamq, gamc, tpp, qpp, cp0 !,tk

   REAL(KIND(1D0)) :: alpha3, &
                      blh_m, & ! Boundary layer height(m)
                      blh1_m, &
                      cm, & ! CO2 concentration in CBL
                      !cp0,gamc,& !
                      gamt_Km, & ! Vertical gradient of theta (K/m)
                      gamq_gkgm, & ! Vertical gradient of specific humidity (g/kg/m)
                      gamq_kgkgm, & ! Vertical gradient of specific humidity (kg/kg/m)
                      !fcbl,&
                      tm_C, & ! Potential temperature in CBL (degree Celsius)
                      tm_K, & ! Potential temperature in CBL (K)
                      tmp_K, &
                      tp_C, & ! Potential temperature just above Boundary layer height(degree Celsius)
                      tp_K, & ! Potential temperature just above Boundary layer height(K)
                      tpp_K, &
                      febl_kgkgms, & ! Kinematic latent heat flux((kg/kg)*m/s)
                      fhbl_Kms, & ! Kinematic sensible heat flux(K*m/s)
                      qm_gkg, & ! Specific humidity in CBL(g/kg)
                      qm_kgkg, & ! Specific humidity in CBL(kg/kg)
                      qp_gkg, & ! Specific humidity above Boundary layer height(g/kg)
                      qp_kgkg, & ! Specific humidity above Boundary layer height(kg/kg)
                      qpp_kgkg

   REAL(KIND(1D0)), DIMENSION(0:500, 2) :: gtheta, ghum ! Vertical gradient of theta and specific humidity from sonde data
   REAL(KIND(1D0)), DIMENSION(6) :: y ! NT set from 4 to 6

END MODULE module_phys_bluews_cbl

! Backward compatibility alias (deprecated - will be removed in future version)
! TODO: Remove in version 2026.1.0 (deprecated since 2025.10.0)
MODULE cbl_MODULE
   USE module_phys_bluews_cbl
END MODULE cbl_MODULE
!===================================================================================

MODULE module_phys_bluews
   USE module_phys_bluews_cbl
   USE meteo, ONLY: qsatf, sat_vap_press_x

   IMPLICIT NONE
CONTAINS
   ! Note: INTERVAL is now set to 3600 s in Initial (it is no longer set in RunControl) HCW 29 Jan 2015
   ! Last modified:
   !  NT 6 Apr 2017 - include top of the CBL variables in RKUTTA scheme + add flag to include or exclude subsidence
   !  HCW 29 Mar 2017 - Changed third dimension of dataOutBL to Gridiv (was previously iMB which seems incorrect)
   !  LJ 27 Jan 2016 - Removal of tabs

   SUBROUTINE CBL(iy, id, it, imin, ir, Gridiv, qh_choice, dectime, Temp_C, Press_hPa, avkdn, avu1, avrh, &
                  avcp, avdens, es_hPa, lv_J_kg, nsh_real, tstep, UStar, psih, is, NumberOfGrids, qhforCBL, qeforCBL, &
                  ReadLinesMetdata, dataOutBL)
      IMPLICIT NONE
      INTEGER, PARAMETER :: ncolumnsdataOutBL = 22

      INTEGER, INTENT(IN) :: tstep, is, NumberOfGrids, Gridiv, ReadLinesMetdata, ir
      REAL(KIND(1D0)), INTENT(IN), DIMENSION(NumberOfGrids) :: qhforCBL, qeforCBL
      REAL(KIND(1D0)), INTENT(IN) :: avkdn, nsh_real, UStar, psih
      INTEGER, INTENT(INOUT) :: qh_choice, iy, id, it, imin
      REAL(KIND(1D0)), INTENT(INOUT) :: dectime, Press_hPa, avu1, avrh, es_hPa, avcp, avdens, lv_J_kg
      REAL(KIND(1D0)), INTENT(OUT) :: Temp_C
      REAL(KIND(1D0)), INTENT(OUT), DIMENSION(ReadLinesMetdata, ncolumnsdataOutBL, NumberOfGrids) :: dataOutBL

      REAL(KIND(1D0)) :: gas_ct_dry = 8.31451/0.028965 !j/kg/k=dry_gas/molar
      ! REAL(KIND(1d0))::  gas_ct_wv = 8.31451/0.0180153 !j/kg/kdry_gas/molar_wat_vap
      REAL(KIND(1D0)) :: qh_use, qe_use, tm_K_zm, qm_gkg_zm
      REAL(KIND(1D0)) :: Temp_C1, avrh1, es_hPa1
      REAL(KIND(1D0)) :: secs0, secs1, Lv
      REAL(KIND(1D0)) :: NAN = -999
      INTEGER :: idoy, startflag

      ! initialise startflag
      startflag = 0

      ! print *, 'iCBLcount', iCBLcount,ifirst
      ! print *,iy, id,it,imin
      ! print *, cbldata(1),tm_K

      ! Reset iCBLcount at start of each metblock (HCW added 29/03/2017)
      ! IF (ifirst == 1) THEN
      !    iCBLcount = 0
      ! ENDIF
      !  print*,IniCBLdata
      !write(*,*) DateTIme
      !Skip first loop and unspecified days
      !IF((ifirst==1 .AND. iMB==1) .OR. CBLday(id)==0) THEN   !HCW modified condition to check for first timestep of the model run
      ! IF (ifirst == 1 .OR. IniCBLdata(id, 2) == -999) THEN   !TS modified to adapt the format of the new CBL_initial file
      !    iCBLcount = iCBLcount + 1
      !    !write(*,*) 'ifirst or nonCBLday', DateTime, iCBLcount
      !    dataOutBL(iCBLcount, 1:ncolumnsdataOutBL, Gridiv) = (/REAL(iy, 8), REAL(id, 8), REAL(it, 8), REAL(imin, 8), dectime, &
      !                                                          (NAN, is=6, ncolumnsdataOutBL)/)
      !    RETURN
      ! ELSEIF (avkdn < 5) THEN
      !    iNBL = 1
      !    IF (iNBL == -9) THEN
      !       CALL CBL_initial(qh_use, qe_use, tm_K_zm, qm_gkg_zm, startflag, ir, Gridiv)
      !       RETURN
      !    ELSE
      !       !ADD NBL for Night:(1)Fixed input/output NBL; (2) Input Fixed Theta,Q to SUEWS; (3) Currently NBL eq 200 m
      !       CALL NBL(qh_use, qe_use, tm_K_zm, qm_gkg_zm, startflag, Gridiv)
      !       RETURN
      !    ENDIF
      ! ENDIF

      IF (avkdn < 5) THEN
         ! iNBL = 1
         ! IF (iNBL == -9) THEN
         !    CALL CBL_initial(qh_use, qe_use, tm_K_zm, qm_gkg_zm, startflag, Gridiv)
         !    RETURN
         ! ELSE
         !ADD NBL for Night:(1)Fixed input/output NBL; (2) Input Fixed Theta,Q to SUEWS; (3) Currently NBL eq 200 m
         CALL NBL(iy, id, it, imin, dectime, ir, qh_choice, qh_use, qe_use, tm_K_zm, qm_gkg_zm, startflag, Gridiv, &
                  psih, UStar, Temp_C, &
                  NumberOfGrids, qhforCBL, qeforCBL, &
                  Press_hPa, avu1, avrh, &
                  readLinesMetdata, dataOutBL, &
                  avcp, avdens, es_hPa, lv_J_kg)
         RETURN
         ! ENDIF
      END IF

      IF (startflag == 0) THEN !write down initial values in previous time step
         !write(*,*) 'startflag', DateTime, iCBLcount
         dataOutBL(ir, 1:ncolumnsdataOutBL, Gridiv) &
            = (/REAL(iy, 8), REAL(id, 8), REAL(it, 8), REAL(imin, 8), dectime, blh_m, tm_K, &
                qm_kgkg*1000, tp_K, qp_kgkg*1000, (NAN, is=11, 20), gamt_Km, gamq_kgkgm/)
         startflag = 1
      END IF

      qh_use = qhforCBL(Gridiv) !HCW 21 Mar 2017
      qe_use = qeforCBL(Gridiv)
      IF (qh_use < -900 .OR. qe_use < -900) THEN ! observed data has a problem
         CALL ErrorHint(22, 'Unrealistic qh or qe_value for CBL in CBL.', qh_use, qe_use, qh_choice)
      END IF
      !!Heat flux choices - these are now made in SUEWS_Calculations for qhforCBL and qeCBL, rather than here
      !IF(Qh_choice==1) THEN   !from SUEWS
      !  !qh_use=qh
      !   !qe_use=qeph
      !   qh_use=qhforCBL(Gridiv)   !HCW 21 Mar 2017
      !   qe_use=qeforCBL(Gridiv)
      !ELSEIF(qh_choice==2)THEN !from LUMPS
      !   qh_use=H_mod
      !   qe_use=E_mod
      !ELSEIF(qh_choice==3)THEN  !from OBS
      !   IF(qh_obs<-900.OR.qe_obs<-900)THEN  ! observed data has a problem
      !      CALL ErrorHint(22,'Unrealistic observed qh or qe_value.',qh_obs,qe_obs,qh_choice)
      !   ENDIF
      !   qh_use=qh_obs
      !   qe_use=qe_obs
      !ENDIF

      !-------Main loop of CBL calculation--------------------------------------
      !-------------------------------------------------------------------------

      !write(*,*) 'Main CBL loop'

      cbldata(1) = float(it) + float(imin)/60.
      cbldata(2) = qh_use
      cbldata(3) = qe_use
      cbldata(4) = avdens
      cbldata(5) = lv_J_kg
      cbldata(6) = avcp
      cbldata(7) = avu1
      cbldata(8) = UStar
      cbldata(9) = Press_hPa
      cbldata(10) = psih

      ! print *, 'cbldata'
      ! print *, cbldata(1),tm_K

      secs0 = cbldata(1)*3600.
      secs1 = secs0 + float(tstep) ! time in seconds
      ! Kinematic fluxes
      fhbl_Kms = cbldata(2)/(cbldata(4)*cbldata(6)) !qh_use/(avdens*avcp)      ! units: degK * m/s
      febl_kgkgms = cbldata(3)/(cbldata(4)*cbldata(5)) !qe_use/(avdens*lv_J_kg)   ! units: kg/kg * m/s
      IF (CO2_included == 1) THEN
         fcbl = 0 !fc(i)/(rmco2/volm)      ! units: mol/mol * m/s
      ELSE
         cm = NAN
      END IF

      !   tpp_K=tp_K
      !   qpp_kgkg=qp_kgkg

      IF (sondeflag == 1) THEN
         CALL gamma_sonde
      END IF
      !             set up array for Runge-Kutta call
      blh1_m = blh_m
      y(1) = blh_m ! integrate h, t, q, c from time s(i-1)
      y(2) = tm_K ! to time s(i) using runge-kutta solution
      y(3) = qm_kgkg ! of slab CBL equations
      y(4) = cm
      y(5) = tp_K
      y(6) = qp_kgkg

      !++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL rkutta(neqn, secs0, secs1, y, 1)
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++
      blh_m = y(1)
      tm_K = y(2) ! potential temperature, units: K
      qm_kgkg = y(3) ! specific humidity, units: kg/kg
      cm = y(4) ! co2 concentration,units: mol/mol
      tp_K = y(5) ! potential temperature top of CBL: K
      qp_kgkg = y(6) ! specific humidity top of CBL: kg/kg
      !             compute derived quantities for this time step

      !NT: now included in rkutta
      !   tp_K   = tpp_K     + (gamt_Km*(blh_m-blh1_m))
      !   qp_kgkg = qpp_kgkg + (gamq_kgkgm*(blh_m-blh1_m))

      !   IF (tp_K.LT.tm_K) THEN
      !      tp_K = tm_K
      !   ENDIF

      !                th = tm_K - (grav/cbldata(5))*blh_m                         ! actual temp just below z=h
      !                dh = qsatf(th,cbldata(8)) - qm_kgkg           ! deficit just below z=h

      tp_C = tp_K - C2K
      tm_C = tm_K - C2K

      !         delt = tp_K - tm_K ! temp
      !         delq = qp_kgkg - qm_kgkg ! humidity
      !deltv = (tp_K - tm_K) + 0.61*tm_k*(qp_kgkg - qm_kgkg)  ! pot virtual temp

      qm_gkg = qm_kgkg*1000 !humidities: kg/kg -> g/kg

      !Output time correction
      idoy = id
      !If(it==0 .and. imin==55) idoy=id-1
      IF (it == 0 .AND. imin == (nsh_real - 1)/nsh_real*60) idoy = id - 1 !Modified by HCW 04 Mar 2015 in case model timestep is not 5-min

      ! QUESTION: any difference between the two options? code looks the same in the two branches.
      IF ((qh_choice == 1) .OR. (qh_choice == 2)) THEN !BLUEWS or BLUMPS
         !Stability correction
         !tm_K_zm=tm_K+cbldata(10)*cbldata(2)/(k*cbldata(8)*cbldata(6)*cbldata(4))
         Temp_C = tm_K/((1000/cbldata(9))**(gas_ct_dry/cbldata(6))) - C2K
         es_hPa = sat_vap_press_x(Temp_C, cbldata(9), 1, dectime)
         lv = (2500.25 - 2.365*Temp_C)*1000
         !qm_gkg_zm=qm_gkg+cbldata(10)*cbldata(3)/(k*cbldata(8)*cbldata(4)*lv)
         avrh = 100*((qm_gkg*cbldata(9)/(622 + qm_gkg))/es_hPa) !check pressure
         IF (avrh > 100) THEN
            CALL errorHint(34, 'subroutine CBL dectime, relative humidity', idoy + cbldata(1)/24.0, avrh, 100)
            avrh = 100
         END IF
         ! iCBLcount = iCBLcount + 1
         ! write(*,*) 'qh1or2', iy,id,it,imin, iCBLcount
         dataOutBL(ir, 1:ncolumnsdataOutBL, Gridiv) &
            = (/REAL(iy, 8), REAL(id, 8), REAL(it, 8), REAL(imin, 8), dectime, blh_m, tm_K, &
                qm_kgkg*1000, tp_K, qp_kgkg*1000, &
                Temp_C, avrh, cbldata([2, 3, 9, 7, 8, 4, 5, 6]), &
                gamt_Km, gamq_kgkgm/)
      ELSEIF (qh_choice == 3) THEN ! CBL
         !tm_K_zm=tm_K+cbldata(10)*cbldata(2)/(k*cbldata(8)*cbldata(6)*cbldata(4))
         Temp_C1 = tm_K/((1000/cbldata(9))**(gas_ct_dry/cbldata(6))) - C2K
         es_hPa1 = sat_vap_press_x(Temp_C1, cbldata(9), 1, dectime)
         lv = (2500.25 - 2.365*Temp_C1)*1000
         !qm_gkg_zm=qm_gkg+cbldata(10)*cbldata(3)/(k*cbldata(8)*cbldata(4)*lv)
         avrh1 = 100*((qm_gkg*cbldata(9)/(622 + qm_gkg))/es_hPa1) ! should be cbldata(9), i.e., Press_hPa
         IF (avrh1 > 100) THEN
            CALL errorHint(34, 'subroutine CBL dectime, relative humidity', idoy + cbldata(1)/24.0, avrh1, 100)
            avrh1 = 100
         END IF
         ! iCBLcount = iCBLcount + 1
         !write(*,*) 'qh3', DateTIme, iCBLcount
         dataOutBL(ir, 1:ncolumnsdataOutBL, Gridiv) &
            = (/REAL(iy, 8), REAL(id, 8), REAL(it, 8), REAL(imin, 8), dectime, blh_m, tm_K, &
                qm_kgkg*1000, tp_K, qp_kgkg*1000, &
                Temp_C1, avrh1, cbldata([2, 3, 9, 7, 8, 4, 5, 6]), &
                gamt_Km, gamq_kgkgm/)
      END IF
      ! move the counter at the end, TS 27 Aug 2019
      ! iCBLcount = iCBLcount + 1

      RETURN

   END SUBROUTINE CBL

   !-----------------------------------------------------------------------
   !-----------------------------------------------------------------------
   SUBROUTINE CBL_ReadInputData(FileInputPath, qh_choice)

      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: qh_choice
      CHARACTER(len=150), INTENT(IN) :: FileInputPath

      INTEGER :: i, ios
      REAL(KIND(1D0)) :: l

      NAMELIST /CBLInput/ EntrainmentType, &
         QH_choice, &
         isubs, &
         CO2_included, &
         cblday, &
         wsb, &
         InitialData_use, &
         InitialDataFileName, &
         sondeflag, &
         FileSonde

      OPEN (51, file=TRIM(FileInputPath)//'CBLInput.nml', status='old', err=24)
      READ (51, nml=CBLInput, err=24)
      CLOSE (51)

      !Read initial values if it's needed
      IF (InitialData_use == 1 .OR. InitialData_use == 2) THEN
         OPEN (52, file=TRIM(FileInputPath)//TRIM(InitialDataFileName), status='old', err=25)
         READ (52, *)
         nlineInData = 0 !Initialise nlines
         DO
            READ (52, *, iostat=ios) l
            IF (ios < 0 .OR. l == -9) EXIT !IF (l == -9) EXIT
            nlineInData = nlineInData + 1
         END DO
         CLOSE (52)

         IF (ALLOCATED(IniCBLdata)) DEALLOCATE (IniCBLdata)
         ALLOCATE (IniCBLdata(1:nlineInData, 1:8))
         OPEN (52, file=TRIM(FileInputPath)//TRIM(InitialDataFileName), status='old', err=25)
         READ (52, *)
         DO i = 1, nlineInData
            READ (52, *) IniCBLdata(i, 1:8)
         END DO
         CLOSE (52)
      END IF

      IF (CO2_included == 0) THEN
         fcbl = 0 ! hard-wire no CO2
      END IF

      ! iCBLcount = 1

      RETURN

24    CALL ErrorHint(24, 'CBLInput.nml', 0.00D0, 0.000D0, 0)
25    CALL ErrorHint(24, TRIM(FileInputPath)//TRIM(InitialDataFileName), 0.00D0, 0.00D0, 0)

   END SUBROUTINE CBL_ReadInputData

   !----------------------------------------------------------------------
   !-----------------------------------------------------------------------
   SUBROUTINE CBL_initial(qh_use, qe_use, tm_K_zm, qm_gkg_zm, startflag, ir, Gridiv)

      USE mod_z
      USE module_phys_atmmoiststab, ONLY: k
      USE gas
      USE time
      USE data_in
      USE sues_data
      USE moist
      USE allocateArray
      USE defaultNotUsed
      USE cbl_module
      USE gis_data
      USE WhereWhen
      USE meteo, ONLY: sat_vap_press_x

      IMPLICIT NONE

      REAL(KIND(1D0)) :: qh_use, qe_use, tm_K_zm, qm_gkg_zm
      REAL(KIND(1D0)) :: lv
      INTEGER :: i, nLineDay, ir, Gridiv, startflag

      qh_use = qhforCBL(Gridiv) !HCW 21 Mar 2017
      qe_use = qeforCBL(Gridiv)
      IF (qh_use < -900 .OR. qe_use < -900) THEN ! observed data has a problem
         CALL ErrorHint(22, 'Unrealistic qh or qe_value for CBL in CBL_initial.', qh_use, qe_use, qh_choice)
      END IF
      !!Heat flux choices - these are now made in SUEWS_Calculations for qhforCBL and qeCBL, rather than here
      !IF(Qh_choice==1) THEN   !from SUEWS
      !   !qh_use=qh
      !   !qe_use=qeph
      !   qh_use=qhforCBL(Gridiv)   !HCW 21 Mar 2017
      !   qe_use=qeforCBL(Gridiv)
      !ELSEIF(qh_choice==2)THEN !from LUMPS
      !   qh_use=H_mod
      !   qe_use=E_mod
      !ELSEIF(qh_choice==3)THEN  !from OBS
      !   IF(qh_obs<-900.OR.qe_obs<-900)THEN  ! observed data has a problem
      !      CALL ErrorHint(22,'Unrealistic observed qh or qe_value.',qh_obs,qe_obs,qh_choice)
      !   ENDIF
      !   qh_use=qh_obs
      !   qe_use=qe_obs
      !ENDIF

      blh_m = NAN
      ! iCBLcount = iCBLcount + 1
      ! write(*,*) 'cblinitial', DateTIme, iCBLcount
      dataOutBL(ir, 1:ncolumnsdataOutBL, Gridiv) = (/REAL(iy, 8), REAL(id, 8), REAL(it, 8), REAL(imin, 8), dectime, &
                                                     (NAN, is=6, ncolumnsdataOutBL)/)

      nLineDay = 0
      DO i = 1, nlineInData
         IF (INT(IniCBLdata(i, 1)) <= id) THEN
            nLineDay = nLineDay + 1
         END IF
      END DO

      IF (InitialData_use == 2) THEN
         blh_m = IniCBLdata(nLineDay, 2)
         gamt_Km = IniCBLdata(nLineDay, 3)
         gamq_gkgm = IniCBLdata(nLineDay, 4)
         tp_K = IniCBLdata(nLineDay, 5)
         qp_gkg = IniCBLdata(nLineDay, 6)
         tm_K = IniCBLdata(nLineDay, 7)
         qm_gkg = IniCBLdata(nLineDay, 8)
      ELSEIF (InitialData_use == 1 .AND. IniCBLdata(nlineDay, 1) == id) THEN ! Changed from i to nlineDay, HCW 29 March 2017
         blh_m = IniCBLdata(nLineDay, 2)
         gamt_Km = IniCBLdata(nLineDay, 3)
         gamq_gkgm = IniCBLdata(nLineDay, 4)
         tm_K_zm = (Temp_C + C2K)*((1000/Press_hPa)**(gas_ct_dry/avcp))
         tm_K = tm_K_zm - psih*qh_use/(k*UStar*avcp*avdens)
         es_hPa = sat_vap_press_x(Temp_C, Press_hPa, 1, dectime)
         qm_gkg_zm = 622*avrh/(100*Press_hPa/es_hPa - avrh)
         lv = (2500.25 - 2.365*temp_C)*1000
         qm_gkg = qm_gkg_zm - psih*qe_use/(k*UStar*avdens*lv)
         tp_K = tm_K
         qp_gkg = qm_gkg
      ELSEIF (InitialData_use == 0) THEN
         blh_m = 241.5
         gamt_Km = 0.043
         gamq_gkgm = 0.0092
         tm_K_zm = (Temp_C + C2K)*((1000/Press_hPa)**(gas_ct_dry/avcp))
         tm_K = tm_K_zm - psih*qh_use/(k*UStar*avcp*avdens)
         es_hPa = sat_vap_press_x(Temp_C, Press_hPa, 1, dectime)
         qm_gkg_zm = 622*avrh/(100*Press_hPa/es_hPa - avrh)
         lv = (2500.25 - 2.365*temp_C)*1000
         qm_gkg = es_hPa - psih*qe_use/(k*UStar*avdens*lv)
         tp_K = tm_K
         qp_gkg = qm_gkg
      END IF

      gamq_kgkgm = gamq_gkgm/1000.
      qp_kgkg = qp_gkg/1000 !humidities: g/kg -> kg/kg   q+
      qm_kgkg = qm_gkg/1000 !conc at mixing layer height h
      tp_C = tp_K - C2K
      tm_C = tm_K - C2K

      ! IF(sondeflag==1 .AND. cblday(id)==1) THEN
      IF (sondeflag == 1 .AND. IniCBLdata(id, 2) /= -999) THEN
         !if gamma theta varies with z (selected by setting gthetaflag=1)
         !if gamma q varies with z (selected by setting ghumflag=1)
         CALL sonde(id)
         gamt_Km = 0
         gamq_kgkgm = 0
      END IF

      !adjusting qp and pm in case of saturation
      IF (qp_kgkg > qsatf(tp_C, Press_hPa) .OR. qp_kgkg < 0) THEN
         qp_kgkg = qsatf(tp_C, Press_hPa)
      END IF
      IF (qm_kgkg > qsatf(tm_C, Press_hPa) .OR. qm_kgkg < 0) THEN
         qm_kgkg = qsatf(tm_C, Press_hPa)
      END IF

      !    if((CBLuse==2).and.(zenith_deg>=90))then
      !    blh_m=188
      !    endif
      startflag = 0

   END SUBROUTINE CBL_initial

   SUBROUTINE NBL(iy, id, it, imin, dectime, ir, qh_choice, qh_use, qe_use, &
                  tm_K_zm, qm_gkg_zm, startflag, Gridiv, &
                  psih, UStar, Temp_C, NumberOfGrids, qhforCBL, qeforCBL, Press_hPa, avu1, avrh, &
                  ReadLinesMetdata, dataOutBL, &
                  avcp, avdens, es_hPa, lv_J_kg)

      IMPLICIT NONE
      INTEGER, PARAMETER :: ncolumnsdataOutBL = 22

      INTEGER, INTENT(IN) :: qh_choice, iy, id, it, imin, NumberOfGrids, ReadLinesMetdata, ir
      REAL(KIND(1D0)), INTENT(IN) :: Press_hPa, psih, UStar
      REAL(KIND(1D0)), INTENT(OUT) :: Temp_C, tm_K_zm, qm_gkg_zm
      REAL(KIND(1D0)), INTENT(INOUT) :: dectime, avu1, avRH, avcp, avdens, es_hPa, lv_J_kg
      REAL(KIND(1D0)), INTENT(IN), DIMENSION(NumberOfGrids) :: qhforCBL, qeforCBL
      REAL(KIND(1D0)), INTENT(OUT), DIMENSION(ReadLinesMetdata, ncolumnsdataOutBL, NumberOfGrids) :: dataOutBL

      REAL(KIND(1D0)) :: &
         k = 0.4, & !Von Karman's contant
         gas_ct_dry = 8.31451/0.028965 !j/kg/k=dry_gas/molar
      REAL(KIND(1D0)) :: qh_use, qe_use
      REAL(KIND(1D0)) :: lv
      INTEGER :: i, nLineDay, Gridiv, startflag

      qh_use = qhforCBL(Gridiv) !HCW 21 Mar 2017
      qe_use = qeforCBL(Gridiv)
      IF (qh_use < -900 .OR. qe_use < -900) THEN ! observed data has a problem
         CALL ErrorHint(22, 'Unrealistic qh or qe value for CBL in NBL.', qh_use, qe_use, qh_choice)
      END IF

      nLineDay = 0
      DO i = 1, nlineInData
         IF (INT(IniCBLdata(i, 1)) <= id) THEN
            nLineDay = nLineDay + 1
         END IF
      END DO

      !Assume Theta and Q in the night constantly Equal to the ones in the morning for CBL to run
      ! seems incorrect initialisation
      ! tm_K=IniCBLdata(nLineDay,7)*1000
      ! qm_gkg=IniCBLdata(nLineDay,8)*1000

      ! corrected to the following, TS 20170609:
      tm_K = IniCBLdata(nLineDay, 7)
      qm_gkg = IniCBLdata(nLineDay, 8)

      !NBL currently fixed to 200 m
      blh_m = 200

      ! also fill in other variables
      gamt_Km = IniCBLdata(nLineDay, 3)
      gamq_gkgm = IniCBLdata(nLineDay, 4)
      tp_K = IniCBLdata(nLineDay, 5)
      qp_gkg = IniCBLdata(nLineDay, 6)
      tm_K = IniCBLdata(nLineDay, 7)
      qm_gkg = IniCBLdata(nLineDay, 8)

      ! dataOutBL(iCBLcount,1:ncolumnsdataOutBL,Gridiv)=(/REAL(iy,8),REAL(id,8),REAL(it,8),REAL(imin,8),&
      ! dectime,blh_m,tm_K,qm_gkg,&
      ! (NAN,is=9,ncolumnsdataOutBL)/)

      Temp_C = tm_K/((1000/Press_hPa)**(gas_ct_dry/avcp)) - C2K
      es_hPa = sat_vap_press_x(Temp_C, Press_hPa, 1, dectime)
      lv = (2500.25 - 2.365*Temp_C)*1000
      !qm_gkg_zm=qm_gkg+cbldata(10)*cbldata(3)/(k*cbldata(8)*cbldata(4)*lv)
      avrh = 100*((qm_gkg*Press_hPa/(622 + qm_gkg))/es_hPa) !check pressure
      IF (avrh > 100) THEN
         CALL errorHint(34, 'subroutine CBL dectime, relative humidity', dectime, avrh, 100)
         avrh = 100
      END IF

      ! print *, 'iCBLcount in NBL',iCBLcount
      ! print *,iy, id,it,imin

      dataOutBL(ir, 1:ncolumnsdataOutBL, Gridiv) = &
         (/REAL(iy, 8), REAL(id, 8), REAL(it, 8), REAL(imin, 8), dectime, &
           blh_m, tm_K, qm_gkg, &
           tp_K, qp_gkg, &
           Temp_C, avrh, qh_use, qe_use, Press_hPa, avu1, UStar, avdens, lv_J_kg, avcp, &
           gamt_Km, gamq_kgkgm/)

      IF (InitialData_use == 2) THEN
         blh_m = IniCBLdata(nLineDay, 2)
         gamt_Km = IniCBLdata(nLineDay, 3)
         gamq_gkgm = IniCBLdata(nLineDay, 4)
         tp_K = IniCBLdata(nLineDay, 5)
         qp_gkg = IniCBLdata(nLineDay, 6)
         tm_K = IniCBLdata(nLineDay, 7)
         qm_gkg = IniCBLdata(nLineDay, 8)
      ELSEIF (InitialData_use == 1 .AND. IniCBLdata(nlineDay, 1) == id) THEN ! Changed from i to nlineDay, HCW 29 March 2017
         blh_m = IniCBLdata(nLineDay, 2)
         gamt_Km = IniCBLdata(nLineDay, 3)
         gamq_gkgm = IniCBLdata(nLineDay, 4)
         tm_K_zm = (Temp_C + C2K)*((1000/Press_hPa)**(gas_ct_dry/avcp))
         tm_K = tm_K_zm - psih*qh_use/(k*UStar*avcp*avdens)
         es_hPa = sat_vap_press_x(Temp_C, Press_hPa, 1, dectime)
         qm_gkg_zm = 622*avrh/(100*Press_hPa/es_hPa - avrh)
         lv = (2500.25 - 2.365*temp_C)*1000
         qm_gkg = qm_gkg_zm - psih*qe_use/(k*UStar*avdens*lv)
         tp_K = tm_K
         qp_gkg = qm_gkg
      ELSEIF (InitialData_use == 0) THEN
         blh_m = 241.5
         gamt_Km = 0.043
         gamq_gkgm = 0.0092
         tm_K_zm = (Temp_C + C2K)*((1000/Press_hPa)**(gas_ct_dry/avcp))
         tm_K = tm_K_zm - psih*qh_use/(k*UStar*avcp*avdens)
         es_hPa = sat_vap_press_x(Temp_C, Press_hPa, 1, dectime)
         qm_gkg_zm = 622*avrh/(100*Press_hPa/es_hPa - avrh)
         lv = (2500.25 - 2.365*temp_C)*1000
         qm_gkg = es_hPa - psih*qe_use/(k*UStar*avdens*lv)
         tp_K = tm_K
         qp_gkg = qm_gkg
      END IF

      gamq_kgkgm = gamq_gkgm/1000.
      qp_kgkg = qp_gkg/1000 !humidities: g/kg -> kg/kg   q+
      qm_kgkg = qm_gkg/1000 !conc at mixing layer height h
      tp_C = tp_K - C2K
      tm_C = tm_K - C2K

      ! IF(sondeflag==1 .AND. cblday(id)==1) THEN
      IF (sondeflag == 1 .AND. IniCBLdata(id, 2) /= -999) THEN
         !if gamma theta varies with z (selected by setting gthetaflag=1)
         !if gamma q varies with z (selected by setting ghumflag=1)
         CALL sonde(id)
         gamt_Km = 0
         gamq_kgkgm = 0
      END IF

      !adjusting qp and pm in case of saturation
      IF (qp_kgkg > qsatf(tp_C, Press_hPa) .OR. qp_kgkg < 0) THEN
         qp_kgkg = qsatf(tp_C, Press_hPa)
      END IF
      IF (qm_kgkg > qsatf(tm_C, Press_hPa) .OR. qm_kgkg < 0) THEN
         qm_kgkg = qsatf(tm_C, Press_hPa)
      END IF

      startflag = 0

      ! iCBLcount = iCBLcount + 1 ! move counter at the end, TS 27 Aug 2019
   END SUBROUTINE NBL

   !------------------------------------------------------------------------

   !-----------------------------------------------------------------------
   ! from CBL modelling Cleugh and Grimmond (2000) BLM
   ! NT 6 Apr 2017: include iteration over top of CBL scalars and include subsidence flag
   ! Last modified: LJ 27 Jan 2016 - Removal of tabs
   !-----------------------------------------------------------------------
   SUBROUTINE RKUTTA(neqn_use, XA, XB, y_use, NSTEPS)
      !       XA=s0
      !       XB=s1
      !       Y(1)=blh_m
      !       Y(2)=tm_K
      !       Y(3)=qm_kgkg
      !       Y(4)=cm
      !       Y(5)=tp_K
      !       Y(6)=qp_kgkg
      !       JOHN KNIGHT, 1985 (AMENDED BY MRR, 23-SEP-85)
      !       EXPLICIT FOURTH-ORDER RUNGE-KUTTA METHOD FOR FIRST-ORDER ODE SYSTEM
      !       OF NE EQUATIONS, WITH INITIAL VALUES SUPPLIED.
      !       MEANING OF PARAMETERS:
      !       NE     = NUMBER OF EQUATIONS (MAX 21)
      !       XA     = VALUE OF INDEPENDENT VARIABLE ON ENTRY
      !       XB     = VALUE OF INDEPENDENT VARIABLE AT END OF INTERVAL
      !       Y(NE)  = ON ENTRY: INITIAL VALUES OF DEPENDENT VARIABLES AT XA
      !       ON EXIT:  CALCULATED VALUES OF DEPENDENT VARIABLES AT XB
      !       NSTEPS = NUMBER OF INTEGRATION STEPS OVER INTERVAL (XA,XB)
      !       DIFF  = NAME OF USER-SUPPLIED SUBROUTINE TO CALCULATE DERIVATIVES
      !       DYDX (DIFF MUST BE DECLARED EXTERNAL IN CALLING PROGRAM).
      !       PARAMETERS IN SUBROUTINE DIFF(NE,X,Y,DYDX):
      !       NEqn = NUMBER OF EQUATIONS
      !       X = INDEPENDENT VARIABLE
      !       Y = ARRAY (LENGTH NE) OF VALUES OF DEPENDENT VARIABLES
      !       DYDX = ON EXIT, ARRAY (LENGTH NE) OF VALUES OF DERIVATIVES
      !        IMPLICIT real*8 (A-H,O-Z)
      IMPLICIT NONE
      INTEGER :: ns, nsteps, nj, n, neqn_use
      REAL(KIND(1D0)), DIMENSION(neqn_use) :: y_use
      REAL(KIND(1D0)), DIMENSION(21) :: dydx, arg
      REAL(KIND(1D0)), DIMENSION(21, 5) :: rk
      REAL(KIND(1D0)), DIMENSION(4) :: coef
      REAL(KIND(1D0)) :: XA, XB, step, X, xx

      coef(1) = 1.0
      coef(2) = 0.5
      coef(3) = 0.5
      coef(4) = 1.0
      !        print*,"rk1: ",xa,xb,y
      STEP = (XB - XA)/NSTEPS

      DO NS = 1, NSTEPS
         DO NJ = 1, neqn_use
            RK(NJ, 1) = 0
         END DO
         X = XA + (NS - 1)*STEP
         DO N = 1, 4
            IF (N == 1) THEN
               XX = X
            ELSEIF (N > 1) THEN
               XX = X + COEF(N)*STEP
            END IF

            DO NJ = 1, neqn_use
               ARG(NJ) = y_use(NJ) + COEF(N)*RK(NJ, N)
            END DO

            CALL DIFF(xx, ARG, DYDX)

            DO NJ = 1, neqn_use
               RK(NJ, N + 1) = STEP*DYDX(NJ)
            END DO
         END DO

         DO NJ = 1, neqn_use
            DO N = 1, 4
               y_use(NJ) = y_use(NJ) + RK(NJ, N + 1)/(6*COEF(N))
            END DO
         END DO
      END DO

      RETURN
   END SUBROUTINE RKUTTA
   !---------------------------------------------------------------------
   !---------------------------------------------------------------------

   SUBROUTINE diff(s, y1, dyds)
      ! in y1,neqn
      ! out dyds

      !       calculates derivatives for cbl slab model
      !       y(1) = h = cbl depth(m)
      !       y(2) = t = potential temp(K)
      !       y(3) = q = specific humidity(kg/kg)
      !       y(4) = c = CO2 concentration
      ! USE data_in
      ! USE sues_data
      !    use allocateArray
      USE time
      USE CBL_MODULE
      USE defaultnotUsed
      USE mod_grav

      IMPLICIT NONE
      REAL(KIND(1D0)), DIMENSION(neqn) :: dyds, y1
      REAL(KIND(1D0)) :: zero = 0.0
      REAL(KIND(1D0)) :: h1, t_K, q_kgkg, c, cp, ws, s, foo
      !     real(kind(1D0)) :: tp_K,qp_kgkg
      REAL(KIND(1D0)) :: delt_K, delq_kgkg, delc
      REAL(KIND(1D0)) :: gamtv_Km, deltv_K, ftv_Kms
      REAL(KIND(1D0)) :: ftva_Kms, delb, qs2, qs3
      REAL(KIND(1D0)) :: dhds, dtds, dqds, dcds, dtpds, dqpds
      REAL(KIND(1D0)) :: conk, conn, cona, conc, cont

      !    print*,"diff: timestamp:",s
      foo = s
      !    pause
      h1 = y1(1) !m
      t_K = y1(2) !K
      q_kgkg = y1(3) !kg/kg
      c = y1(4)
      tp_K = y1(5) !K
      qp_kgkg = y1(6) !kg/kg

      !       find t, q, c above inversion, and jumps across inversion
      !       tp = tp + gamt*h
      !       qp = qp0 + gamq*h

      cp = 0 ! cp0 + gamc* h1   ! todo

      delt_K = tp_K - t_K
      delq_kgkg = qp_kgkg - q_kgkg
      delc = cp - c

      !       find potential virtual temperature flux, gradient and jump
      ftv_Kms = fhbl_Kms + 0.61*tm_K*febl_kgkgms
      gamtv_Km = gamt_Km + 0.61*tm_K*gamq_kgkgm !/1000
      deltv_K = delt_K + 0.61*tm_K*delq_kgkg

      !       find velocity scale ws
      ftva_Kms = MAX(ftv_Kms, zero) ! virtual heat flux
      ws = (h1*ftva_Kms*grav/tm_K)**0.3333333333

      !       find dhds using one of 4 alternative schemes chosen by ient:
      IF (EntrainmentType == 2) THEN
         !       EntrainmentType=1: encroachment (as in McN and S 1986 eq 16))
         dhds = ftva_Kms/(h1*gamtv_Km)

      ELSE IF (EntrainmentType == 1) THEN
         !       EntrainmentType=2: Driedonks 1981 (as in McN and S 1986 eq 13)
         IF (deltv_K <= 0.01) THEN
            dhds = ftva_Kms/(h1*gamtv_Km)
            CALL errorHint(30, "subroutine diff [CBL: Deltv_K<0.01 EntrainmentType=1], deltv_K,delt_K,", deltv_K, delt_K, notUsedI)
            CALL errorHint(30, "subroutine diff [CBL: Deltv_K<0.01 EntrainmentType=1], tm_K,TPP_K,y1", tm_K, TPP_K, notUsedI)
            ! call errorHint(31,"subroutine diff [CBL: Deltv_K<0.01 EntrainmentType=1], y1",real(y1(1),kind(1d0)),notUsed,notUsedI)
         ELSE
            delb = grav*deltv_K/tm_K
            conc = 0.2
            cona = 5.0
            dhds = (conc*ws**3 + cona*cbldata(8)**3)/(h1*delb)
         END IF

      ELSE IF (EntrainmentType == 4) THEN
         !       EntrainmentType=3: Tennekes 1973 (as in R 1991 eqs 3,4)
         alpha3 = 0.2 ! alpha changed back to original Tennekes 1973 value
         IF (deltv_K <= 0.01) THEN
            dhds = ftva_Kms/(h1*gamtv_Km)
            CALL ErrorHint(31, 'subroutine difflfnout: [CBL: deltv_K<0.01 EntrainmentType=4],deltv_K', &
                           deltv_K, notUsed, notUsedI)
         ELSE
            ! include the option whether or not to include subsidence
            IF (isubs == 1) THEN
               dhds = alpha3*ftva_Kms/deltv_k + wsb
            ELSE
               dhds = alpha3*ftva_Kms/deltv_K
            END IF
         END IF

         !       write (4,*) tpp, gamq, dhds, deltv

      ELSE IF (EntrainmentType == 3) THEN
         !       EntrainmentType=4: Rayner and Watson 1991 eq 21
         conn = 1.33
         conk = 0.18
         cont = 0.80
         qs3 = ws**3 + (conn*cbldata(8))**3
         qs2 = qs3**(0.6666666667)

         IF (deltv_K <= 0.01) THEN
            dhds = ftva_Kms/(h1*gamtv_Km)
            CALL ErrorHint(31, 'subroutine difflfnout: [CBL: deltv_K<0.01 EntrainmentType=3],deltv_K', &
                           deltv_K, notUsed, notUsedI)

         ELSE
            delb = grav*deltv_K/tm_K
            dhds = (conk*qs3)/(cont*qs2 + h1*delb)
         END IF

      ELSE
         CALL ErrorHint(24, 'BLUEWS_DIff- CBL- illegal alpha', notUsed, notUsed, notUsedI)
      END IF
      ! find dtds, dqds, dc/ds:
      !        wsb is the subsidence velocity. Try using: -0.01, -0.05, -0.1.
      IF (isubs == 1) THEN
         dtds = fhbl_Kms/h1 + delt_K*(dhds - wsb)/h1
         dqds = febl_kgkgms/h1 + delq_kgkg*(dhds - wsb)/h1
         dcds = fcbl/h1 + delc*(dhds - wsb)/h1
         ! also iterate the top of CBL scalars
         dtpds = gamt_Km*(dhds - wsb)
         dqpds = gamq_kgkgm*(dhds - wsb)
      ELSE
         dtds = fhbl_Kms/h1 + delt_K*(dhds)/h1
         dqds = febl_kgkgms/h1 + delq_kgkg*(dhds)/h1
         dcds = fcbl/h1 + delc*(dhds)/h1
         ! also iterate the top of CBL scalars
         dtpds = gamt_Km*(dhds)
         dqpds = gamq_kgkgm*(dhds)
      END IF

      dyds(1) = dhds
      dyds(2) = dtds
      dyds(3) = dqds
      dyds(4) = dcds
      dyds(5) = dtpds
      dyds(6) = dqpds

      RETURN
   END SUBROUTINE diff

   !--------------------------------------------------------------------------
   !--------------------------------------------------------------------------
   SUBROUTINE sonde(id)
      ! read sonde or vertical profile data - when available
      !use allocateArray
      USE data_in
      USE cbl_module
      IMPLICIT NONE
      INTEGER :: i, fn = 101, izm = 500, notUsedI = -9999, id
      CHARACTER(len=200) :: FileN
      REAL(KIND(1D0)) :: dxx
      REAL(KIND(1D0)), PARAMETER :: notUsed = -9999.99

      FileN = TRIM(FileInputPath)//TRIM(FileSonde(id))
      OPEN (fn, file=FileN, status="old", err=24)
      ! todo gneric skip header
      READ (fn, *)
      READ (fn, *)
      READ (fn, *)

      DO i = 1, 1000
         READ (fn, *, END=900, err=25) gtheta(i, 1), dxx, gtheta(i, 2), ghum(i, 1), dxx, ghum(i, 2)
         ghum(i, 2) = ghum(i, 2)
      END DO
900   zmax = i - 1
      IF (zmax > izm) THEN
         CALL ErrorHint(23, FileN, REAL(zmax, KIND(1D0)), notUsed, izm)
      END IF
      CLOSE (fn)
      RETURN
24    CALL ErrorHint(24, FileN, notUsed, notUsed, notUsedI)
25    CALL ErrorHint(25, FileN, notUsed, notUsed, i)
      RETURN
   END SUBROUTINE sonde
   !------------------------------------------------------------------------------
   !------------------------------------------------------------------------------
   SUBROUTINE gamma_sonde
      USE cbl_module
      !use allocateArray

      IMPLICIT NONE
      REAL(KIND(1D0)) :: gamtt, gamqq
      INTEGER :: j
      ! gtheta(i,1),dxx,gtheta(i,2),ghum(i,1),dxx,ghum(i,2)
      !search for correct gamma theta, depends on h(i-1),
      !               ie current value for mixed layer depth
      IF (sondeflag == 1) THEN
         DO j = 2, zmax
            IF (blh_m >= gtheta(j - 1, 1)) THEN
               gamtt = gtheta(j - 1, 2)
            END IF
            gamt_Km = gamtt
         END DO

         DO j = 2, zmax
            IF (blh_m >= ghum(j - 1, 1)) THEN
               gamqq = ghum(j - 1, 2)
            END IF
            gamq_kgkgm = gamqq/1000.
         END DO
      END IF
      RETURN

   END SUBROUTINE gamma_sonde

END MODULE module_phys_bluews

! Backward compatibility alias (deprecated - will be removed in future version)
! TODO: Remove in version 2026.1.0 (deprecated since 2025.10.0)
MODULE BLUEWS_module
   USE module_phys_bluews
END MODULE BLUEWS_module
