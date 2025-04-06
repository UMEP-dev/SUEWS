 subroutine SurfaceResistance(id,it)
! Last modified by HCW 18 Jun 2015
! Alternative gs calculation added using different functional forms and new coefficients
! Error hints added
! Last modified by LJ in 24 April 2013
! Added impact of snow fraction in LAI and in soil moisture deficit
! HCW 31/07/2014 Modified condition on g6 part to select meas/mod smd
! HCW 01/03/2016 SM dependence is now on modelled smd for vegetated surfaces only (vsmd) (Note:  obs smd still not operational!)

  use allocateArray
  use data_in
  use gis_data
  use moist
  use resist
  use sues_data
  use time, only: iy, imin

  implicit none

  integer::id,it,iv
  real(kind(1d0))::id_real

  !gsChoice = 1 - original Jarvis 76 and Jarvi 2011 method
  !gsChoice = 2 - new method

  id_real = real(id) !Day of year in real number

  if(gsChoice == 1) then
    ! ResistSurf - Surface resistance --------------------------------------------
    ! At nighttime gsc set at arbitrary low value: gsc=0.1 mm/s (Shuttleworth, 1988b)
    if(avkdn<=0)then  !Is this limit good here? ZZ
       gsc=0.1
    else
        QNM=Kmax/(Kmax+G2)
       !gq=(qn1/(g2+qn1))/qnm !With net all-wave radiation

       gq=(avkdn/(G2+avkdn))/QNM !With Kdown

       !Specific humidity deficit
       if(dq<G4) then
          gdq=1-G3*dq
       else
          gdq=1-G3*G4
       endif

       TC=(TH-G5)/(G5-TL)
       TC2=(G5-TL)*(TH-G5)**TC

       !If air temperature below TL or above TH,
       !fit it to TL+0.1/TH-0.1
       if (Temp_C<=tl) then
          gtemp=(tl+0.1-tl)*(th-(tl+0.1))**tc/tc2

          !Call error only if no snow on ground
          if (min(snowFrac(1),snowFrac(2),snowFrac(3),snowFrac(4),snowFrac(5),snowFrac(6))/=1) then
             call errorHint(29,'subroutine SurfaceResistance:T changed to fit limits TL=0.1,Temp_c,id,it',&
                  real(Temp_c,kind(1d0)),id_real,it)
          endif

       elseif (Temp_C>=th) then
          gtemp=((th-0.1)-tl)*(th-(th-0.1))**tc/tc2
          call errorHint(29,'subroutine SurfaceResistance:T changed to fit limits TH=39.9,Temp_c,id,it',&
               real(Temp_c,kind(1d0)),id_real,it)
       else
          gtemp=(Temp_C-tl)*(th-Temp_C)**tc/tc2  ! temp
       endif

    !   soil mosture deficit
    !   write(*,*)'smd:',lai(id,2),smd,xsmd

        sdp=S1/G6+S2

        if(smd_choice>0)then         !Modified from ==1 to > 0 by HCW 31/07/2014
            gs=1-exp(g6*(xsmd-sdp))  !Measured soil moisture deficit is used
        else
            gs=1-exp(g6*(vsmd-sdp))   !Modelled is used
        endif

        gs = gs*(1-sum(snowFrac(1:6))/6)

        if(gs<0)then
            call errorHint(65,'SUEWS_SurfaceResistance (gsChoice=1): gs < 0 calculated, setting to 0.0001',gs,id_real,it)
            gs=0.0001
        endif

        !LAI
        !Original way
        !gl=((lai(id,2)*areaunir/lm)+areair)/(areair+areaunir)

        !New way
        gl=0    !First initialize
        ! vegetated surfaces
        ! check basis for values koe - maximum surface conductance
      !  print*,id,it,sfr
        do iv=ivConif,ivGrass
      !    print*,iv,laimax(iv),MaxConductance(iv),sfr(iv+2),lai(id-1,iv)
      !     pause
            ! check
            ! sg feb 2012 -- changed to use previous day LAI value
            !  and uses the maximum LAI read in
            !gl=gl+sfr(iv+2)*lai(id-1,iv)/MaxLaiMax*MaxConductance(iv)
            !gl=gl+(sfr(iv+2)*(1-snowFrac(iv+2)))*lai(id-1,iv)/MaxLaiMax*MaxConductance(iv)
            !sg 12nov13 -- removed maxLAImax - so just use the veg by class
               gl=gl+(sfr(iv+2)*(1-snowFrac(iv+2)))*lai(id-1,iv)/LaiMax(iv)*MaxConductance(iv)
        enddo

        gsc=(g1*gq*gdq*gtemp*gs*gl) !Original

        IF(gsc<=0) then
            call errorHint(65,'SUEWS_SurfaceResistance (gsChoice=1): gsc <= 0, setting to 0.1 mm s-1',gsc,id_real,it)
            gsc=0.1
        endif

    endif

  elseif(gsChoice == 2) then
     !At nighttime, currently set gsc to arbitrary low value 0.1 mm s-1 (Shuttleworth, 1988b)   !!Address this later - HCW!!
     if(avkdn<=0) then
        gsc=0.1
     else   !Calculate components of surface conductance
       ! ---- g(kdown)----
       QNM = Kmax/(Kmax+G2)
       gq = (avkdn/(avkdn+G2))/QNM
       if(avkdn >= Kmax) then   !! Add proper error handling later - HCW!!
           write(*,*) 'Kmax exceeds Kdn setting to g(Kdn) to 1'
           gq = 1
       endif
       ! ---- g(delq) ----
       gdq = G3 + (1-G3)*(G4**dq)   !Ogink-Hendriks (1995) Eq 12 (using G3 as Kshd and G4 as r)
       ! ---- g(Tair) ----
       Tc=(TH-G5)/(G5-TL)
       Tc2=(G5-TL)*(TH-G5)**Tc
       ! If air temperature below TL or above TH, then use value for TL+0.1 or TH-0.1
       if (Temp_C <= TL) then
          gtemp=(TL+0.1-TL)*(TH-(TL+0.1))**Tc/Tc2
          ! Call error only if no snow on ground
          if (min(snowFrac(1),snowFrac(2),snowFrac(3),snowFrac(4),snowFrac(5),snowFrac(6))/=1) then
             call errorHint(29,'subroutine SurfaceResistance:T changed to fit limits TL+0.1,Temp_C,id,it',&
                  real(Temp_c,kind(1d0)),id_real,it)
          endif
       elseif (Temp_C >= TH) then
          gtemp=((TH-0.1)-TL)*(TH-(TH-0.1))**Tc/Tc2
          call errorHint(29,'subroutine SurfaceResistance:T changed to fit limits TH-0.1,Temp_C,id,it',&
               real(Temp_c,kind(1d0)),id_real,it)
       else
          gtemp=(Temp_C-TL)*(TH-Temp_C)**Tc/Tc2
       endif
       ! ---- g(smd) ----
       sdp=S1/G6+S2
       if(smd_choice>0) then                           !Modified from ==1 to > 0 by HCW 31/07/2014
          gs=(1-exp(g6*(xsmd-sdp)))/(1-exp(g6*(-sdp))) !Use measured smd
       else
          gs=1-exp(g6*(vsmd-sdp))   !Use modelled smd
          gs=(1-exp(g6*(vsmd-sdp)))/(1-exp(g6*(-sdp)))
       endif

       gs = gs*(1-sum(snowFrac(1:6))/6)

       if(gs<0)then
          call errorHint(65,'SUEWS_SurfaceResistance (gsChoice=2): gs < 0 calculated, setting to 0.0001',gs,id_real,it)
          gs=0.0001
       endif

       ! ---- g(LAI) ----
       gl=0    !Initialise
       do iv=ivConif,ivGrass   !For vegetated surfaces
          gl=gl+(sfr(iv+2)*(1-snowFrac(iv+2)))*lai(id-1,iv)/LaiMax(iv)*MaxConductance(iv)
       enddo

       ! Multiply parts together
       gsc=(G1*gq*gdq*gtemp*gs*gl)

       if(gsc<=0) then
          call errorHint(65,'SUEWS_SurfaceResistance (gsChoice=2): gsc <= 0, setting to 0.1 mm s-1',gsc,id_real,it)
          gsc=0.1
       endif

    endif

  endif

  ResistSurf=1/(gsc/1000)  ![s m-1]

!   ===============================
!   for debug
  write(*,*) 'iy,id,it,imin,gsc, g1, gq, gdq, gtemp, gs, gl:',iy,id,it,imin,gsc, g1, gq, gdq, gtemp, gs, gl
!   ===============================

  return
end subroutine SurfaceResistance
