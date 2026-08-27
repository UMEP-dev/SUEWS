module module_phys_lai
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use module_ctrl_const_allocate, only: nvegsurf
    use module_ctrl_error_state, only: set_supy_error, supy_error_flag

    implicit none

contains

   subroutine update_GDDLAI( &
      id, LAICalcYes, & !input
      lat, LAI_obs, &
      Tmin_id_prev, Tmax_id_prev, lenDay_id_prev, &
      BaseT_GDD, BaseT_SDD, &
      GDDFull, SDDFull, &
      LAIMin, LAIMax, LAIPower, LAIType, &
      LAI_id_prev, &
      GDD_id, SDD_id, & !inout
      LAI_id_next) !output
      
      implicit none

      !------------------------------------------------------------------------------
      ! Calculation of LAI from growing degree days
      ! This was revised and checked on 16 Feb 2014 by LJ
      !------------------------------------------------------------------------------

      integer, intent(in) :: id
      integer, intent(in) :: LAICalcYes

      real(kind(1D0)), intent(in) :: lat
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: LAI_obs
      real(kind(1D0)), intent(in) :: Tmin_id_prev
      real(kind(1D0)), intent(in) :: Tmax_id_prev
      real(kind(1D0)), intent(in) :: lenDay_id_prev

      ! --- Vegetation phenology ---------------------------------------------------------------------
      ! Parameters provided in input information for each vegetation surface (SUEWS_Veg.txt)
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: BaseT_GDD !Base temperature for growing degree days [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: BaseT_SDD !Base temperature for senescence degree days [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: GDDFull !Growing degree days needed for full capacity [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: SDDFull !Senescence degree days needed to initiate leaf off [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: laimin !Min LAI [m2 m-2]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: laimax !Max LAI [m2 m-2]
      real(kind(1D0)), dimension(4, nvegsurf), intent(in) :: LAIPower !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off
      !! N.B. currently DecTr only, although input provided for all veg types
      integer, dimension(nvegsurf), intent(in) :: LAIType !LAI equation to use: original (0) or new (1)

      real(kind(1D0)), dimension(3), intent(inout) :: GDD_id !Growing Degree Days (see SUEWS_DailyState.f95)
      real(kind(1D0)), dimension(3), intent(inout) :: SDD_id !Senescence Degree Days (see SUEWS_DailyState.f95)
      real(kind(1D0)), dimension(nvegsurf), intent(out) :: LAI_id_next !LAI for each veg surface [m2 m-2]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: LAI_id_prev ! LAI of previous day

      real(kind(1D0)) :: delta_SDD !Switches and checks for GDD
      real(kind(1D0)) :: delta_GDD !Switches and checks for GDD
      real(kind(1D0)) :: indHelp !Switches and checks for GDD
      real(kind(1D0)), dimension(3) :: GDD_id_prev ! GDD of previous day
      real(kind(1D0)), dimension(3) :: SDD_id_prev ! SDD of previous day

      integer :: critDays = 50 !Critical limit for GDD when GDD or SDD is set to zero
      integer :: iv
      
      integer, parameter :: LAI_ORIGINAL = 0
      integer, parameter :: LAI_NEW = 1

      integer, parameter :: SEN_DAYLENGTH = 1
      integer, parameter :: SEN_SDD = 2

      logical :: valid_observed_lai
      logical :: cold_spring
      
      ! translate values of previous day to local variables
      GDD_id_prev = GDD_id
      SDD_id_prev = SDD_id

      if (LAICalcYes == 0) then
         call observed_lai(valid_observed_lai)
         if (.not. valid_observed_lai) return
      end if
      
      ! Loop through vegetation types (iv)
      do iv = 1, NVegSurf

         call calc_delta_gdd_sdd( &
            tmin_prev=Tmin_id_prev, &
            tmax_prev=Tmax_id_prev, &
            base_t_gdd=BaseT_GDD(iv), &
            base_t_sdd=BaseT_SDD(iv), &
            delta_gdd=delta_GDD, &
            delta_sdd=delta_SDD, &
            ind_help=indHelp &
         )
         
         call apply_delta_gdd_sdd( &
            gdd_prev=GDD_id_prev(iv), &
            sdd_prev=SDD_id_prev(iv), &
            delta_gdd=delta_GDD, &
            delta_sdd=delta_SDD, &
            gdd_id=GDD_id(iv), &
            sdd_id=SDD_id(iv) &
         )

         cold_spring = cold_spring_condition( &
            sdd_id=sdd_id(iv), &
            sdd_full=sddFull(iv), &
            ind_help=indHelp &
         )

         call limit_gdd_sdd( &
            GDD_id=GDD_id(iv), &
            SDD_id=SDD_id(iv), &
            GDDFull=GDDFull(iv), &
            SDDFull=SDDFull(iv), &
            critDays=critDays &
         )

         ! With these limits SDD, GDD is set to zero
         if (SDD_id(iv) < -critDays .AND. SDD_id(iv) > SDDFull(iv)) GDD_id(iv) = 0
         if (GDD_id(iv) > critDays .AND. GDD_id(iv) < GDDFull(iv)) SDD_id(iv) = 0

         ! Now calculate LAI itself
         if (lat >= 0) THEN !Northern hemispere
            call reset_degree_day_states( &
               id=id, &
               sdd_reset_day=140, &
               crit_days=critDays, &
               summer_day=170, &
               winter_day=170, &
               southern_hemisphere=.false., &
               sdd_id=SDD_id(iv), &
               gdd_id=GDD_id(iv) &
            )
            
            if (LAICalcYes /= 0) then
               call calculate_lai( &
                  senescence_mode=SEN_DAYLENGTH, &
                  id=id, &
                  SDD_id=SDD_id(iv), &
                  GDD_id=GDD_id(iv), &
                  critDays=critDays, &
                  LAItype=LAItype(iv), &
                  LAIPower=LAIPower(:, iv), &
                  GDDFull=GDDFull(iv), &
                  SDDFull=SDDFull(iv), &
                  cold_spring=cold_spring, &
                  lenDay_id_prev=lenDay_id_prev, &
                  laimax=laimax(iv), &
                  laimin=laimin(iv), &
                  LAI_id_prev=LAI_id_prev(iv), &
                  LAI_id_next=LAI_id_next(iv) &
               )
            end if

         else !Southern hemisphere !! N.B. not identical to N hemisphere - return to later
            call reset_degree_day_states( &
               id=id, &
               sdd_reset_day=300, &
               crit_days=critDays, &
               summer_day=250, &
               winter_day=250, &
               southern_hemisphere=.true., &
               sdd_id=SDD_id(iv), &
               gdd_id=GDD_id(iv) &
            )

            if (LAICalcYes /= 0) then
               call calculate_lai( &
                  senescence_mode=SEN_SDD, &
                  id=id, &
                  SDD_id=SDD_id(iv), &
                  GDD_id=GDD_id(iv), &
                  critDays=critDays, &
                  LAItype=LAItype(iv), &
                  LAIPower=LAIPower(:, iv), &
                  GDDFull=GDDFull(iv), &
                  SDDFull=SDDFull(iv), &
                  cold_spring=cold_spring, &
                  lenDay_id_prev=lenDay_id_prev, &
                  laimax=laimax(iv), &
                  laimin=laimin(iv), &
                  LAI_id_prev=LAI_id_prev(iv), &
                  LAI_id_next=LAI_id_next(iv) &
               )

            end if

         end if !N or S hemisphere
            
      end do !End of loop over veg surfaces

      !------------------------------------------------------------------------------

   CONTAINS
   
      subroutine observed_lai(valid)

         implicit none

         logical, intent(out) :: valid

         valid = .false.

         if (any(ieee_is_nan(LAI_obs)) .or. any(LAI_obs < 0.0D0)) then
            ! Invalid LAI_obs slipped past pre-flight; raise an error before
            ! mutating phenology state and assign a safe sentinel to the output.
            LAI_id_next = -999.0D0
            call set_supy_error( &
               105, &
               'update_GDDLAI: laimethod=0 requires non-missing lai_* or lai >= 0 at every timestep')
            return
         end if

         ! Observed-LAI override: when LAICalcYes == 0, every timestep's forcing
         ! value must be a non-missing, non-negative observation (LAI_obs >= 0).
         
         ! A genuine zero observation (e.g. complete winter dieback) is valid and
         ! passes through unchanged. Missing/NaN values and strictly negative
         ! values - including the -999 missing sentinel - are rejected; choosing
         ! this path commits the user to providing an observation for every
         ! timestep. Observed LAI is intentionally not clipped to LAImin/LAImax,
         ! because those bounds describe the internal GDD/SDD phenology path.

         ! The Python pre-flight validator (supy._check.check_forcing) enforces
         ! this contract before a run starts; the guard below is a defensive
         ! backstop for callers that bypass preflight. Reports via
         ! module_ctrl_error_state so SuPy can surface a clean exception —
         ! never WRITE(*,...) + STOP, which kills the embedding Python process.

         ! Copy the effective observed LAI for EveTr, DecTr and Grass into the
         ! daily state without applying the GDD/SDD envelope.
         do iv = 1, NVegSurf
            LAI_id_next(iv) = LAI_obs(iv)
         end do

         valid = .true.

      end subroutine observed_lai

      subroutine calc_delta_gdd_sdd( &
            tmin_prev, tmax_prev, base_t_gdd, base_t_sdd, &
            delta_gdd, delta_sdd, ind_help)

         implicit none

         real(kind(1D0)), intent(in)  :: tmin_prev
         real(kind(1D0)), intent(in)  :: tmax_prev
         real(kind(1D0)), intent(in)  :: base_t_gdd
         real(kind(1D0)), intent(in)  :: base_t_sdd

         real(kind(1D0)), intent(out) :: delta_gdd
         real(kind(1D0)), intent(out) :: delta_sdd
         real(kind(1D0)), intent(out) :: ind_help

         ! Calculate GDD and SDD
         delta_gdd = calc_delta_degree_days( &
            tmin_prev, tmax_prev, base_t_gdd)

         delta_sdd = calc_delta_degree_days( &
            tmin_prev, tmax_prev, base_t_sdd)

         ! SDD cannot be positive
         if (delta_sdd > 0) delta_sdd = 0

         ! Help switch to allow GDD to go to zero in spring-time
         ind_help = 0

         ! GDD cannot be negative
         if (delta_gdd < 0) then
            ind_help = delta_gdd
            delta_gdd = 0
         end if

      end subroutine calc_delta_gdd_sdd

      function calc_delta_degree_days(Tmin_prev, Tmax_prev, base_t) result(delta_dd)

         implicit none

         real(kind(1D0)), intent(in) :: Tmin_prev
         real(kind(1D0)), intent(in) :: Tmax_prev
         real(kind(1D0)), intent(in) :: base_t

         real(kind(1D0)) :: delta_dd

         delta_dd = (Tmin_prev + Tmax_prev) / 2 - base_t

      end function calc_delta_degree_days

      subroutine apply_delta_gdd_sdd(gdd_prev, sdd_prev, &
                                    delta_gdd, delta_sdd, &
                                    gdd_id, sdd_id)

         implicit none

         real(kind(1D0)), intent(in) :: gdd_prev
         real(kind(1D0)), intent(in) :: sdd_prev
         real(kind(1D0)), intent(in) :: delta_gdd
         real(kind(1D0)), intent(in) :: delta_sdd

         real(kind(1D0)), intent(out) :: gdd_id
         real(kind(1D0)), intent(out) :: sdd_id

         ! Calculate cumulative growing and senescence degree days
         gdd_id = gdd_prev + delta_gdd
         sdd_id = sdd_prev + delta_sdd

      end subroutine apply_delta_gdd_sdd

      function cold_spring_condition(sdd_id, sdd_full, ind_help) result(cold_spring)
      
         implicit none

         real(kind(1D0)), intent(in) :: sdd_id
         real(kind(1D0)), intent(in) :: sdd_full
         real(kind(1D0)), intent(in) :: ind_help
         
         logical :: cold_spring

         cold_spring = ((sdd_id <= sdd_full) .and. (ind_help < 0))

      end function cold_spring_condition

      subroutine limit_gdd_sdd( &
            GDD_id, SDD_id, GDDFull, SDDFull, critDays)

         implicit none

         real(kind(1D0)), intent(inout) :: GDD_id
         real(kind(1D0)), intent(inout) :: SDD_id
         real(kind(1D0)), intent(in) :: GDDFull
         real(kind(1D0)), intent(in) :: SDDFull
         
         integer, intent(in) :: critDays

         !Start senescence
         if (GDD_id >= GDDFull) then
            GDD_id = GDDFull !Leaves should not grow so delete yes from earlier
            if (SDD_id < -critDays) GDD_id = 0
         end if

         !After senescence now start growing leaves
         if (SDD_id <= SDDFull) then
            SDD_id = SDDFull !Leaves off so add back earlier
            if (GDD_id > critDays) SDD_id = 0
         end if

      end subroutine limit_gdd_sdd

      subroutine calculate_lai( &
            senescence_mode, &
            id, SDD_id, GDD_id, critDays, LAItype, LAIPower, GDDFull, SDDFull, &
            cold_spring, lenDay_id_prev, LAI_id_prev, laimax, laimin, LAI_id_next)

         implicit none

         integer, intent(in) :: senescence_mode
         integer, intent(in) :: id

         real(kind(1D0)), intent(inout) :: SDD_id
         real(kind(1D0)), intent(inout) :: GDD_id
         
         integer, intent(in) :: critDays
         integer, intent(in) :: LAItype
         
         real(kind(1D0)), dimension(4), intent(in) :: LAIPower

         real(kind(1D0)), intent(in) :: GDDFull
         real(kind(1D0)), intent(in) :: SDDFull
         real(kind(1D0)), intent(in) :: lenDay_id_prev
         real(kind(1D0)), intent(in) :: LAI_id_prev
         real(kind(1D0)), intent(in) :: laimax
         real(kind(1D0)), intent(in) :: laimin
         real(kind(1D0)), intent(out) :: LAI_id_next

         logical, intent(in) :: cold_spring

         logical :: start_senescence

         if (GDD_id > 0 .and. GDD_id < GDDFull) then !Leaves can still grow
            ! Allow cold-spring to prevent further growth
            if (.not. cold_spring) then
               call calculate_gdd( &
                  LAI_id_prev=LAI_id_prev, &
                  LAIPower=LAIPower, &
                  GDD_id=GDD_id, &
                  LAI_id_next=LAI_id_next &
               )
            end if
         
         else if (LAItype <= LAI_ORIGINAL) THEN !Original LAI type

            if (SDD_id < 0 .and. SDD_id > SDDFull) then !Start senescence
               call calculate_sdd_type0( &
                  LAI_id_prev=LAI_id_prev, &
                  LAIPower=LAIPower, &
                  SDD_id=SDD_id, &
                  LAI_id_next=LAI_id_next &
               )

            else
               LAI_id_next = LAI_id_prev

            end if

         else

            !! Use day length to start senescence at high latitudes (controlled in senescence_mode)
            start_senescence = check_start_senescence( &
               senescence_mode=senescence_mode, &
               lenDay_id_prev=lenDay_id_prev, &
               SDD_id=SDD_id, &
               SDDFull=SDDFull &
            )

            if (start_senescence) then !Start senescence
               call calculate_sdd_type1( &
                  LAI_id_prev=LAI_id_prev, &
                  LAIPower=LAIPower, &
                  SDD_id=SDD_id, &
                  LAI_id_next=LAI_id_next &
               )
            else
               LAI_id_next = LAI_id_prev
            end if

         end if

         ! Keep internally computed phenology within the configured canopy envelope.
         call limit_lai( &
            LAI_id_next=LAI_id_next, &
            LAImax=LAImax, &
            LAImin=LAImin &
         )

      end subroutine calculate_lai

      subroutine reset_degree_day_states( &
         id, sdd_reset_day, crit_days, summer_day, winter_day, &
         southern_hemisphere, sdd_id, gdd_id)

         implicit none

         integer, intent(in) :: id
         integer, intent(in) :: sdd_reset_day
         integer, intent(in) :: crit_days
         integer, intent(in) :: summer_day
         integer, intent(in) :: winter_day
         logical, intent(in) :: southern_hemisphere

         real(kind(1D0)), intent(inout) :: sdd_id
         real(kind(1D0)), intent(inout) :: gdd_id

         ! if SDD is not zero by the transition day, force it
         if (id == sdd_reset_day .and. sdd_id /= 0) sdd_id = 0

         if (southern_hemisphere) then

            ! Set SDD to zero in southern summer
            if (gdd_id > crit_days .and. id > summer_day) sdd_id = 0

            ! Set GDD zero in southern winter
            if (sdd_id < -crit_days .and. id < winter_day) gdd_id = 0

         else

            ! Set SDD to zero in northern summer
            if (gdd_id > crit_days .and. id < summer_day) sdd_id = 0

            ! Set GDD zero in northern winter
            if (sdd_id < -crit_days .and. id > winter_day) gdd_id = 0

         end if

      end subroutine reset_degree_day_states

      function check_start_senescence(senescence_mode, lenDay_id_prev, SDD_id, SDDFull) result(start_senescence)
         
         implicit none
         
         integer, intent(in) :: senescence_mode

         real(kind(1D0)), intent(in) :: lenDay_id_prev
         real(kind(1D0)), intent(in) :: SDD_id
         real(kind(1D0)), intent(in) :: SDDFull

         logical :: start_senescence
         
         select case (senescence_mode)

            case (SEN_DAYLENGTH)
               start_senescence = ((lenDay_id_prev <= 12) .and. (SDD_id > SDDFull))

            case (SEN_SDD)
               start_senescence = ((SDD_id < 0) .and. (SDD_id > SDDFull))

            case default
               ! Invalid option falls back to SEN_SDD. No error yet registered.
               ! default currently not possible as function calls hard-coded
               start_senescence = ((SDD_id < 0) .and. (SDD_id > SDDFull))

         end select

      end function check_start_senescence

      subroutine calculate_gdd( &
            LAI_id_prev, LAIPower, GDD_id, LAI_id_next)

         implicit none

         real(kind(1D0)), intent(in) :: LAI_id_prev
         real(kind(1D0)), dimension(4), intent(in) :: LAIPower
         real(kind(1D0)), intent(in) :: GDD_id
         real(kind(1D0)), intent(out) :: LAI_id_next

         LAI_id_next = (LAI_id_prev**LAIPower(1) * &
                        GDD_id * LAIPower(2)) + LAI_id_prev

      end subroutine calculate_gdd
   
      subroutine calculate_sdd_type0( &
            LAI_id_prev, LAIPower, SDD_id, LAI_id_next)

         implicit none

         real(kind(1D0)), intent(in) :: LAI_id_prev
         real(kind(1D0)), dimension(4), intent(in) :: LAIPower
         real(kind(1D0)), intent(in) :: SDD_id
         real(kind(1D0)), intent(out) :: LAI_id_next

         LAI_id_next = (LAI_id_prev**LAIPower(3) * SDD_id * LAIPower(4)) + LAI_id_prev

      end subroutine calculate_sdd_type0
   
      subroutine calculate_sdd_type1( & ! 
            LAI_id_prev, LAIPower, SDD_id, LAI_id_next)

         implicit none

         real(kind(1D0)), intent(in) :: LAI_id_prev
         real(kind(1D0)), dimension(4), intent(in) :: LAIPower
         real(kind(1D0)), intent(in) :: SDD_id
         real(kind(1D0)), intent(out) :: LAI_id_next

         LAI_id_next = (LAI_id_prev * LAIPower(3) * (1 - SDD_id) * LAIPower(4)) + LAI_id_prev

      end subroutine calculate_sdd_type1

      subroutine limit_lai(lai_id_next, laimax, laimin)

         ! Keep internally computed phenology within the configured canopy envelope.

         implicit none

         real(kind(1D0)), intent(inout) :: lai_id_next
         real(kind(1D0)), intent(in) :: laimax
         real(kind(1D0)), intent(in) :: laimin

         if (lai_id_next > LAImax) then
            lai_id_next = laimax
         else if (lai_id_next < LAImin) then
            lai_id_next = laimin
         end if

      end subroutine limit_lai

   end subroutine update_gddlai

end module module_phys_lai