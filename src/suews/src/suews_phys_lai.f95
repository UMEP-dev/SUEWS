module module_phys_lai
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use module_ctrl_const_allocate, only: nvegsurf
    use module_ctrl_error_state, only: set_supy_error, supy_error_flag

    implicit none

contains

    subroutine update_gddlai( &
      id, lai_calc_yes, & !input
      lat, lai_obs, &
      t_min_id_prev, t_max_id_prev, len_day_id_prev, &
      base_t_gdd, base_t_sdd, &
      gdd_full, sdd_full, &
      lai_min, lai_max, lai_power, lai_type, &
      lai_id_prev, &
      dd_type, gdd_id, sdd_id, & !inout
      lai_id_next) !output
      
      implicit none

      !------------------------------------------------------------------------------
      ! Calculation of LAI from growing degree days
      ! This was revised and checked on 16 Feb 2014 by LJ
      !------------------------------------------------------------------------------

      integer, intent(in) :: id
      integer, intent(in) :: lai_calc_yes

      real(kind(1D0)), intent(in) :: lat
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: lai_obs
      real(kind(1D0)), intent(in) :: t_min_id_prev
      real(kind(1D0)), intent(in) :: t_max_id_prev
      real(kind(1D0)), intent(in) :: len_day_id_prev

      ! --- Vegetation phenology ---------------------------------------------------------------------
      ! Parameters provided in input information for each vegetation surface (SUEWS_Veg.txt)
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: base_t_gdd !Base temperature for growing degree days [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: base_t_sdd !Base temperature for senescence degree days [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: gdd_full !Growing degree days needed for full capacity [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: sdd_full !Senescence degree days needed to initiate leaf off [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: lai_min !Min LAI [m2 m-2]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: lai_max !Max LAI [m2 m-2]
      real(kind(1D0)), dimension(4, nvegsurf), intent(in) :: lai_power !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off

      !! N.B. currently DecTr only, although input provided for all veg types
      integer, dimension(nvegsurf), intent(in) :: lai_type !LAI equation to use: original (0) or new (1)
      
      ! 0: Use accumulated GDD for LAI calculations, 1: Use daily change in GDD
      integer, dimension(nvegsurf), intent(in):: dd_type

      real(kind(1D0)), dimension(3), intent(inout) :: gdd_id !Growing Degree Days (see SUEWS_DailyState.f95)
      real(kind(1D0)), dimension(3), intent(inout) :: sdd_id !Senescence Degree Days (see SUEWS_DailyState.f95)
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: lai_id_prev ! LAI of previous day
      real(kind(1D0)), dimension(nvegsurf), intent(out) :: lai_id_next !LAI for each veg surface [m2 m-2]

      real(kind(1D0)) :: delta_gdd !Switches and checks for GDD
      real(kind(1D0)) :: delta_sdd !Switches and checks for GDD
      real(kind(1D0)) :: ind_help !Switches and checks for GDD
      real(kind(1D0)), dimension(3) :: gdd_id_prev ! GDD of previous day
      real(kind(1D0)), dimension(3) :: sdd_id_prev ! SDD of previous day

      integer, parameter :: CRIT_DAYS = 50 !Critical limit for GDD when GDD or SDD is set to zero

      integer :: iv
      
      ! Enumeration parameters for the LAI type
      integer, parameter :: LAI_ORIGINAL = 0
      integer, parameter :: LAI_NEW = 1

      ! Enumeration parameters for senescence conditions
      integer, parameter :: SEN_DAYLENGTH = 1
      integer, parameter :: SEN_SDD = 2

      ! Option for gdd value in LAI calculation
      integer, parameter :: GDD_ACCUMULATIVE = 0
      integer, parameter :: GDD_CHANGE = 1

      logical :: valid_observed_lai

      ! Hemisphere specific parameters for LAI calculations
      integer :: sdd_reset_day
      integer :: summer_day
      integer :: winter_day
      integer :: senescence_mode
      logical :: southern_hemisphere

      logical :: cold_spring

      ! translate values of previous day to local variables
      GDD_id_prev = GDD_id
      SDD_id_prev = SDD_id

      if (lai_calc_yes == 0) then
         call observed_lai(valid_observed_lai)
         if (.not. valid_observed_lai) return
      end if
      
      ! Determine N/S hemisphere parameters
      ! TODO: Move outside timestep loop as timestep independent
      if (lat >= 0) then
         sdd_reset_day = 140
         summer_day = 170
         winter_day = 170
         southern_hemisphere = .false.
         senescence_mode = SEN_DAYLENGTH
      else !! N.B. not identical to N hemisphere - return to later
         sdd_reset_day = 300
         summer_day = 250
         winter_day = 250
         southern_hemisphere = .true.
         senescence_mode = SEN_SDD
      end if

      ! Loop through vegetation types (iv)
      do iv = 1, NVegSurf

         call calc_delta_gdd_sdd( &
            tmin_prev=t_min_id_prev, &
            tmax_prev=t_max_id_prev, &
            base_t_gdd=base_t_gdd(iv), &
            base_t_sdd=base_t_sdd(iv), &
            delta_gdd=delta_gdd, &
            delta_sdd=delta_sdd, &
            ind_help=ind_help &
         )

         ! Calculate cumulative growing and senescence degree days
         gdd_id(iv) = gdd_id_prev(iv) + delta_gdd
         sdd_id(iv) = sdd_id_prev(iv) + delta_sdd

         cold_spring = cold_spring_condition( &
            sdd_id=sdd_id(iv), &
            sdd_full=sdd_full(iv), &
            ind_help=ind_help &
         )

         call limit_gdd_sdd( &
            gdd_full=gdd_full(iv), &
            sdd_full=sdd_full(iv), &
            gdd_id=gdd_id(iv), &
            sdd_id=sdd_id(iv) &
         )

         ! Now calculate LAI itself
         call reset_degree_day_states( &
            id=id, &
            sdd_reset_day=sdd_reset_day, &
            summer_day=summer_day, &
            winter_day=winter_day, &
            gdd_id=gdd_id(iv), &
            sdd_id=sdd_id(iv), &
            southern_hemisphere=southern_hemisphere &
         )

         if (lai_calc_yes /= 0) then
            call calculate_lai( &
               dd_type=dd_type(iv), &
               senescence_mode=senescence_mode, &
               len_day_id_prev=len_day_id_prev, &
               gdd_id=gdd_id(iv), &
               sdd_id=sdd_id(iv), &
               gdd_full=gdd_full(iv), &
               sdd_full=sdd_full(iv), &
               cold_spring=cold_spring, &
               lai_type=lai_type(iv), &
               lai_power=lai_power(:, iv), &
               lai_max=lai_max(iv), &
               lai_min=lai_min(iv), &
               lai_id_prev=lai_id_prev(iv), &
               lai_id_next=lai_id_next(iv) &
            )
         end if
            
      end do !End of loop over veg surfaces

      !------------------------------------------------------------------------------

   CONTAINS
   
      subroutine observed_lai(valid)

         implicit none

         logical, intent(out) :: valid

         valid = .false.

         if (any(ieee_is_nan(lai_obs)) .or. any(lai_obs < 0.0D0)) then
            ! Invalid lai_obs slipped past pre-flight; raise an error before
            ! mutating phenology state and assign a safe sentinel to the output.
            lai_id_next = -999.0D0
            call set_supy_error( &
               105, &
               'update_GDDLAI: laimethod=0 requires non-missing lai_* or lai >= 0 at every timestep')
            return
         end if

         ! Observed-LAI override: when lai_calc_yes == 0, every timestep's forcing
         ! value must be a non-missing, non-negative observation (lai_obs >= 0).
         
         ! A genuine zero observation (e.g. complete winter dieback) is valid and
         ! passes through unchanged. Missing/NaN values and strictly negative
         ! values - including the -999 missing sentinel - are rejected; choosing
         ! this path commits the user to providing an observation for every
         ! timestep. Observed LAI is intentionally not clipped to lai_min/lai_max,
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

      function cold_spring_condition(sdd_id, sdd_full, ind_help) result(cold_spring)
      
         implicit none

         real(kind(1D0)), intent(in) :: sdd_id
         real(kind(1D0)), intent(in) :: sdd_full
         real(kind(1D0)), intent(in) :: ind_help
         
         logical :: cold_spring

         cold_spring = ((sdd_id <= sdd_full) .and. (ind_help < 0))

      end function cold_spring_condition

      subroutine limit_gdd_sdd( &
            gdd_full, sdd_full, gdd_id, sdd_id)

         implicit none

         real(kind(1D0)), intent(in) :: gdd_full
         real(kind(1D0)), intent(in) :: sdd_full
         real(kind(1D0)), intent(inout) :: gdd_id
         real(kind(1D0)), intent(inout) :: sdd_id

         !Start senescence
         if (gdd_id >= gdd_full) then
            gdd_id = gdd_full !Leaves should not grow so delete yes from earlier
            if (sdd_id < -CRIT_DAYS) gdd_id = 0
         end if

         !After senescence now start growing leaves
         if (sdd_id <= sdd_full) then
            sdd_id = sdd_full !Leaves off so add back earlier
            if (gdd_id > CRIT_DAYS) sdd_id = 0
         end if

         ! With these limits SDD, GDD is set to zero
         if (sdd_id < -CRIT_DAYS .AND. sdd_id > sdd_full) gdd_id = 0
         if (gdd_id > CRIT_DAYS .AND. gdd_id < gdd_full) sdd_id = 0

      end subroutine limit_gdd_sdd

      subroutine calculate_lai( &
            dd_type, senescence_mode, &
            gdd_id, sdd_id, lai_type, lai_power, gdd_full, sdd_full, &
            cold_spring, len_day_id_prev, lai_id_prev, lai_max, lai_min, lai_id_next)

         implicit none

         integer, intent(in) :: dd_type
         integer, intent(in) :: senescence_mode

         real(kind(1D0)), intent(in) :: gdd_id
         real(kind(1D0)), intent(in) :: sdd_id
         
         integer, intent(in) :: lai_type
         
         real(kind(1D0)), dimension(4), intent(in) :: lai_power

         real(kind(1D0)), intent(in) :: gdd_full
         real(kind(1D0)), intent(in) :: sdd_full
         real(kind(1D0)), intent(in) :: len_day_id_prev
         real(kind(1D0)), intent(in) :: lai_id_prev
         real(kind(1D0)), intent(in) :: lai_max
         real(kind(1D0)), intent(in) :: lai_min
         real(kind(1D0)), intent(out) :: lai_id_next

         logical, intent(in) :: cold_spring

         logical :: start_senescence

         ! Temp variable to store gdd value based on gdd_option
         real(kind(1D0)) :: gdd
         real(kind(1D0)) :: sdd

         lai_id_next = lai_id_prev

         if (dd_type == GDD_CHANGE) then
            gdd = delta_gdd
            sdd = delta_sdd
         else
            gdd = gdd_id
            sdd = sdd_id
         end if

         if (gdd_id > 0 .and. gdd_id < gdd_full) then !Leaves can still grow
            ! Allow cold-spring to prevent further growth
            if (.not. cold_spring) then
               call calculate_gdd( &
                  lai_id_prev=lai_id_prev, &
                  lai_power=lai_power, &
                  gdd=gdd, &
                  lai_id_next=lai_id_next &
               )
            end if
         
         else if (lai_type <= LAI_ORIGINAL) THEN !Original LAI type
            if (sdd_id < 0 .and. sdd_id > sdd_full) then !Start senescence
               call calculate_sdd_type0( &
                  lai_id_prev=lai_id_prev, &
                  lai_power=lai_power, &
                  sdd=sdd, &
                  lai_id_next=lai_id_next &
               )
            end if

         else
            !! Use day length to start senescence at high latitudes (controlled in senescence_mode)
            start_senescence = check_start_senescence( &
               senescence_mode=senescence_mode, &
               len_day_id_prev=len_day_id_prev, &
               sdd_id=sdd_id, &
               sdd_full=sdd_full &
            )

            if (start_senescence) then !Start senescence
               call calculate_sdd_type1( &
                  lai_id_prev=lai_id_prev, &
                  lai_power=lai_power, &
                  sdd=sdd, &
                  lai_id_next=lai_id_next &
               )
            end if

         end if

         ! Keep internally computed phenology within the configured canopy envelope.
         call limit_lai( &
            lai_id_next=lai_id_next, &
            lai_max=lai_max, &
            lai_min=lai_min &
         )

      end subroutine calculate_lai

      subroutine reset_degree_day_states( &
         id, sdd_reset_day, summer_day, winter_day, &
         southern_hemisphere, sdd_id, gdd_id)

         implicit none

         integer, intent(in) :: id
         integer, intent(in) :: sdd_reset_day
         integer, intent(in) :: summer_day
         integer, intent(in) :: winter_day
         logical, intent(in) :: southern_hemisphere

         real(kind(1D0)), intent(inout) :: sdd_id
         real(kind(1D0)), intent(inout) :: gdd_id

         ! if SDD is not zero by the transition day, force it
         if (id == sdd_reset_day .and. sdd_id /= 0) sdd_id = 0

         if (southern_hemisphere) then

            ! Set SDD to zero in southern summer
            if (gdd_id > CRIT_DAYS .and. id > summer_day) sdd_id = 0

            ! Set GDD zero in southern winter
            if (sdd_id < -CRIT_DAYS .and. id < winter_day) gdd_id = 0

         else

            ! Set SDD to zero in northern summer
            if (gdd_id > CRIT_DAYS .and. id < summer_day) sdd_id = 0

            ! Set GDD zero in northern winter
            if (sdd_id < -CRIT_DAYS .and. id > winter_day) gdd_id = 0

         end if

      end subroutine reset_degree_day_states

      function check_start_senescence(senescence_mode, len_day_id_prev, sdd_id, sdd_full) result(start_senescence)
         
         implicit none
         
         integer, intent(in) :: senescence_mode

         real(kind(1D0)), intent(in) :: len_day_id_prev
         real(kind(1D0)), intent(in) :: sdd_id
         real(kind(1D0)), intent(in) :: sdd_full

         logical :: start_senescence
         
         select case (senescence_mode)

            case (SEN_DAYLENGTH)
               start_senescence = ((len_day_id_prev <= 12) .and. (sdd_id > sdd_full))

            case (SEN_SDD)
               start_senescence = ((sdd_id < 0) .and. (sdd_id > sdd_full))

            case default
               ! Invalid option falls back to SEN_SDD. No error yet registered.
               ! default currently not possible as function calls hard-coded
               start_senescence = ((sdd_id < 0) .and. (sdd_id > sdd_full))

         end select

      end function check_start_senescence

      subroutine calculate_gdd( &
            lai_id_prev, lai_power, gdd, lai_id_next)

         implicit none

         real(kind(1D0)), intent(in) :: lai_id_prev
         real(kind(1D0)), dimension(4), intent(in) :: lai_power
         real(kind(1D0)), intent(in) :: gdd
         real(kind(1D0)), intent(out) :: lai_id_next

         LAI_id_next = (lai_id_prev**lai_power(1) * &
                        gdd * lai_power(2)) + lai_id_prev

      end subroutine calculate_gdd
   
      subroutine calculate_sdd_type0( &
            lai_id_prev, lai_power, sdd, lai_id_next)

         implicit none

         real(kind(1D0)), intent(in) :: lai_id_prev
         real(kind(1D0)), dimension(4), intent(in) :: lai_power
         real(kind(1D0)), intent(in) :: sdd
         real(kind(1D0)), intent(out) :: lai_id_next

         lai_id_next = (lai_id_prev**lai_power(3) * sdd * lai_power(4)) + lai_id_prev
      end subroutine calculate_sdd_type0
   
      subroutine calculate_sdd_type1( & ! 
            lai_id_prev, lai_power, sdd, lai_id_next)

         implicit none

         real(kind(1D0)), intent(in) :: lai_id_prev
         real(kind(1D0)), dimension(4), intent(in) :: lai_power
         real(kind(1D0)), intent(in) :: sdd
         real(kind(1D0)), intent(out) :: lai_id_next

         lai_id_next = (lai_id_prev * lai_power(3) * (1 - sdd) * lai_power(4)) + lai_id_prev

      end subroutine calculate_sdd_type1

      subroutine limit_lai(lai_id_next, lai_max, lai_min)

         ! Keep internally computed phenology within the configured canopy envelope.

         implicit none

         real(kind(1D0)), intent(inout) :: lai_id_next
         real(kind(1D0)), intent(in) :: lai_max
         real(kind(1D0)), intent(in) :: lai_min

         if (lai_id_next > lai_max) then
            lai_id_next = lai_max
         else if (lai_id_next < lai_min) then
            lai_id_next = lai_min
         end if

      end subroutine limit_lai

   end subroutine update_gddlai

end module module_phys_lai