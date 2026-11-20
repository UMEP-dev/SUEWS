! =============================================================================
! DO NOT EDIT -- emitted from scripts/suews/gen_capi_field_observer.py.
! Test-only typed Fortran C-API field-binding observer (gh#2096).
! Regenerate with: python3 scripts/suews/gen_capi_field_observer.py
! =============================================================================
module module_test_capi_field_observer
   use, intrinsic :: iso_c_binding, only: c_int, c_int64_t, c_double
   use module_c_api_common, only: SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER
   use module_ctrl_type, only: flag_STATE
   use module_type_anthro, only: anthroEMIS_PRM, anthroEmis_STATE
   use module_type_atmosphere, only: atm_state, solar_State
   use module_type_ehc, only: EHC_PRM
   use module_type_heat, only: HEAT_STATE
   use module_type_hydro, only: HYDRO_STATE
   use module_type_landcover, only: LC_BLDG_PRM, LC_BSOIL_PRM, LC_DECTR_PRM, LC_EVETR_PRM, LC_GRASS_PRM, LC_PAVED_PRM, LC_WATER_PRM
   use module_type_snow, only: SNOW_PRM, SNOW_STATE
   use module_type_spartacus, only: SPARTACUS_LAYER_PRM, SPARTACUS_PRM
   use module_type_stebbs, only: BUILDING_ARCHETYPE_PRM, NHOOD_STATE, STEBBS_PRM, STEBBS_STATE
   use module_type_surface, only: CONDUCTANCE_PRM, LUMPS_PRM, OHM_STATE, ROUGHNESS_STATE
   use module_type_vegetation, only: PHENOLOGY_STATE
   use module_type_waterdist, only: IRRIGATION_PRM, SURF_STORE_PRM
   use module_c_api_anthro_emis_prm, only: anthro_emis_prm_unpack
   use module_c_api_anthro_emis_state, only: anthroemis_state_unpack
   use module_c_api_atm, only: atm_state_unpack
   use module_c_api_building_archetype_prm, only: building_archetype_prm_unpack
   use module_c_api_conductance, only: conductance_prm_unpack
   use module_c_api_ehc_prm, only: ehc_prm_unpack
   use module_c_api_flag, only: flag_state_unpack
   use module_c_api_heat_state, only: heat_state_unpack
   use module_c_api_hydro_state, only: hydro_state_unpack
   use module_c_api_irrigation_prm, only: irrigation_prm_unpack
   use module_c_api_lc_bldg_prm, only: lc_bldg_prm_unpack
   use module_c_api_lc_bsoil_prm, only: lc_bsoil_prm_unpack
   use module_c_api_lc_dectr_prm, only: lc_dectr_prm_unpack
   use module_c_api_lc_evetr_prm, only: lc_evetr_prm_unpack
   use module_c_api_lc_grass_prm, only: lc_grass_prm_unpack
   use module_c_api_lc_paved_prm, only: lc_paved_prm_unpack
   use module_c_api_lc_water_prm, only: lc_water_prm_unpack
   use module_c_api_lumps, only: lumps_prm_unpack
   use module_c_api_nhood, only: nhood_state_unpack
   use module_c_api_ohm, only: ohm_state_unpack
   use module_c_api_phenology, only: phenology_state_unpack
   use module_c_api_roughness, only: roughness_state_unpack
   use module_c_api_snow_prm, only: snow_prm_unpack
   use module_c_api_snow, only: snow_state_unpack
   use module_c_api_solar, only: solar_state_unpack
   use module_c_api_spartacus_layer_prm, only: spartacus_layer_prm_unpack
   use module_c_api_spartacus_prm, only: spartacus_prm_unpack
   use module_c_api_stebbs_prm, only: stebbs_prm_unpack
   use module_c_api_stebbs_state, only: stebbs_state_unpack
   use module_c_api_surf_store, only: surf_store_prm_unpack

   implicit none
   private

   integer(c_int), parameter :: OBS_KIND_REAL = 1_c_int
   integer(c_int), parameter :: OBS_KIND_INT = 2_c_int
   integer(c_int), parameter :: OBS_KIND_LOGICAL = 3_c_int

   public :: suews_obs_unpack_anthro_emis_prm
   public :: suews_obs_unpack_anthroemis_state
   public :: suews_obs_unpack_atm_state
   public :: suews_obs_unpack_building_archetype_prm
   public :: suews_obs_unpack_conductance_prm
   public :: suews_obs_unpack_ehc_prm
   public :: suews_obs_unpack_flag_state
   public :: suews_obs_unpack_heat_state
   public :: suews_obs_unpack_hydro_state
   public :: suews_obs_unpack_irrigation_prm
   public :: suews_obs_unpack_lc_bldg_prm
   public :: suews_obs_unpack_lc_bsoil_prm
   public :: suews_obs_unpack_lc_dectr_prm
   public :: suews_obs_unpack_lc_evetr_prm
   public :: suews_obs_unpack_lc_grass_prm
   public :: suews_obs_unpack_lc_paved_prm
   public :: suews_obs_unpack_lc_water_prm
   public :: suews_obs_unpack_lumps_prm
   public :: suews_obs_unpack_nhood_state
   public :: suews_obs_unpack_ohm_state
   public :: suews_obs_unpack_phenology_state
   public :: suews_obs_unpack_roughness_state
   public :: suews_obs_unpack_snow_prm
   public :: suews_obs_unpack_snow_state
   public :: suews_obs_unpack_solar_state
   public :: suews_obs_unpack_spartacus_layer_prm
   public :: suews_obs_unpack_spartacus_prm
   public :: suews_obs_unpack_stebbs_prm
   public :: suews_obs_unpack_stebbs_state
   public :: suews_obs_unpack_surf_store_prm

contains

   subroutine obs_emit_unallocated(fid, out_meta, n_meta, err)
      implicit none
      integer(c_int), intent(in) :: fid
      integer(c_int), intent(in) :: n_meta
      integer(c_int), intent(inout) :: out_meta(5, n_meta)
      integer(c_int), intent(inout) :: err
      if (err /= SUEWS_CAPI_OK) return
      if (fid < 1_c_int .or. fid > n_meta) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      out_meta(1, fid) = 0_c_int
      out_meta(2, fid) = 0_c_int
      out_meta(3, fid) = 0_c_int
      out_meta(4, fid) = 0_c_int
      out_meta(5, fid) = 0_c_int
   end subroutine obs_emit_unallocated

   subroutine obs_emit_alloc_bounds_1d(fid, lb1, ub1, out_meta, n_meta, err)
      implicit none
      integer(c_int), intent(in) :: fid
      integer(c_int), intent(in) :: lb1
      integer(c_int), intent(in) :: ub1
      integer(c_int), intent(in) :: n_meta
      integer(c_int), intent(inout) :: out_meta(5, n_meta)
      integer(c_int), intent(inout) :: err
      if (err /= SUEWS_CAPI_OK) return
      if (fid < 1_c_int .or. fid > n_meta) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      out_meta(1, fid) = 1_c_int
      out_meta(2, fid) = lb1
      out_meta(3, fid) = ub1
      out_meta(4, fid) = 0_c_int
      out_meta(5, fid) = 0_c_int
   end subroutine obs_emit_alloc_bounds_1d

   subroutine obs_emit_real_scalar( &
      fid, val, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, &
      out_meta, n_meta, cap, pos, err)
      implicit none
      integer(c_int), intent(in) :: fid
      real(kind(1d0)), intent(in) :: val
      integer(c_int), intent(inout) :: out_fid(*)
      integer(c_int), intent(inout) :: out_kind(*)
      integer(c_int), intent(inout) :: out_i1(*)
      integer(c_int), intent(inout) :: out_i2(*)
      real(c_double), intent(inout) :: out_f64(*)
      integer(c_int64_t), intent(inout) :: out_i64(*)
      integer(c_int), intent(in) :: n_meta
      integer(c_int), intent(inout) :: out_meta(5, n_meta)
      integer(c_int), intent(in) :: cap
      integer(c_int), intent(inout) :: pos
      integer(c_int), intent(inout) :: err
      if (err /= SUEWS_CAPI_OK) return
      if (fid < 1_c_int .or. fid > n_meta .or. pos >= cap) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      out_meta(1, fid) = 1_c_int
      out_meta(2, fid) = 0_c_int
      out_meta(3, fid) = 0_c_int
      out_meta(4, fid) = 0_c_int
      out_meta(5, fid) = 0_c_int
      pos = pos + 1_c_int
      out_fid(pos) = fid
      out_kind(pos) = OBS_KIND_REAL
      out_i1(pos) = 0_c_int
      out_i2(pos) = 0_c_int
      out_f64(pos) = real(val, c_double)
      out_i64(pos) = 0_c_int64_t
   end subroutine obs_emit_real_scalar

   subroutine obs_emit_int_scalar( &
      fid, val, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, &
      out_meta, n_meta, cap, pos, err)
      implicit none
      integer(c_int), intent(in) :: fid
      integer, intent(in) :: val
      integer(c_int), intent(inout) :: out_fid(*)
      integer(c_int), intent(inout) :: out_kind(*)
      integer(c_int), intent(inout) :: out_i1(*)
      integer(c_int), intent(inout) :: out_i2(*)
      real(c_double), intent(inout) :: out_f64(*)
      integer(c_int64_t), intent(inout) :: out_i64(*)
      integer(c_int), intent(in) :: n_meta
      integer(c_int), intent(inout) :: out_meta(5, n_meta)
      integer(c_int), intent(in) :: cap
      integer(c_int), intent(inout) :: pos
      integer(c_int), intent(inout) :: err
      if (err /= SUEWS_CAPI_OK) return
      if (fid < 1_c_int .or. fid > n_meta .or. pos >= cap) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      out_meta(1, fid) = 1_c_int
      out_meta(2, fid) = 0_c_int
      out_meta(3, fid) = 0_c_int
      out_meta(4, fid) = 0_c_int
      out_meta(5, fid) = 0_c_int
      pos = pos + 1_c_int
      out_fid(pos) = fid
      out_kind(pos) = OBS_KIND_INT
      out_i1(pos) = 0_c_int
      out_i2(pos) = 0_c_int
      out_f64(pos) = 0.0_c_double
      out_i64(pos) = int(val, c_int64_t)
   end subroutine obs_emit_int_scalar

   subroutine obs_emit_logical_scalar( &
      fid, val, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, &
      out_meta, n_meta, cap, pos, err)
      implicit none
      integer(c_int), intent(in) :: fid
      logical, intent(in) :: val
      integer(c_int), intent(inout) :: out_fid(*)
      integer(c_int), intent(inout) :: out_kind(*)
      integer(c_int), intent(inout) :: out_i1(*)
      integer(c_int), intent(inout) :: out_i2(*)
      real(c_double), intent(inout) :: out_f64(*)
      integer(c_int64_t), intent(inout) :: out_i64(*)
      integer(c_int), intent(in) :: n_meta
      integer(c_int), intent(inout) :: out_meta(5, n_meta)
      integer(c_int), intent(in) :: cap
      integer(c_int), intent(inout) :: pos
      integer(c_int), intent(inout) :: err
      if (err /= SUEWS_CAPI_OK) return
      if (fid < 1_c_int .or. fid > n_meta .or. pos >= cap) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      out_meta(1, fid) = 1_c_int
      out_meta(2, fid) = 0_c_int
      out_meta(3, fid) = 0_c_int
      out_meta(4, fid) = 0_c_int
      out_meta(5, fid) = 0_c_int
      pos = pos + 1_c_int
      out_fid(pos) = fid
      out_kind(pos) = OBS_KIND_LOGICAL
      out_i1(pos) = 0_c_int
      out_i2(pos) = 0_c_int
      out_f64(pos) = 0.0_c_double
      out_i64(pos) = merge(1_c_int64_t, 0_c_int64_t, val)
   end subroutine obs_emit_logical_scalar

   subroutine obs_emit_real_1d( &
      fid, arr, lb1, ub1, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, &
      out_meta, n_meta, cap, pos, err)
      implicit none
      integer(c_int), intent(in) :: fid
      integer(c_int), intent(in) :: lb1
      integer(c_int), intent(in) :: ub1
      real(kind(1d0)), intent(in) :: arr(lb1:ub1)
      integer(c_int), intent(inout) :: out_fid(*)
      integer(c_int), intent(inout) :: out_kind(*)
      integer(c_int), intent(inout) :: out_i1(*)
      integer(c_int), intent(inout) :: out_i2(*)
      real(c_double), intent(inout) :: out_f64(*)
      integer(c_int64_t), intent(inout) :: out_i64(*)
      integer(c_int), intent(in) :: n_meta
      integer(c_int), intent(inout) :: out_meta(5, n_meta)
      integer(c_int), intent(in) :: cap
      integer(c_int), intent(inout) :: pos
      integer(c_int), intent(inout) :: err
      integer(c_int) :: i
      if (err /= SUEWS_CAPI_OK) return
      if (fid < 1_c_int .or. fid > n_meta) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      out_meta(1, fid) = 1_c_int
      out_meta(2, fid) = lb1
      out_meta(3, fid) = ub1
      out_meta(4, fid) = 0_c_int
      out_meta(5, fid) = 0_c_int
      do i = lb1, ub1
         if (pos >= cap) then
            err = SUEWS_CAPI_BAD_BUFFER
            return
         end if
         pos = pos + 1_c_int
         out_fid(pos) = fid
         out_kind(pos) = OBS_KIND_REAL
         out_i1(pos) = i
         out_i2(pos) = 0_c_int
         out_f64(pos) = real(arr(i), c_double)
         out_i64(pos) = 0_c_int64_t
      end do
   end subroutine obs_emit_real_1d

   subroutine obs_emit_real_2d( &
      fid, arr, lb1, ub1, lb2, ub2, out_fid, out_kind, out_i1, out_i2, &
      out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      implicit none
      integer(c_int), intent(in) :: fid
      integer(c_int), intent(in) :: lb1
      integer(c_int), intent(in) :: ub1
      integer(c_int), intent(in) :: lb2
      integer(c_int), intent(in) :: ub2
      real(kind(1d0)), intent(in) :: arr(lb1:ub1, lb2:ub2)
      integer(c_int), intent(inout) :: out_fid(*)
      integer(c_int), intent(inout) :: out_kind(*)
      integer(c_int), intent(inout) :: out_i1(*)
      integer(c_int), intent(inout) :: out_i2(*)
      real(c_double), intent(inout) :: out_f64(*)
      integer(c_int64_t), intent(inout) :: out_i64(*)
      integer(c_int), intent(in) :: n_meta
      integer(c_int), intent(inout) :: out_meta(5, n_meta)
      integer(c_int), intent(in) :: cap
      integer(c_int), intent(inout) :: pos
      integer(c_int), intent(inout) :: err
      integer(c_int) :: i
      integer(c_int) :: j
      if (err /= SUEWS_CAPI_OK) return
      if (fid < 1_c_int .or. fid > n_meta) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      out_meta(1, fid) = 1_c_int
      out_meta(2, fid) = lb1
      out_meta(3, fid) = ub1
      out_meta(4, fid) = lb2
      out_meta(5, fid) = ub2
      do i = lb1, ub1
         do j = lb2, ub2
            if (pos >= cap) then
               err = SUEWS_CAPI_BAD_BUFFER
               return
            end if
            pos = pos + 1_c_int
            out_fid(pos) = fid
            out_kind(pos) = OBS_KIND_REAL
            out_i1(pos) = i
            out_i2(pos) = j
            out_f64(pos) = real(arr(i, j), c_double)
            out_i64(pos) = 0_c_int64_t
         end do
      end do
   end subroutine obs_emit_real_2d

   subroutine suews_obs_unpack_anthro_emis_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_anthro_emis_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(anthroEMIS_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 43_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call anthro_emis_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_int_scalar(1_c_int, actual%start_dls, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(2_c_int, actual%end_dls, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%anthro_heat%qf0_beu_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%anthro_heat%qf0_beu_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%anthro_heat%qf_a_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%anthro_heat%qf_a_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%anthro_heat%qf_b_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%anthro_heat%qf_b_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%anthro_heat%qf_c_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%anthro_heat%qf_c_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%anthro_heat%baset_cooling_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%anthro_heat%baset_cooling_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%anthro_heat%baset_heating_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%anthro_heat%baset_heating_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%anthro_heat%ah_min_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%anthro_heat%ah_min_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%anthro_heat%ah_slope_cooling_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%anthro_heat%ah_slope_cooling_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%anthro_heat%ah_slope_heating_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%anthro_heat%ah_slope_heating_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(21_c_int, actual%anthro_heat%ahprof_24hr_working, int(lbound(actual%anthro_heat%ahprof_24hr_working, 1), c_int), int(ubound(actual%anthro_heat%ahprof_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(22_c_int, actual%anthro_heat%ahprof_24hr_holiday, int(lbound(actual%anthro_heat%ahprof_24hr_holiday, 1), c_int), int(ubound(actual%anthro_heat%ahprof_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%anthro_heat%pop_density_daytime_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%anthro_heat%pop_density_daytime_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%anthro_heat%pop_density_nighttime, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(26_c_int, actual%anthro_heat%popprof_24hr_working, int(lbound(actual%anthro_heat%popprof_24hr_working, 1), c_int), int(ubound(actual%anthro_heat%popprof_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(27_c_int, actual%anthro_heat%popprof_24hr_holiday, int(lbound(actual%anthro_heat%popprof_24hr_holiday, 1), c_int), int(ubound(actual%anthro_heat%popprof_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%ef_umol_co2_per_j, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%en_ef_v_jkm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%fr_fossil_fuel_heat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%fr_fossil_fuel_non_heat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(32_c_int, actual%fc_ef_v_kgkm, int(lbound(actual%fc_ef_v_kgkm, 1), c_int), int(ubound(actual%fc_ef_v_kgkm, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(33_c_int, actual%hum_activity_24hr_working, int(lbound(actual%hum_activity_24hr_working, 1), c_int), int(ubound(actual%hum_activity_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(34_c_int, actual%hum_activity_24hr_holiday, int(lbound(actual%hum_activity_24hr_holiday, 1), c_int), int(ubound(actual%hum_activity_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(35_c_int, actual%max_fc_metab, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%max_qf_metab, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(37_c_int, actual%min_fc_metab, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(38_c_int, actual%min_qf_metab, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(39_c_int, actual%traffic_rate_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(40_c_int, actual%traffic_rate_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(41_c_int, actual%traffic_units, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(42_c_int, actual%traff_prof_24hr_working, int(lbound(actual%traff_prof_24hr_working, 1), c_int), int(ubound(actual%traff_prof_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(43_c_int, actual%traff_prof_24hr_holiday, int(lbound(actual%traff_prof_24hr_holiday, 1), c_int), int(ubound(actual%traff_prof_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_anthro_emis_prm

   subroutine suews_obs_unpack_anthroemis_state( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_anthroemis_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(anthroEmis_STATE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 11_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call anthroemis_state_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_1d(1_c_int, actual%HDD_id, int(lbound(actual%HDD_id, 1), c_int), int(ubound(actual%HDD_id, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%Fc, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%Fc_anthro, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%Fc_biogen, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%Fc_build, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%Fc_metab, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%Fc_photo, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%Fc_point, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%Fc_respi, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%Fc_traff, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(11_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_anthroemis_state

   subroutine suews_obs_unpack_atm_state( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_atm_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(atm_state) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 33_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call atm_state_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%f_cloud, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%av_cp, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%dens_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%av_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%dq, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%Ea_hPa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%Es_hPa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%lv_J_kg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%lv_s_j_kg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%tlv, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%psyc_hPa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%psyc_ice_h_pa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%s_Pa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%s_hpa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%s_ice_hpa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%vpd_hPa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%vpd_pa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%U10_ms, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%U_hbh, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%T2_C, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%T_half_bldg_C, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%q2_gkg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%RH2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%L_mod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%z_l, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%RA_h, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%RS, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%u_star, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%t_star, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%RB, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%Tair_av, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(32_c_int, actual%rss_surf, int(lbound(actual%rss_surf, 1), c_int), int(ubound(actual%rss_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(33_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_atm_state

   subroutine suews_obs_unpack_building_archetype_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_building_archetype_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(BUILDING_ARCHETYPE_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 69_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call building_archetype_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%building_count, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%occupants_state, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%hhs0, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%age_0_4, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%age_5_11, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%age_12_18, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%age_19_64, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%age_65plus, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%stebbs_Height, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%footprint_area, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%wall_external_area, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%internal_volume_ratio, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%internal_mass_area, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%WWR, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%wall_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%wall_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%wall_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%wall_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%wall_external_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%wall_external_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%wall_external_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%wall_external_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%wall_outer_heat_capacity_fraction, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%wall_external_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%wall_internal_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%wall_transmissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%wall_absorptivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%wall_reflectivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%roof_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%roof_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%roof_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%roof_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%roof_external_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%roof_external_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(35_c_int, actual%roof_external_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%roof_external_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(37_c_int, actual%roof_outer_heat_capacity_fraction, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(38_c_int, actual%roof_external_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(39_c_int, actual%roof_internal_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(40_c_int, actual%roof_transmissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(41_c_int, actual%roof_absorptivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(42_c_int, actual%roof_reflectivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(43_c_int, actual%ground_floor_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(44_c_int, actual%ground_floor_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(45_c_int, actual%ground_floor_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(46_c_int, actual%ground_floor_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(47_c_int, actual%window_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(48_c_int, actual%window_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(49_c_int, actual%window_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(50_c_int, actual%window_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(51_c_int, actual%window_external_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(52_c_int, actual%window_internal_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(53_c_int, actual%window_transmissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(54_c_int, actual%window_absorptivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(55_c_int, actual%window_reflectivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(56_c_int, actual%internal_mass_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(57_c_int, actual%internal_mass_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(58_c_int, actual%internal_mass_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(59_c_int, actual%max_heating_power, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(60_c_int, actual%hot_water_tank_volume, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(61_c_int, actual%maximum_hot_water_heating_power, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(62_c_int, actual%heating_setpoint_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(63_c_int, actual%cooling_setpoint_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(64_c_int, actual%heating_setpoint_temperature_profile, int(lbound(actual%heating_setpoint_temperature_profile, 1), c_int), int(ubound(actual%heating_setpoint_temperature_profile, 1), c_int), int(lbound(actual%heating_setpoint_temperature_profile, 2), c_int), int(ubound(actual%heating_setpoint_temperature_profile, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(65_c_int, actual%cooling_setpoint_temperature_profile, int(lbound(actual%cooling_setpoint_temperature_profile, 1), c_int), int(ubound(actual%cooling_setpoint_temperature_profile, 1), c_int), int(lbound(actual%cooling_setpoint_temperature_profile, 2), c_int), int(ubound(actual%cooling_setpoint_temperature_profile, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(66_c_int, actual%lighting_power_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(67_c_int, actual%metabolism_profile, int(lbound(actual%metabolism_profile, 1), c_int), int(ubound(actual%metabolism_profile, 1), c_int), int(lbound(actual%metabolism_profile, 2), c_int), int(ubound(actual%metabolism_profile, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(68_c_int, actual%appliance_profile, int(lbound(actual%appliance_profile, 1), c_int), int(ubound(actual%appliance_profile, 1), c_int), int(lbound(actual%appliance_profile, 2), c_int), int(ubound(actual%appliance_profile, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(69_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_building_archetype_prm

   subroutine suews_obs_unpack_conductance_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_conductance_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(CONDUCTANCE_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 12_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call conductance_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%g_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%g_k, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%g_q_base, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%g_q_shape, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%g_t, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%g_sm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%k_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(8_c_int, actual%gs_model, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%s1, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%s2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%TH, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%TL, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_conductance_prm

   subroutine suews_obs_unpack_ehc_prm( &
      flat, n_flat, nlayer, ndepth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_ehc_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), value, intent(in) :: nlayer
      integer(c_int), value, intent(in) :: ndepth
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(EHC_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 18_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call ehc_prm_unpack(flat, n_flat, nlayer, ndepth, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      if (allocated(actual%soil_store_capacity_roof)) then
         call obs_emit_real_1d(1_c_int, actual%soil_store_capacity_roof, int(lbound(actual%soil_store_capacity_roof, 1), c_int), int(ubound(actual%soil_store_capacity_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(1_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%soil_store_capacity_wall)) then
         call obs_emit_real_1d(2_c_int, actual%soil_store_capacity_wall, int(lbound(actual%soil_store_capacity_wall, 1), c_int), int(ubound(actual%soil_store_capacity_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(2_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%state_limit_roof)) then
         call obs_emit_real_1d(3_c_int, actual%state_limit_roof, int(lbound(actual%state_limit_roof, 1), c_int), int(ubound(actual%state_limit_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(3_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%state_limit_wall)) then
         call obs_emit_real_1d(4_c_int, actual%state_limit_wall, int(lbound(actual%state_limit_wall, 1), c_int), int(ubound(actual%state_limit_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(4_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%wet_thresh_roof)) then
         call obs_emit_real_1d(5_c_int, actual%wet_thresh_roof, int(lbound(actual%wet_thresh_roof, 1), c_int), int(ubound(actual%wet_thresh_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(5_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%wet_thresh_wall)) then
         call obs_emit_real_1d(6_c_int, actual%wet_thresh_wall, int(lbound(actual%wet_thresh_wall, 1), c_int), int(ubound(actual%wet_thresh_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(6_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%tin_roof)) then
         call obs_emit_real_1d(7_c_int, actual%tin_roof, int(lbound(actual%tin_roof, 1), c_int), int(ubound(actual%tin_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(7_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%tin_wall)) then
         call obs_emit_real_1d(8_c_int, actual%tin_wall, int(lbound(actual%tin_wall, 1), c_int), int(ubound(actual%tin_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(8_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%tin_surf)) then
         call obs_emit_real_1d(9_c_int, actual%tin_surf, int(lbound(actual%tin_surf, 1), c_int), int(ubound(actual%tin_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(9_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%k_roof)) then
         call obs_emit_real_2d(10_c_int, actual%k_roof, int(lbound(actual%k_roof, 1), c_int), int(ubound(actual%k_roof, 1), c_int), int(lbound(actual%k_roof, 2), c_int), int(ubound(actual%k_roof, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(10_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%k_wall)) then
         call obs_emit_real_2d(11_c_int, actual%k_wall, int(lbound(actual%k_wall, 1), c_int), int(ubound(actual%k_wall, 1), c_int), int(lbound(actual%k_wall, 2), c_int), int(ubound(actual%k_wall, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(11_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%k_surf)) then
         call obs_emit_real_2d(12_c_int, actual%k_surf, int(lbound(actual%k_surf, 1), c_int), int(ubound(actual%k_surf, 1), c_int), int(lbound(actual%k_surf, 2), c_int), int(ubound(actual%k_surf, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(12_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%cp_roof)) then
         call obs_emit_real_2d(13_c_int, actual%cp_roof, int(lbound(actual%cp_roof, 1), c_int), int(ubound(actual%cp_roof, 1), c_int), int(lbound(actual%cp_roof, 2), c_int), int(ubound(actual%cp_roof, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(13_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%cp_wall)) then
         call obs_emit_real_2d(14_c_int, actual%cp_wall, int(lbound(actual%cp_wall, 1), c_int), int(ubound(actual%cp_wall, 1), c_int), int(lbound(actual%cp_wall, 2), c_int), int(ubound(actual%cp_wall, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(14_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%cp_surf)) then
         call obs_emit_real_2d(15_c_int, actual%cp_surf, int(lbound(actual%cp_surf, 1), c_int), int(ubound(actual%cp_surf, 1), c_int), int(lbound(actual%cp_surf, 2), c_int), int(ubound(actual%cp_surf, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(15_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%dz_roof)) then
         call obs_emit_real_2d(16_c_int, actual%dz_roof, int(lbound(actual%dz_roof, 1), c_int), int(ubound(actual%dz_roof, 1), c_int), int(lbound(actual%dz_roof, 2), c_int), int(ubound(actual%dz_roof, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(16_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%dz_wall)) then
         call obs_emit_real_2d(17_c_int, actual%dz_wall, int(lbound(actual%dz_wall, 1), c_int), int(ubound(actual%dz_wall, 1), c_int), int(lbound(actual%dz_wall, 2), c_int), int(ubound(actual%dz_wall, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(17_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%dz_surf)) then
         call obs_emit_real_2d(18_c_int, actual%dz_surf, int(lbound(actual%dz_surf, 1), c_int), int(ubound(actual%dz_surf, 1), c_int), int(lbound(actual%dz_surf, 2), c_int), int(ubound(actual%dz_surf, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(18_c_int, out_meta, n_meta, err)
      end if

      n_out = pos
   end subroutine suews_obs_unpack_ehc_prm

   subroutine suews_obs_unpack_flag_state( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_flag_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(flag_STATE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 5_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call flag_state_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_logical_scalar(1_c_int, actual%flag_converge, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(2_c_int, actual%i_iter, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(3_c_int, actual%stebbs_bldg_init, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(4_c_int, actual%snow_warning_shown, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(5_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_flag_state

   subroutine suews_obs_unpack_heat_state( &
      flat, n_flat, nlayer, ndepth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_heat_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), value, intent(in) :: nlayer
      integer(c_int), value, intent(in) :: ndepth
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(HEAT_STATE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 56_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call heat_state_unpack(flat, n_flat, nlayer, ndepth, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      if (allocated(actual%temp_roof)) then
         call obs_emit_real_2d(1_c_int, actual%temp_roof, int(lbound(actual%temp_roof, 1), c_int), int(ubound(actual%temp_roof, 1), c_int), int(lbound(actual%temp_roof, 2), c_int), int(ubound(actual%temp_roof, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(1_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%temp_wall)) then
         call obs_emit_real_2d(2_c_int, actual%temp_wall, int(lbound(actual%temp_wall, 1), c_int), int(ubound(actual%temp_wall, 1), c_int), int(lbound(actual%temp_wall, 2), c_int), int(ubound(actual%temp_wall, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(2_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%temp_surf)) then
         call obs_emit_real_2d(3_c_int, actual%temp_surf, int(lbound(actual%temp_surf, 1), c_int), int(ubound(actual%temp_surf, 1), c_int), int(lbound(actual%temp_surf, 2), c_int), int(ubound(actual%temp_surf, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(3_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%temp_surf_dyohm)) then
         call obs_emit_real_2d(4_c_int, actual%temp_surf_dyohm, int(lbound(actual%temp_surf_dyohm, 1), c_int), int(ubound(actual%temp_surf_dyohm, 1), c_int), int(lbound(actual%temp_surf_dyohm, 2), c_int), int(ubound(actual%temp_surf_dyohm, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(4_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%temp_surf_ehc_fast)) then
         call obs_emit_real_2d(5_c_int, actual%temp_surf_ehc_fast, int(lbound(actual%temp_surf_ehc_fast, 1), c_int), int(ubound(actual%temp_surf_ehc_fast, 1), c_int), int(lbound(actual%temp_surf_ehc_fast, 2), c_int), int(ubound(actual%temp_surf_ehc_fast, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(5_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%temp_surf_ehc_slow)) then
         call obs_emit_real_2d(6_c_int, actual%temp_surf_ehc_slow, int(lbound(actual%temp_surf_ehc_slow, 1), c_int), int(ubound(actual%temp_surf_ehc_slow, 1), c_int), int(lbound(actual%temp_surf_ehc_slow, 2), c_int), int(ubound(actual%temp_surf_ehc_slow, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(6_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%tsfc_roof)) then
         call obs_emit_real_1d(7_c_int, actual%tsfc_roof, int(lbound(actual%tsfc_roof, 1), c_int), int(ubound(actual%tsfc_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(7_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%tsfc_wall)) then
         call obs_emit_real_1d(8_c_int, actual%tsfc_wall, int(lbound(actual%tsfc_wall, 1), c_int), int(ubound(actual%tsfc_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(8_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%tsfc_surf)) then
         call obs_emit_real_1d(9_c_int, actual%tsfc_surf, int(lbound(actual%tsfc_surf, 1), c_int), int(ubound(actual%tsfc_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(9_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%tsfc_surf_dyohm)) then
         call obs_emit_real_1d(10_c_int, actual%tsfc_surf_dyohm, int(lbound(actual%tsfc_surf_dyohm, 1), c_int), int(ubound(actual%tsfc_surf_dyohm, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(10_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%tsfc_roof_stepstart)) then
         call obs_emit_real_1d(11_c_int, actual%tsfc_roof_stepstart, int(lbound(actual%tsfc_roof_stepstart, 1), c_int), int(ubound(actual%tsfc_roof_stepstart, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(11_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%tsfc_wall_stepstart)) then
         call obs_emit_real_1d(12_c_int, actual%tsfc_wall_stepstart, int(lbound(actual%tsfc_wall_stepstart, 1), c_int), int(ubound(actual%tsfc_wall_stepstart, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(12_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%tsfc_surf_stepstart)) then
         call obs_emit_real_1d(13_c_int, actual%tsfc_surf_stepstart, int(lbound(actual%tsfc_surf_stepstart, 1), c_int), int(ubound(actual%tsfc_surf_stepstart, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(13_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%QS_roof)) then
         call obs_emit_real_1d(14_c_int, actual%QS_roof, int(lbound(actual%QS_roof, 1), c_int), int(ubound(actual%QS_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(14_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%QN_roof)) then
         call obs_emit_real_1d(15_c_int, actual%QN_roof, int(lbound(actual%QN_roof, 1), c_int), int(ubound(actual%QN_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(15_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%qe_roof)) then
         call obs_emit_real_1d(16_c_int, actual%qe_roof, int(lbound(actual%qe_roof, 1), c_int), int(ubound(actual%qe_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(16_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%qh_roof)) then
         call obs_emit_real_1d(17_c_int, actual%qh_roof, int(lbound(actual%qh_roof, 1), c_int), int(ubound(actual%qh_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(17_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%qh_resist_roof)) then
         call obs_emit_real_1d(18_c_int, actual%qh_resist_roof, int(lbound(actual%qh_resist_roof, 1), c_int), int(ubound(actual%qh_resist_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(18_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%QS_wall)) then
         call obs_emit_real_1d(19_c_int, actual%QS_wall, int(lbound(actual%QS_wall, 1), c_int), int(ubound(actual%QS_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(19_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%QN_wall)) then
         call obs_emit_real_1d(20_c_int, actual%QN_wall, int(lbound(actual%QN_wall, 1), c_int), int(ubound(actual%QN_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(20_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%qe_wall)) then
         call obs_emit_real_1d(21_c_int, actual%qe_wall, int(lbound(actual%qe_wall, 1), c_int), int(ubound(actual%qe_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(21_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%qh_wall)) then
         call obs_emit_real_1d(22_c_int, actual%qh_wall, int(lbound(actual%qh_wall, 1), c_int), int(ubound(actual%qh_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(22_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%qh_resist_wall)) then
         call obs_emit_real_1d(23_c_int, actual%qh_resist_wall, int(lbound(actual%qh_resist_wall, 1), c_int), int(ubound(actual%qh_resist_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(23_c_int, out_meta, n_meta, err)
      end if
      call obs_emit_real_1d(24_c_int, actual%qs_surf, int(lbound(actual%qs_surf, 1), c_int), int(ubound(actual%qs_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(25_c_int, actual%QN_surf, int(lbound(actual%QN_surf, 1), c_int), int(ubound(actual%QN_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(26_c_int, actual%qe0_surf, int(lbound(actual%qe0_surf, 1), c_int), int(ubound(actual%qe0_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(27_c_int, actual%qe_surf, int(lbound(actual%qe_surf, 1), c_int), int(ubound(actual%qe_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(28_c_int, actual%qh_surf, int(lbound(actual%qh_surf, 1), c_int), int(ubound(actual%qh_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(29_c_int, actual%qh_resist_surf, int(lbound(actual%qh_resist_surf, 1), c_int), int(ubound(actual%qh_resist_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(30_c_int, actual%tsurf_ind, int(lbound(actual%tsurf_ind, 1), c_int), int(ubound(actual%tsurf_ind, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%QH_LUMPS, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%QE_LUMPS, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%k_clear, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%k_up, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(35_c_int, actual%l_down, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%l_up, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(37_c_int, actual%qe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(38_c_int, actual%qf, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(39_c_int, actual%QF_SAHP, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(40_c_int, actual%qh, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(41_c_int, actual%qh_residual, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(42_c_int, actual%qh_resist, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(43_c_int, actual%ehc_iter_dif_tsfc, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(44_c_int, actual%ehc_iter_dif_qh, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(45_c_int, actual%ehc_iter_score, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(46_c_int, actual%qn, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(47_c_int, actual%qn_snowfree, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(48_c_int, actual%qs, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(49_c_int, actual%TSfc_C, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(50_c_int, actual%tsurf, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(51_c_int, actual%QH_Init, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(52_c_int, actual%roof_in_sw_spc, int(lbound(actual%roof_in_sw_spc, 1), c_int), int(ubound(actual%roof_in_sw_spc, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(53_c_int, actual%roof_in_lw_spc, int(lbound(actual%roof_in_lw_spc, 1), c_int), int(ubound(actual%roof_in_lw_spc, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(54_c_int, actual%wall_in_sw_spc, int(lbound(actual%wall_in_sw_spc, 1), c_int), int(ubound(actual%wall_in_sw_spc, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(55_c_int, actual%wall_in_lw_spc, int(lbound(actual%wall_in_lw_spc, 1), c_int), int(ubound(actual%wall_in_lw_spc, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(56_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_heat_state

   subroutine suews_obs_unpack_hydro_state( &
      flat, n_flat, nlayer, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_hydro_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), value, intent(in) :: nlayer
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(HYDRO_STATE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 42_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call hydro_state_unpack(flat, n_flat, nlayer, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_1d(1_c_int, actual%soil_store_surf, int(lbound(actual%soil_store_surf, 1), c_int), int(ubound(actual%soil_store_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(2_c_int, actual%state_surf, int(lbound(actual%state_surf, 1), c_int), int(ubound(actual%state_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(3_c_int, actual%WUDay_id, int(lbound(actual%WUDay_id, 1), c_int), int(ubound(actual%WUDay_id, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      if (allocated(actual%soil_store_roof)) then
         call obs_emit_real_1d(4_c_int, actual%soil_store_roof, int(lbound(actual%soil_store_roof, 1), c_int), int(ubound(actual%soil_store_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(4_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%state_roof)) then
         call obs_emit_real_1d(5_c_int, actual%state_roof, int(lbound(actual%state_roof, 1), c_int), int(ubound(actual%state_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(5_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%soil_store_wall)) then
         call obs_emit_real_1d(6_c_int, actual%soil_store_wall, int(lbound(actual%soil_store_wall, 1), c_int), int(ubound(actual%soil_store_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(6_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%state_wall)) then
         call obs_emit_real_1d(7_c_int, actual%state_wall, int(lbound(actual%state_wall, 1), c_int), int(ubound(actual%state_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(7_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ev_roof)) then
         call obs_emit_real_1d(8_c_int, actual%ev_roof, int(lbound(actual%ev_roof, 1), c_int), int(ubound(actual%ev_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(8_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ev_wall)) then
         call obs_emit_real_1d(9_c_int, actual%ev_wall, int(lbound(actual%ev_wall, 1), c_int), int(ubound(actual%ev_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(9_c_int, out_meta, n_meta, err)
      end if
      call obs_emit_real_1d(10_c_int, actual%ev0_surf, int(lbound(actual%ev0_surf, 1), c_int), int(ubound(actual%ev0_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(11_c_int, actual%ev_surf, int(lbound(actual%ev_surf, 1), c_int), int(ubound(actual%ev_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(12_c_int, actual%wu_surf, int(lbound(actual%wu_surf, 1), c_int), int(ubound(actual%wu_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(13_c_int, actual%runoff_soil, int(lbound(actual%runoff_soil, 1), c_int), int(ubound(actual%runoff_soil, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(14_c_int, actual%smd_surf, int(lbound(actual%smd_surf, 1), c_int), int(ubound(actual%smd_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(15_c_int, actual%drain_surf, int(lbound(actual%drain_surf, 1), c_int), int(ubound(actual%drain_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%drain_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%ev_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%wu_ext, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%wu_int, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%runoff_ag_veg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%runoff_ag_impervious, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%runoff_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%runoff_pipes, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%runoff_soil_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%runoff_waterbody, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%smd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%soil_state, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%state_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%surf_chang_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%tot_chang_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%runoff_per_interval, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%nw_state_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%soil_moist_cap, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%vsmd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(35_c_int, actual%additional_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%add_impervious, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(37_c_int, actual%add_pipes, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(38_c_int, actual%add_veg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(39_c_int, actual%add_water_body, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(40_c_int, actual%add_water, int(lbound(actual%add_water, 1), c_int), int(ubound(actual%add_water, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(41_c_int, actual%frac_water2runoff, int(lbound(actual%frac_water2runoff, 1), c_int), int(ubound(actual%frac_water2runoff, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(42_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_hydro_state

   subroutine suews_obs_unpack_irrigation_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_irrigation_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(IRRIGATION_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 25_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call irrigation_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%h_maintain, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%f_aut, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(3_c_int, actual%ie_a, int(lbound(actual%ie_a, 1), c_int), int(ubound(actual%ie_a, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(4_c_int, actual%ie_m, int(lbound(actual%ie_m, 1), c_int), int(ubound(actual%ie_m, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(5_c_int, actual%ie_start, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(6_c_int, actual%ie_end, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%internalwateruse_h, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%irr_daywater%monday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%irr_daywater%monday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%irr_daywater%tuesday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%irr_daywater%tuesday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%irr_daywater%wednesday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%irr_daywater%wednesday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%irr_daywater%thursday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%irr_daywater%thursday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%irr_daywater%friday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%irr_daywater%friday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%irr_daywater%saturday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%irr_daywater%saturday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%irr_daywater%sunday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%irr_daywater%sunday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(22_c_int, actual%wuprofa_24hr_working, int(lbound(actual%wuprofa_24hr_working, 1), c_int), int(ubound(actual%wuprofa_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(23_c_int, actual%wuprofa_24hr_holiday, int(lbound(actual%wuprofa_24hr_holiday, 1), c_int), int(ubound(actual%wuprofa_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(24_c_int, actual%wuprofm_24hr_working, int(lbound(actual%wuprofm_24hr_working, 1), c_int), int(ubound(actual%wuprofm_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(25_c_int, actual%wuprofm_24hr_holiday, int(lbound(actual%wuprofm_24hr_holiday, 1), c_int), int(ubound(actual%wuprofm_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_irrigation_prm

   subroutine suews_obs_unpack_lc_bldg_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_lc_bldg_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(LC_BLDG_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 36_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call lc_bldg_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%fai_building, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%height_building, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%state, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%irrigation_fraction_bldgs, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(35_c_int, actual%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_lc_bldg_prm

   subroutine suews_obs_unpack_lc_bsoil_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_lc_bsoil_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(LC_BSOIL_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 33_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call lc_bsoil_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%irrigation_fraction_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_lc_bsoil_prm

   subroutine suews_obs_unpack_lc_dectr_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_lc_dectr_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(LC_DECTR_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 58_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call lc_dectr_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%fai_deciduous_tree, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%height_deciduous_tree, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%porosity_min_deciduous, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%porosity_max_deciduous, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%alb_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%alb_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%capacity_max_deciduous, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%capacity_min_deciduous, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%irrigation_fraction_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%bioco2%beta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(35_c_int, actual%bioco2%beta_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%bioco2%alpha_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(37_c_int, actual%bioco2%alpha_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(38_c_int, actual%bioco2%resp_a, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(39_c_int, actual%bioco2%resp_b, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(40_c_int, actual%bioco2%theta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(41_c_int, actual%bioco2%min_res_bioCO2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(42_c_int, actual%max_conductance, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(43_c_int, actual%lai%base_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(44_c_int, actual%lai%gdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(45_c_int, actual%lai%base_temperature_senescence, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(46_c_int, actual%lai%sdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(47_c_int, actual%lai%lai_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(48_c_int, actual%lai%lai_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(49_c_int, actual%lai%lai_power, int(lbound(actual%lai%lai_power, 1), c_int), int(ubound(actual%lai%lai_power, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(50_c_int, actual%lai%lai_type, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(51_c_int, actual%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(52_c_int, actual%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(53_c_int, actual%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(54_c_int, actual%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(55_c_int, actual%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(56_c_int, actual%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(57_c_int, actual%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(58_c_int, actual%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_lc_dectr_prm

   subroutine suews_obs_unpack_lc_evetr_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_lc_evetr_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(LC_EVETR_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 54_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call lc_evetr_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%fai_evergreen_tree, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%height_evergreen_tree, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%alb_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%alb_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%irrigation_fraction_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%bioco2%beta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%bioco2%beta_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%bioco2%alpha_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%bioco2%alpha_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%bioco2%resp_a, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(35_c_int, actual%bioco2%resp_b, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%bioco2%theta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(37_c_int, actual%bioco2%min_res_bioCO2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(38_c_int, actual%max_conductance, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(39_c_int, actual%lai%base_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(40_c_int, actual%lai%gdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(41_c_int, actual%lai%base_temperature_senescence, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(42_c_int, actual%lai%sdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(43_c_int, actual%lai%lai_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(44_c_int, actual%lai%lai_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(45_c_int, actual%lai%lai_power, int(lbound(actual%lai%lai_power, 1), c_int), int(ubound(actual%lai%lai_power, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(46_c_int, actual%lai%lai_type, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(47_c_int, actual%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(48_c_int, actual%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(49_c_int, actual%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(50_c_int, actual%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(51_c_int, actual%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(52_c_int, actual%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(53_c_int, actual%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(54_c_int, actual%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_lc_evetr_prm

   subroutine suews_obs_unpack_lc_grass_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_lc_grass_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(LC_GRASS_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 52_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call lc_grass_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%alb_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%alb_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%irrigation_fraction_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%bioco2%beta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%bioco2%beta_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%bioco2%alpha_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%bioco2%alpha_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%bioco2%resp_a, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%bioco2%resp_b, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%bioco2%theta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(35_c_int, actual%bioco2%min_res_bioCO2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%max_conductance, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(37_c_int, actual%lai%base_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(38_c_int, actual%lai%gdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(39_c_int, actual%lai%base_temperature_senescence, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(40_c_int, actual%lai%sdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(41_c_int, actual%lai%lai_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(42_c_int, actual%lai%lai_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(43_c_int, actual%lai%lai_power, int(lbound(actual%lai%lai_power, 1), c_int), int(ubound(actual%lai%lai_power, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(44_c_int, actual%lai%lai_type, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(45_c_int, actual%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(46_c_int, actual%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(47_c_int, actual%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(48_c_int, actual%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(49_c_int, actual%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(50_c_int, actual%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(51_c_int, actual%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(52_c_int, actual%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_lc_grass_prm

   subroutine suews_obs_unpack_lc_paved_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_lc_paved_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(LC_PAVED_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 34_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call lc_paved_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%state, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%irrigation_fraction_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_lc_paved_prm

   subroutine suews_obs_unpack_lc_water_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_lc_water_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(LC_WATER_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 26_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call lc_water_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%irrigation_fraction_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%flow_change, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_lc_water_prm

   subroutine suews_obs_unpack_lumps_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_lumps_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(LUMPS_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 4_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call lumps_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%rain_cover, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%rain_max_res, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%drain_rate, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(4_c_int, actual%veg_type, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_lumps_prm

   subroutine suews_obs_unpack_nhood_state( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_nhood_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(NHOOD_STATE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 5_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call nhood_state_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%U_hbh_1dravg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%QN_1dravg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%Tair_mn_prev, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%iter_count, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(5_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_nhood_state

   subroutine suews_obs_unpack_ohm_state( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_ohm_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(OHM_STATE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 57_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call ohm_state_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%qn_av, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%dqndt, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(3_c_int, actual%qn_surfs, int(lbound(actual%qn_surfs, 1), c_int), int(ubound(actual%qn_surfs, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(4_c_int, actual%dqndt_surf, int(lbound(actual%dqndt_surf, 1), c_int), int(ubound(actual%dqndt_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%qn_s_av, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%dqnsdt, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%a1, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%a2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%a3, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%t2_prev, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%ws_rav, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%tair_prev, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(13_c_int, actual%qn_rav, int(lbound(actual%qn_rav, 1), c_int), int(ubound(actual%qn_rav, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%a1_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%a2_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%a3_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%a1_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%a2_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%a3_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%a1_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%a2_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%a3_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%a1_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%a2_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%a3_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%a1_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%a2_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%a3_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%a1_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%a2_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%a3_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%a1_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%a2_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%a3_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(35_c_int, actual%anohm_working_day, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(36_c_int, actual%anohm_working_count, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(37_c_int, actual%anohm_working_tHr, int(lbound(actual%anohm_working_tHr, 1), c_int), int(ubound(actual%anohm_working_tHr, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(38_c_int, actual%anohm_working_sd, int(lbound(actual%anohm_working_sd, 1), c_int), int(ubound(actual%anohm_working_sd, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(39_c_int, actual%anohm_working_ta, int(lbound(actual%anohm_working_ta, 1), c_int), int(ubound(actual%anohm_working_ta, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(40_c_int, actual%anohm_working_rh, int(lbound(actual%anohm_working_rh, 1), c_int), int(ubound(actual%anohm_working_rh, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(41_c_int, actual%anohm_working_pres, int(lbound(actual%anohm_working_pres, 1), c_int), int(ubound(actual%anohm_working_pres, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(42_c_int, actual%anohm_working_ws, int(lbound(actual%anohm_working_ws, 1), c_int), int(ubound(actual%anohm_working_ws, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(43_c_int, actual%anohm_working_ah, int(lbound(actual%anohm_working_ah, 1), c_int), int(ubound(actual%anohm_working_ah, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(44_c_int, actual%anohm_coeff_day, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(45_c_int, actual%anohm_coeff_count, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(46_c_int, actual%anohm_coeff_ready, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(47_c_int, actual%anohm_coeff_tHr, int(lbound(actual%anohm_coeff_tHr, 1), c_int), int(ubound(actual%anohm_coeff_tHr, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(48_c_int, actual%anohm_coeff_sd, int(lbound(actual%anohm_coeff_sd, 1), c_int), int(ubound(actual%anohm_coeff_sd, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(49_c_int, actual%anohm_coeff_ta, int(lbound(actual%anohm_coeff_ta, 1), c_int), int(ubound(actual%anohm_coeff_ta, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(50_c_int, actual%anohm_coeff_rh, int(lbound(actual%anohm_coeff_rh, 1), c_int), int(ubound(actual%anohm_coeff_rh, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(51_c_int, actual%anohm_coeff_pres, int(lbound(actual%anohm_coeff_pres, 1), c_int), int(ubound(actual%anohm_coeff_pres, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(52_c_int, actual%anohm_coeff_ws, int(lbound(actual%anohm_coeff_ws, 1), c_int), int(ubound(actual%anohm_coeff_ws, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(53_c_int, actual%anohm_coeff_ah, int(lbound(actual%anohm_coeff_ah, 1), c_int), int(ubound(actual%anohm_coeff_ah, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(54_c_int, actual%anohm_a1_surf, int(lbound(actual%anohm_a1_surf, 1), c_int), int(ubound(actual%anohm_a1_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(55_c_int, actual%anohm_a2_surf, int(lbound(actual%anohm_a2_surf, 1), c_int), int(ubound(actual%anohm_a2_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(56_c_int, actual%anohm_a3_surf, int(lbound(actual%anohm_a3_surf, 1), c_int), int(ubound(actual%anohm_a3_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(57_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_ohm_state

   subroutine suews_obs_unpack_phenology_state( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_phenology_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(PHENOLOGY_STATE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 23_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call phenology_state_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_1d(1_c_int, actual%alb, int(lbound(actual%alb, 1), c_int), int(ubound(actual%alb, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(2_c_int, actual%lai_id, int(lbound(actual%lai_id, 1), c_int), int(ubound(actual%lai_id, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(3_c_int, actual%GDD_id, int(lbound(actual%GDD_id, 1), c_int), int(ubound(actual%GDD_id, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(4_c_int, actual%SDD_id, int(lbound(actual%SDD_id, 1), c_int), int(ubound(actual%SDD_id, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%veg_phen_lumps, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%porosity_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%decidcap_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%albDecTr_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%albEveTr_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%albGrass_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%Tmin_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%Tmax_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%lenDay_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%temp_veg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(15_c_int, actual%storage_drain_params, int(lbound(actual%storage_drain_params, 1), c_int), int(ubound(actual%storage_drain_params, 1), c_int), int(lbound(actual%storage_drain_params, 2), c_int), int(ubound(actual%storage_drain_params, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%g_func, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%gsc, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%g_kdown, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%g_dq, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%g_ta, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%g_smd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%g_lai, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(23_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_phenology_state

   subroutine suews_obs_unpack_roughness_state( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_roughness_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(ROUGHNESS_STATE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 11_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call roughness_state_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%fai_bldg_use, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%fai_evetree_use, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%fai_dectree_use, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%FAI, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%PAI, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%Zh, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%z0m, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%z0v, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%zdm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%ZZD, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(11_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_roughness_state

   subroutine suews_obs_unpack_snow_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_snow_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(SNOW_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 19_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call snow_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%water_holding_capacity_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%water_holding_capacity_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%narp_emis_snow, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%temperature_rain_snow_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%precipitation_threshold_albedo_reset, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%snow_albedo_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%snow_albedo_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%snow_density_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%snow_density_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%snow_depth_limit_building, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%snow_depth_limit_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(12_c_int, actual%snowpack_limit, int(lbound(actual%snowpack_limit, 1), c_int), int(ubound(actual%snowpack_limit, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(13_c_int, actual%snowprof_24hr_working, int(lbound(actual%snowprof_24hr_working, 1), c_int), int(ubound(actual%snowprof_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(14_c_int, actual%snowprof_24hr_holiday, int(lbound(actual%snowprof_24hr_holiday, 1), c_int), int(ubound(actual%snowprof_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%tau_a, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%tau_f, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%tau_r, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%temperature_melt_factor, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%radiation_melt_factor, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_snow_prm

   subroutine suews_obs_unpack_snow_state( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_snow_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(SNOW_STATE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 24_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call snow_state_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%snowfall_cum, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%snow_albedo, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%chSnow_per_interval, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%mwh, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%melt_water_store, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%qn_snow, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%qm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%qm_freeze, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%qm_rain, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%swe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%z0v_snow, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%ra_snow, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%s_ice_hpa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(14_c_int, actual%snow_removal, int(lbound(actual%snow_removal, 1), c_int), int(ubound(actual%snow_removal, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(15_c_int, actual%ice_frac, int(lbound(actual%ice_frac, 1), c_int), int(ubound(actual%ice_frac, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(16_c_int, actual%snow_density, int(lbound(actual%snow_density, 1), c_int), int(ubound(actual%snow_density, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(17_c_int, actual%snow_fraction, int(lbound(actual%snow_fraction, 1), c_int), int(ubound(actual%snow_fraction, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(18_c_int, actual%snow_pack, int(lbound(actual%snow_pack, 1), c_int), int(ubound(actual%snow_pack, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(19_c_int, actual%snow_water, int(lbound(actual%snow_water, 1), c_int), int(ubound(actual%snow_water, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(20_c_int, actual%kup_ind_snow, int(lbound(actual%kup_ind_snow, 1), c_int), int(ubound(actual%kup_ind_snow, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(21_c_int, actual%qn_ind_snow, int(lbound(actual%qn_ind_snow, 1), c_int), int(ubound(actual%qn_ind_snow, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(22_c_int, actual%delta_qi, int(lbound(actual%delta_qi, 1), c_int), int(ubound(actual%delta_qi, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(23_c_int, actual%Tsurf_ind_snow, int(lbound(actual%Tsurf_ind_snow, 1), c_int), int(ubound(actual%Tsurf_ind_snow, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(24_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_snow_state

   subroutine suews_obs_unpack_solar_state( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_solar_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(solar_State) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 3_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call solar_state_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%azimuth_deg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%zenith_deg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(3_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_solar_state

   subroutine suews_obs_unpack_spartacus_layer_prm( &
      flat, n_flat, nlayer, nspec, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_spartacus_layer_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), value, intent(in) :: nlayer
      integer(c_int), value, intent(in) :: nspec
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(SPARTACUS_LAYER_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 11_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call spartacus_layer_prm_unpack(flat, n_flat, nlayer, nspec, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      if (allocated(actual%building_frac)) then
         call obs_emit_real_1d(1_c_int, actual%building_frac, int(lbound(actual%building_frac, 1), c_int), int(ubound(actual%building_frac, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(1_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%building_scale)) then
         call obs_emit_real_1d(2_c_int, actual%building_scale, int(lbound(actual%building_scale, 1), c_int), int(ubound(actual%building_scale, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(2_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%veg_frac)) then
         call obs_emit_real_1d(3_c_int, actual%veg_frac, int(lbound(actual%veg_frac, 1), c_int), int(ubound(actual%veg_frac, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(3_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%veg_scale)) then
         call obs_emit_real_1d(4_c_int, actual%veg_scale, int(lbound(actual%veg_scale, 1), c_int), int(ubound(actual%veg_scale, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(4_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%veg_ext)) then
         call obs_emit_real_1d(5_c_int, actual%veg_ext, int(lbound(actual%veg_ext, 1), c_int), int(ubound(actual%veg_ext, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(5_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%alb_roof)) then
         call obs_emit_real_1d(6_c_int, actual%alb_roof, int(lbound(actual%alb_roof, 1), c_int), int(ubound(actual%alb_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(6_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%emis_roof)) then
         call obs_emit_real_1d(7_c_int, actual%emis_roof, int(lbound(actual%emis_roof, 1), c_int), int(ubound(actual%emis_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(7_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%alb_wall)) then
         call obs_emit_real_1d(8_c_int, actual%alb_wall, int(lbound(actual%alb_wall, 1), c_int), int(ubound(actual%alb_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(8_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%emis_wall)) then
         call obs_emit_real_1d(9_c_int, actual%emis_wall, int(lbound(actual%emis_wall, 1), c_int), int(ubound(actual%emis_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(9_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%roof_albedo_dir_mult_fact)) then
         call obs_emit_real_2d(10_c_int, actual%roof_albedo_dir_mult_fact, int(lbound(actual%roof_albedo_dir_mult_fact, 1), c_int), int(ubound(actual%roof_albedo_dir_mult_fact, 1), c_int), int(lbound(actual%roof_albedo_dir_mult_fact, 2), c_int), int(ubound(actual%roof_albedo_dir_mult_fact, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(10_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%wall_specular_frac)) then
         call obs_emit_real_2d(11_c_int, actual%wall_specular_frac, int(lbound(actual%wall_specular_frac, 1), c_int), int(ubound(actual%wall_specular_frac, 1), c_int), int(lbound(actual%wall_specular_frac, 2), c_int), int(ubound(actual%wall_specular_frac, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(11_c_int, out_meta, n_meta, err)
      end if

      n_out = pos
   end subroutine suews_obs_unpack_spartacus_layer_prm

   subroutine suews_obs_unpack_spartacus_prm( &
      flat, n_flat, nlayer, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_spartacus_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), value, intent(in) :: nlayer
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(SPARTACUS_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 18_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call spartacus_prm_unpack(flat, n_flat, nlayer, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%air_ext_lw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%air_ext_sw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%air_ssa_lw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%air_ssa_sw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      if (allocated(actual%height)) then
         call obs_emit_real_1d(5_c_int, actual%height, int(lbound(actual%height, 1), c_int), int(ubound(actual%height, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(5_c_int, out_meta, n_meta, err)
      end if
      call obs_emit_real_scalar(6_c_int, actual%ground_albedo_dir_mult_fact, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(7_c_int, actual%n_stream_lw_urban, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(8_c_int, actual%n_stream_sw_urban, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(9_c_int, actual%n_stream_lw_forest, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(10_c_int, actual%n_stream_sw_forest, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(11_c_int, actual%n_vegetation_region_urban, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(12_c_int, actual%n_vegetation_region_forest, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%sw_dn_direct_frac, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%use_sw_direct_albedo, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%veg_contact_fraction_const, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%veg_fsd_const, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%veg_ssa_lw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%veg_ssa_sw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_spartacus_prm

   subroutine suews_obs_unpack_stebbs_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_stebbs_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(STEBBS_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 50_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call stebbs_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%wall_internal_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%roof_internal_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%internal_mass_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%floor_internal_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%window_internal_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%wall_external_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%roof_external_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%window_external_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%ground_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%external_ground_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%indoor_air_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%indoor_air_cp, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%metabolism_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%latent_sensible_ratio, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(15_c_int, actual%daylight_control, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%lighting_illuminance_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(17_c_int, actual%internal_shading, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%reduction_factor_shading, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%temperature_threshold_shading, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%radiation_threshold_shading, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%heating_system_efficiency, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(22_c_int, actual%destination_waste_heat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%fraction_convective_heating, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%max_cooling_power, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%cooling_system_cop, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%ventilation_rate, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%hot_water_tank_wall_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%hot_water_tank_surface_area, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%hot_water_heating_setpoint_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%hot_water_tank_wall_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%hot_water_vessel_wall_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%hot_water_volume, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%hot_water_surface_area, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%hot_water_flow_rate, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(35_c_int, actual%hot_water_flow_profile, int(lbound(actual%hot_water_flow_profile, 1), c_int), int(ubound(actual%hot_water_flow_profile, 1), c_int), int(lbound(actual%hot_water_flow_profile, 2), c_int), int(ubound(actual%hot_water_flow_profile, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%hot_water_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(37_c_int, actual%hot_water_tank_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(38_c_int, actual%hot_water_vessel_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(39_c_int, actual%hot_water_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(40_c_int, actual%hot_water_tank_wall_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(41_c_int, actual%hot_water_vessel_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(42_c_int, actual%hot_water_tank_wall_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(43_c_int, actual%hot_water_tank_internal_wall_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(44_c_int, actual%hot_water_tank_external_wall_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(45_c_int, actual%hot_water_vessel_wall_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(46_c_int, actual%hot_water_vessel_internal_wall_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(47_c_int, actual%hot_water_vessel_external_wall_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(48_c_int, actual%hot_water_vessel_wall_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(49_c_int, actual%hot_water_heating_efficiency, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(50_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_stebbs_prm

   subroutine suews_obs_unpack_stebbs_state( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_stebbs_state')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(STEBBS_STATE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 39_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call stebbs_state_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%kdown_2d, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%kup_2d, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%k_west, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(4_c_int, actual%k_south, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%k_north, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%k_east, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%ldown_2d, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%lup_2d, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%l_west, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%l_south, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%l_north, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%l_east, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(13_c_int, actual%z_array, int(lbound(actual%z_array, 1), c_int), int(ubound(actual%z_array, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(14_c_int, actual%dataout_line_u_rsl, int(lbound(actual%dataout_line_u_rsl, 1), c_int), int(ubound(actual%dataout_line_u_rsl, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(15_c_int, actual%dataout_line_t_rsl, int(lbound(actual%dataout_line_t_rsl, 1), c_int), int(ubound(actual%dataout_line_t_rsl, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(16_c_int, actual%dataout_line_q_rsl, int(lbound(actual%dataout_line_q_rsl, 1), c_int), int(ubound(actual%dataout_line_q_rsl, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%deep_soil_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%MonthMeanAirTemperature_diffmax, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%outdoor_air_start_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%indoor_air_start_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%indoor_mass_start_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%wall_indoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%wall_outdoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%roof_indoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%roof_outdoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%window_indoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%window_outdoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%ground_floor_indoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%ground_floor_outdoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%water_tank_temperature_state, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%internal_wall_water_tank_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%external_wall_water_tank_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%mains_water_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%domestic_hot_water_temperature_in_use_in_building, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(35_c_int, actual%internal_wall_dhw_vessel_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%external_wall_dhw_vessel_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(37_c_int, actual%QS_stebbs, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      if (allocated(actual%buildings)) then
         call obs_emit_alloc_bounds_1d(38_c_int, int(lbound(actual%buildings, 1), c_int), int(ubound(actual%buildings, 1), c_int), out_meta, n_meta, err)
      else
         call obs_emit_unallocated(38_c_int, out_meta, n_meta, err)
      end if
      call obs_emit_logical_scalar(39_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_stebbs_state

   subroutine suews_obs_unpack_surf_store_prm( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_surf_store_prm')
      implicit none
      real(c_double), intent(in) :: flat(*)
      integer(c_int), value, intent(in) :: n_flat
      integer(c_int), intent(out) :: out_fid(*)
      integer(c_int), intent(out) :: out_kind(*)
      integer(c_int), intent(out) :: out_i1(*)
      integer(c_int), intent(out) :: out_i2(*)
      real(c_double), intent(out) :: out_f64(*)
      integer(c_int64_t), intent(out) :: out_i64(*)
      integer(c_int), value, intent(in) :: n_meta
      integer(c_int), intent(out) :: out_meta(*)
      integer(c_int), value, intent(in) :: cap
      integer(c_int), intent(out) :: n_out
      integer(c_int), intent(out) :: err
      type(SURF_STORE_PRM) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 6_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call surf_store_prm_unpack(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%store_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%store_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%store_cap, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(4_c_int, actual%drain_eq, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%drain_coef_1, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%drain_coef_2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_surf_store_prm

end module module_test_capi_field_observer
