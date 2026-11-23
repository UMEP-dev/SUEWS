! =============================================================================
! DO NOT EDIT -- emitted from scripts/suews/gen_capi_field_observer.py.
! Test-only typed Fortran C-API field-binding observer (gh#2096).
! Regenerate with: python3 scripts/suews/gen_capi_field_observer.py
! =============================================================================
module module_test_capi_field_observer
   use, intrinsic :: iso_c_binding, only: c_int, c_int64_t, c_double
   use module_c_api_common, only: SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER
   use module_ctrl_type, only: SUEWS_CONFIG, SUEWS_SITE, SUEWS_STATE, SUEWS_TIMER, flag_STATE
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
   use module_c_api_driver, only: unpack_config, unpack_timer, unpack_site_scalars, unpack_site_members, unpack_state_members

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
   public :: suews_obs_unpack_suews_config
   public :: suews_obs_unpack_suews_site
   public :: suews_obs_unpack_suews_state
   public :: suews_obs_unpack_suews_timer

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

   subroutine obs_emit_int_1d( &
      fid, arr, lb1, ub1, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, &
      out_meta, n_meta, cap, pos, err)
      implicit none
      integer(c_int), intent(in) :: fid
      integer(c_int), intent(in) :: lb1
      integer(c_int), intent(in) :: ub1
      integer, intent(in) :: arr(lb1:ub1)
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
         out_kind(pos) = OBS_KIND_INT
         out_i1(pos) = i
         out_i2(pos) = 0_c_int
         out_f64(pos) = 0.0_c_double
         out_i64(pos) = int(arr(i), c_int64_t)
      end do
   end subroutine obs_emit_int_1d

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
      if (n_meta < 56_c_int) then
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
      call obs_emit_real_1d(12_c_int, actual%qn_rav, int(lbound(actual%qn_rav, 1), c_int), int(ubound(actual%qn_rav, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%a1_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%a2_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%a3_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%a1_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%a2_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%a3_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%a1_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%a2_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%a3_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%a1_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%a2_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%a3_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%a1_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%a2_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%a3_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%a1_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%a2_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%a3_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%a1_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%a2_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%a3_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(34_c_int, actual%anohm_working_day, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(35_c_int, actual%anohm_working_count, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(36_c_int, actual%anohm_working_tHr, int(lbound(actual%anohm_working_tHr, 1), c_int), int(ubound(actual%anohm_working_tHr, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(37_c_int, actual%anohm_working_sd, int(lbound(actual%anohm_working_sd, 1), c_int), int(ubound(actual%anohm_working_sd, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(38_c_int, actual%anohm_working_ta, int(lbound(actual%anohm_working_ta, 1), c_int), int(ubound(actual%anohm_working_ta, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(39_c_int, actual%anohm_working_rh, int(lbound(actual%anohm_working_rh, 1), c_int), int(ubound(actual%anohm_working_rh, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(40_c_int, actual%anohm_working_pres, int(lbound(actual%anohm_working_pres, 1), c_int), int(ubound(actual%anohm_working_pres, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(41_c_int, actual%anohm_working_ws, int(lbound(actual%anohm_working_ws, 1), c_int), int(ubound(actual%anohm_working_ws, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(42_c_int, actual%anohm_working_ah, int(lbound(actual%anohm_working_ah, 1), c_int), int(ubound(actual%anohm_working_ah, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(43_c_int, actual%anohm_coeff_day, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(44_c_int, actual%anohm_coeff_count, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(45_c_int, actual%anohm_coeff_ready, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(46_c_int, actual%anohm_coeff_tHr, int(lbound(actual%anohm_coeff_tHr, 1), c_int), int(ubound(actual%anohm_coeff_tHr, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(47_c_int, actual%anohm_coeff_sd, int(lbound(actual%anohm_coeff_sd, 1), c_int), int(ubound(actual%anohm_coeff_sd, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(48_c_int, actual%anohm_coeff_ta, int(lbound(actual%anohm_coeff_ta, 1), c_int), int(ubound(actual%anohm_coeff_ta, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(49_c_int, actual%anohm_coeff_rh, int(lbound(actual%anohm_coeff_rh, 1), c_int), int(ubound(actual%anohm_coeff_rh, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(50_c_int, actual%anohm_coeff_pres, int(lbound(actual%anohm_coeff_pres, 1), c_int), int(ubound(actual%anohm_coeff_pres, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(51_c_int, actual%anohm_coeff_ws, int(lbound(actual%anohm_coeff_ws, 1), c_int), int(ubound(actual%anohm_coeff_ws, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(52_c_int, actual%anohm_coeff_ah, int(lbound(actual%anohm_coeff_ah, 1), c_int), int(ubound(actual%anohm_coeff_ah, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(53_c_int, actual%anohm_a1_surf, int(lbound(actual%anohm_a1_surf, 1), c_int), int(ubound(actual%anohm_a1_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(54_c_int, actual%anohm_a2_surf, int(lbound(actual%anohm_a2_surf, 1), c_int), int(ubound(actual%anohm_a2_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(55_c_int, actual%anohm_a3_surf, int(lbound(actual%anohm_a3_surf, 1), c_int), int(ubound(actual%anohm_a3_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(56_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

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
      if (n_meta < 36_c_int) then
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
      call obs_emit_real_scalar(20_c_int, actual%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(35_c_int, actual%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

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
      if (n_meta < 23_c_int) then
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
      call obs_emit_real_1d(13_c_int, actual%snow_removal, int(lbound(actual%snow_removal, 1), c_int), int(ubound(actual%snow_removal, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(14_c_int, actual%ice_frac, int(lbound(actual%ice_frac, 1), c_int), int(ubound(actual%ice_frac, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(15_c_int, actual%snow_density, int(lbound(actual%snow_density, 1), c_int), int(ubound(actual%snow_density, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(16_c_int, actual%snow_fraction, int(lbound(actual%snow_fraction, 1), c_int), int(ubound(actual%snow_fraction, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(17_c_int, actual%snow_pack, int(lbound(actual%snow_pack, 1), c_int), int(ubound(actual%snow_pack, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(18_c_int, actual%snow_water, int(lbound(actual%snow_water, 1), c_int), int(ubound(actual%snow_water, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(19_c_int, actual%kup_ind_snow, int(lbound(actual%kup_ind_snow, 1), c_int), int(ubound(actual%kup_ind_snow, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(20_c_int, actual%qn_ind_snow, int(lbound(actual%qn_ind_snow, 1), c_int), int(ubound(actual%qn_ind_snow, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(21_c_int, actual%delta_qi, int(lbound(actual%delta_qi, 1), c_int), int(ubound(actual%delta_qi, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(22_c_int, actual%Tsurf_ind_snow, int(lbound(actual%Tsurf_ind_snow, 1), c_int), int(ubound(actual%Tsurf_ind_snow, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(23_c_int, actual%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

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

   subroutine suews_obs_unpack_suews_config( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_suews_config')
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
      type(SUEWS_CONFIG) :: actual
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
      call unpack_config(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_int_scalar(1_c_int, actual%RSLMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(2_c_int, actual%EmissionsMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(3_c_int, actual%RoughLenHeatMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(4_c_int, actual%RoughLenMomMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(5_c_int, actual%FAIMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(6_c_int, actual%SMDMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(7_c_int, actual%WaterUseMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(8_c_int, actual%NetRadiationMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(9_c_int, actual%KdownSplitMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(10_c_int, actual%StabilityMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(11_c_int, actual%StorageHeatMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(12_c_int, actual%Diagnose, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(13_c_int, actual%SnowUse, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(14_c_int, actual%use_sw_direct_albedo, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(15_c_int, actual%ohmIncQF, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(16_c_int, actual%DiagQS, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(17_c_int, actual%EvapMethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(18_c_int, actual%LAImethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(19_c_int, actual%RSLLevel, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(20_c_int, actual%stebbsmethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(21_c_int, actual%rcmethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(22_c_int, actual%setpointmethod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(23_c_int, actual%flag_test, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(24_c_int, actual%ForcingTimestampReference, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_suews_config

   subroutine suews_obs_unpack_suews_timer( &
      flat, n_flat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_suews_timer')
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
      type(SUEWS_TIMER) :: actual
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
      call unpack_timer(flat, n_flat, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_int_scalar(1_c_int, actual%id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(2_c_int, actual%imin, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(3_c_int, actual%isec, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(4_c_int, actual%it, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(5_c_int, actual%iy, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(6_c_int, actual%tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(7_c_int, actual%tstep_prev, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(8_c_int, actual%dt_since_start, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(9_c_int, actual%dt_since_start_prev, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(10_c_int, actual%nsh, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%nsh_real, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%tstep_real, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%dectime, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_1d(14_c_int, actual%dayofWeek_id, int(lbound(actual%dayofWeek_id, 1), c_int), int(ubound(actual%dayofWeek_id, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(15_c_int, actual%DLS, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(16_c_int, actual%new_day, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(17_c_int, actual%iy_st, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(18_c_int, actual%id_st, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(19_c_int, actual%it_st, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(20_c_int, actual%imin_st, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%dectime_st, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_1d(22_c_int, actual%dayofWeek_id_st, int(lbound(actual%dayofWeek_id_st, 1), c_int), int(ubound(actual%dayofWeek_id_st, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%tz_solar, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_suews_timer

   subroutine suews_obs_unpack_suews_site( &
      site_scalars_flat, site_scalars_len, site_flat, site_flat_len, &
      site_toc, site_toc_len, site_member_count, nlayer, ndepth, &
      out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, &
      n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_suews_site')
      implicit none
      real(c_double), intent(in) :: site_scalars_flat(*)
      integer(c_int), value, intent(in) :: site_scalars_len
      real(c_double), intent(in) :: site_flat(*)
      integer(c_int), value, intent(in) :: site_flat_len
      integer(c_int), intent(in) :: site_toc(*)
      integer(c_int), value, intent(in) :: site_toc_len
      integer(c_int), value, intent(in) :: site_member_count
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
      type(SUEWS_SITE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 610_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call unpack_site_scalars(site_scalars_flat, site_scalars_len, actual, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if
      call unpack_site_members( &
         actual, int(nlayer), int(ndepth), &
         site_flat, site_flat_len, site_toc, site_toc_len, site_member_count, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_real_scalar(1_c_int, actual%lat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(2_c_int, actual%lon, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(3_c_int, actual%alt, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(4_c_int, actual%gridiv, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(5_c_int, actual%timezone, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(6_c_int, actual%surfacearea, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(7_c_int, actual%z, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%z0m_in, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%zdm_in, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%pipecapacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%runofftowater, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%narp_trans_site, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%CO2PointSource, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%flow_change, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%n_buildings, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%h_std, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(17_c_int, actual%lambda_c, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(18_c_int, actual%sfr_surf, int(lbound(actual%sfr_surf, 1), c_int), int(ubound(actual%sfr_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%VegFraction, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(20_c_int, actual%ImpervFraction, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(21_c_int, actual%PervFraction, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%NonWaterFraction, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      if (allocated(actual%sfr_roof)) then
         call obs_emit_real_1d(23_c_int, actual%sfr_roof, int(lbound(actual%sfr_roof, 1), c_int), int(ubound(actual%sfr_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(23_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%sfr_wall)) then
         call obs_emit_real_1d(24_c_int, actual%sfr_wall, int(lbound(actual%sfr_wall, 1), c_int), int(ubound(actual%sfr_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(24_c_int, out_meta, n_meta, err)
      end if
      call obs_emit_int_scalar(25_c_int, actual%nlayer, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%spartacus%air_ext_lw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%spartacus%air_ext_sw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%spartacus%air_ssa_lw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(29_c_int, actual%spartacus%air_ssa_sw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      if (allocated(actual%spartacus%height)) then
         call obs_emit_real_1d(30_c_int, actual%spartacus%height, int(lbound(actual%spartacus%height, 1), c_int), int(ubound(actual%spartacus%height, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(30_c_int, out_meta, n_meta, err)
      end if
      call obs_emit_real_scalar(31_c_int, actual%spartacus%ground_albedo_dir_mult_fact, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(32_c_int, actual%spartacus%n_stream_lw_urban, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(33_c_int, actual%spartacus%n_stream_sw_urban, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(34_c_int, actual%spartacus%n_stream_lw_forest, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(35_c_int, actual%spartacus%n_stream_sw_forest, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(36_c_int, actual%spartacus%n_vegetation_region_urban, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(37_c_int, actual%spartacus%n_vegetation_region_forest, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(38_c_int, actual%spartacus%sw_dn_direct_frac, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(39_c_int, actual%spartacus%use_sw_direct_albedo, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(40_c_int, actual%spartacus%veg_contact_fraction_const, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(41_c_int, actual%spartacus%veg_fsd_const, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(42_c_int, actual%spartacus%veg_ssa_lw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(43_c_int, actual%spartacus%veg_ssa_sw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(44_c_int, actual%lumps%rain_cover, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(45_c_int, actual%lumps%rain_max_res, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(46_c_int, actual%lumps%drain_rate, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(47_c_int, actual%lumps%veg_type, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      if (allocated(actual%ehc%soil_store_capacity_roof)) then
         call obs_emit_real_1d(48_c_int, actual%ehc%soil_store_capacity_roof, int(lbound(actual%ehc%soil_store_capacity_roof, 1), c_int), int(ubound(actual%ehc%soil_store_capacity_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(48_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%soil_store_capacity_wall)) then
         call obs_emit_real_1d(49_c_int, actual%ehc%soil_store_capacity_wall, int(lbound(actual%ehc%soil_store_capacity_wall, 1), c_int), int(ubound(actual%ehc%soil_store_capacity_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(49_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%state_limit_roof)) then
         call obs_emit_real_1d(50_c_int, actual%ehc%state_limit_roof, int(lbound(actual%ehc%state_limit_roof, 1), c_int), int(ubound(actual%ehc%state_limit_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(50_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%state_limit_wall)) then
         call obs_emit_real_1d(51_c_int, actual%ehc%state_limit_wall, int(lbound(actual%ehc%state_limit_wall, 1), c_int), int(ubound(actual%ehc%state_limit_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(51_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%wet_thresh_roof)) then
         call obs_emit_real_1d(52_c_int, actual%ehc%wet_thresh_roof, int(lbound(actual%ehc%wet_thresh_roof, 1), c_int), int(ubound(actual%ehc%wet_thresh_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(52_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%wet_thresh_wall)) then
         call obs_emit_real_1d(53_c_int, actual%ehc%wet_thresh_wall, int(lbound(actual%ehc%wet_thresh_wall, 1), c_int), int(ubound(actual%ehc%wet_thresh_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(53_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%tin_roof)) then
         call obs_emit_real_1d(54_c_int, actual%ehc%tin_roof, int(lbound(actual%ehc%tin_roof, 1), c_int), int(ubound(actual%ehc%tin_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(54_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%tin_wall)) then
         call obs_emit_real_1d(55_c_int, actual%ehc%tin_wall, int(lbound(actual%ehc%tin_wall, 1), c_int), int(ubound(actual%ehc%tin_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(55_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%tin_surf)) then
         call obs_emit_real_1d(56_c_int, actual%ehc%tin_surf, int(lbound(actual%ehc%tin_surf, 1), c_int), int(ubound(actual%ehc%tin_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(56_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%k_roof)) then
         call obs_emit_real_2d(57_c_int, actual%ehc%k_roof, int(lbound(actual%ehc%k_roof, 1), c_int), int(ubound(actual%ehc%k_roof, 1), c_int), int(lbound(actual%ehc%k_roof, 2), c_int), int(ubound(actual%ehc%k_roof, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(57_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%k_wall)) then
         call obs_emit_real_2d(58_c_int, actual%ehc%k_wall, int(lbound(actual%ehc%k_wall, 1), c_int), int(ubound(actual%ehc%k_wall, 1), c_int), int(lbound(actual%ehc%k_wall, 2), c_int), int(ubound(actual%ehc%k_wall, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(58_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%k_surf)) then
         call obs_emit_real_2d(59_c_int, actual%ehc%k_surf, int(lbound(actual%ehc%k_surf, 1), c_int), int(ubound(actual%ehc%k_surf, 1), c_int), int(lbound(actual%ehc%k_surf, 2), c_int), int(ubound(actual%ehc%k_surf, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(59_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%cp_roof)) then
         call obs_emit_real_2d(60_c_int, actual%ehc%cp_roof, int(lbound(actual%ehc%cp_roof, 1), c_int), int(ubound(actual%ehc%cp_roof, 1), c_int), int(lbound(actual%ehc%cp_roof, 2), c_int), int(ubound(actual%ehc%cp_roof, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(60_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%cp_wall)) then
         call obs_emit_real_2d(61_c_int, actual%ehc%cp_wall, int(lbound(actual%ehc%cp_wall, 1), c_int), int(ubound(actual%ehc%cp_wall, 1), c_int), int(lbound(actual%ehc%cp_wall, 2), c_int), int(ubound(actual%ehc%cp_wall, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(61_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%cp_surf)) then
         call obs_emit_real_2d(62_c_int, actual%ehc%cp_surf, int(lbound(actual%ehc%cp_surf, 1), c_int), int(ubound(actual%ehc%cp_surf, 1), c_int), int(lbound(actual%ehc%cp_surf, 2), c_int), int(ubound(actual%ehc%cp_surf, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(62_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%dz_roof)) then
         call obs_emit_real_2d(63_c_int, actual%ehc%dz_roof, int(lbound(actual%ehc%dz_roof, 1), c_int), int(ubound(actual%ehc%dz_roof, 1), c_int), int(lbound(actual%ehc%dz_roof, 2), c_int), int(ubound(actual%ehc%dz_roof, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(63_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%dz_wall)) then
         call obs_emit_real_2d(64_c_int, actual%ehc%dz_wall, int(lbound(actual%ehc%dz_wall, 1), c_int), int(ubound(actual%ehc%dz_wall, 1), c_int), int(lbound(actual%ehc%dz_wall, 2), c_int), int(ubound(actual%ehc%dz_wall, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(64_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%ehc%dz_surf)) then
         call obs_emit_real_2d(65_c_int, actual%ehc%dz_surf, int(lbound(actual%ehc%dz_surf, 1), c_int), int(ubound(actual%ehc%dz_surf, 1), c_int), int(lbound(actual%ehc%dz_surf, 2), c_int), int(ubound(actual%ehc%dz_surf, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(65_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%spartacus_layer%building_frac)) then
         call obs_emit_real_1d(66_c_int, actual%spartacus_layer%building_frac, int(lbound(actual%spartacus_layer%building_frac, 1), c_int), int(ubound(actual%spartacus_layer%building_frac, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(66_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%spartacus_layer%building_scale)) then
         call obs_emit_real_1d(67_c_int, actual%spartacus_layer%building_scale, int(lbound(actual%spartacus_layer%building_scale, 1), c_int), int(ubound(actual%spartacus_layer%building_scale, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(67_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%spartacus_layer%veg_frac)) then
         call obs_emit_real_1d(68_c_int, actual%spartacus_layer%veg_frac, int(lbound(actual%spartacus_layer%veg_frac, 1), c_int), int(ubound(actual%spartacus_layer%veg_frac, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(68_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%spartacus_layer%veg_scale)) then
         call obs_emit_real_1d(69_c_int, actual%spartacus_layer%veg_scale, int(lbound(actual%spartacus_layer%veg_scale, 1), c_int), int(ubound(actual%spartacus_layer%veg_scale, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(69_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%spartacus_layer%veg_ext)) then
         call obs_emit_real_1d(70_c_int, actual%spartacus_layer%veg_ext, int(lbound(actual%spartacus_layer%veg_ext, 1), c_int), int(ubound(actual%spartacus_layer%veg_ext, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(70_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%spartacus_layer%alb_roof)) then
         call obs_emit_real_1d(71_c_int, actual%spartacus_layer%alb_roof, int(lbound(actual%spartacus_layer%alb_roof, 1), c_int), int(ubound(actual%spartacus_layer%alb_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(71_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%spartacus_layer%emis_roof)) then
         call obs_emit_real_1d(72_c_int, actual%spartacus_layer%emis_roof, int(lbound(actual%spartacus_layer%emis_roof, 1), c_int), int(ubound(actual%spartacus_layer%emis_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(72_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%spartacus_layer%alb_wall)) then
         call obs_emit_real_1d(73_c_int, actual%spartacus_layer%alb_wall, int(lbound(actual%spartacus_layer%alb_wall, 1), c_int), int(ubound(actual%spartacus_layer%alb_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(73_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%spartacus_layer%emis_wall)) then
         call obs_emit_real_1d(74_c_int, actual%spartacus_layer%emis_wall, int(lbound(actual%spartacus_layer%emis_wall, 1), c_int), int(ubound(actual%spartacus_layer%emis_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(74_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%spartacus_layer%roof_albedo_dir_mult_fact)) then
         call obs_emit_real_2d(75_c_int, actual%spartacus_layer%roof_albedo_dir_mult_fact, int(lbound(actual%spartacus_layer%roof_albedo_dir_mult_fact, 1), c_int), int(ubound(actual%spartacus_layer%roof_albedo_dir_mult_fact, 1), c_int), int(lbound(actual%spartacus_layer%roof_albedo_dir_mult_fact, 2), c_int), int(ubound(actual%spartacus_layer%roof_albedo_dir_mult_fact, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(75_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%spartacus_layer%wall_specular_frac)) then
         call obs_emit_real_2d(76_c_int, actual%spartacus_layer%wall_specular_frac, int(lbound(actual%spartacus_layer%wall_specular_frac, 1), c_int), int(ubound(actual%spartacus_layer%wall_specular_frac, 1), c_int), int(lbound(actual%spartacus_layer%wall_specular_frac, 2), c_int), int(ubound(actual%spartacus_layer%wall_specular_frac, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(76_c_int, out_meta, n_meta, err)
      end if
      call obs_emit_real_scalar(77_c_int, actual%surf_store%store_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(78_c_int, actual%surf_store%store_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(79_c_int, actual%surf_store%store_cap, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(80_c_int, actual%surf_store%drain_eq, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(81_c_int, actual%surf_store%drain_coef_1, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(82_c_int, actual%surf_store%drain_coef_2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(83_c_int, actual%irrigation%h_maintain, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(84_c_int, actual%irrigation%f_aut, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(85_c_int, actual%irrigation%ie_a, int(lbound(actual%irrigation%ie_a, 1), c_int), int(ubound(actual%irrigation%ie_a, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(86_c_int, actual%irrigation%ie_m, int(lbound(actual%irrigation%ie_m, 1), c_int), int(ubound(actual%irrigation%ie_m, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(87_c_int, actual%irrigation%ie_start, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(88_c_int, actual%irrigation%ie_end, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(89_c_int, actual%irrigation%internalwateruse_h, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(90_c_int, actual%irrigation%irr_daywater%monday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(91_c_int, actual%irrigation%irr_daywater%monday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(92_c_int, actual%irrigation%irr_daywater%tuesday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(93_c_int, actual%irrigation%irr_daywater%tuesday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(94_c_int, actual%irrigation%irr_daywater%wednesday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(95_c_int, actual%irrigation%irr_daywater%wednesday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(96_c_int, actual%irrigation%irr_daywater%thursday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(97_c_int, actual%irrigation%irr_daywater%thursday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(98_c_int, actual%irrigation%irr_daywater%friday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(99_c_int, actual%irrigation%irr_daywater%friday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(100_c_int, actual%irrigation%irr_daywater%saturday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(101_c_int, actual%irrigation%irr_daywater%saturday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(102_c_int, actual%irrigation%irr_daywater%sunday_flag, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(103_c_int, actual%irrigation%irr_daywater%sunday_percent, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(104_c_int, actual%irrigation%wuprofa_24hr_working, int(lbound(actual%irrigation%wuprofa_24hr_working, 1), c_int), int(ubound(actual%irrigation%wuprofa_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(105_c_int, actual%irrigation%wuprofa_24hr_holiday, int(lbound(actual%irrigation%wuprofa_24hr_holiday, 1), c_int), int(ubound(actual%irrigation%wuprofa_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(106_c_int, actual%irrigation%wuprofm_24hr_working, int(lbound(actual%irrigation%wuprofm_24hr_working, 1), c_int), int(ubound(actual%irrigation%wuprofm_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(107_c_int, actual%irrigation%wuprofm_24hr_holiday, int(lbound(actual%irrigation%wuprofm_24hr_holiday, 1), c_int), int(ubound(actual%irrigation%wuprofm_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(108_c_int, actual%anthroemis%start_dls, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(109_c_int, actual%anthroemis%end_dls, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(110_c_int, actual%anthroemis%anthro_heat%qf0_beu_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(111_c_int, actual%anthroemis%anthro_heat%qf0_beu_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(112_c_int, actual%anthroemis%anthro_heat%qf_a_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(113_c_int, actual%anthroemis%anthro_heat%qf_a_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(114_c_int, actual%anthroemis%anthro_heat%qf_b_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(115_c_int, actual%anthroemis%anthro_heat%qf_b_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(116_c_int, actual%anthroemis%anthro_heat%qf_c_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(117_c_int, actual%anthroemis%anthro_heat%qf_c_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(118_c_int, actual%anthroemis%anthro_heat%baset_cooling_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(119_c_int, actual%anthroemis%anthro_heat%baset_cooling_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(120_c_int, actual%anthroemis%anthro_heat%baset_heating_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(121_c_int, actual%anthroemis%anthro_heat%baset_heating_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(122_c_int, actual%anthroemis%anthro_heat%ah_min_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(123_c_int, actual%anthroemis%anthro_heat%ah_min_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(124_c_int, actual%anthroemis%anthro_heat%ah_slope_cooling_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(125_c_int, actual%anthroemis%anthro_heat%ah_slope_cooling_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(126_c_int, actual%anthroemis%anthro_heat%ah_slope_heating_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(127_c_int, actual%anthroemis%anthro_heat%ah_slope_heating_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(128_c_int, actual%anthroemis%anthro_heat%ahprof_24hr_working, int(lbound(actual%anthroemis%anthro_heat%ahprof_24hr_working, 1), c_int), int(ubound(actual%anthroemis%anthro_heat%ahprof_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(129_c_int, actual%anthroemis%anthro_heat%ahprof_24hr_holiday, int(lbound(actual%anthroemis%anthro_heat%ahprof_24hr_holiday, 1), c_int), int(ubound(actual%anthroemis%anthro_heat%ahprof_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(130_c_int, actual%anthroemis%anthro_heat%pop_density_daytime_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(131_c_int, actual%anthroemis%anthro_heat%pop_density_daytime_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(132_c_int, actual%anthroemis%anthro_heat%pop_density_nighttime, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(133_c_int, actual%anthroemis%anthro_heat%popprof_24hr_working, int(lbound(actual%anthroemis%anthro_heat%popprof_24hr_working, 1), c_int), int(ubound(actual%anthroemis%anthro_heat%popprof_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(134_c_int, actual%anthroemis%anthro_heat%popprof_24hr_holiday, int(lbound(actual%anthroemis%anthro_heat%popprof_24hr_holiday, 1), c_int), int(ubound(actual%anthroemis%anthro_heat%popprof_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(135_c_int, actual%anthroemis%ef_umol_co2_per_j, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(136_c_int, actual%anthroemis%en_ef_v_jkm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(137_c_int, actual%anthroemis%fr_fossil_fuel_heat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(138_c_int, actual%anthroemis%fr_fossil_fuel_non_heat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(139_c_int, actual%anthroemis%fc_ef_v_kgkm, int(lbound(actual%anthroemis%fc_ef_v_kgkm, 1), c_int), int(ubound(actual%anthroemis%fc_ef_v_kgkm, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(140_c_int, actual%anthroemis%hum_activity_24hr_working, int(lbound(actual%anthroemis%hum_activity_24hr_working, 1), c_int), int(ubound(actual%anthroemis%hum_activity_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(141_c_int, actual%anthroemis%hum_activity_24hr_holiday, int(lbound(actual%anthroemis%hum_activity_24hr_holiday, 1), c_int), int(ubound(actual%anthroemis%hum_activity_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(142_c_int, actual%anthroemis%max_fc_metab, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(143_c_int, actual%anthroemis%max_qf_metab, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(144_c_int, actual%anthroemis%min_fc_metab, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(145_c_int, actual%anthroemis%min_qf_metab, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(146_c_int, actual%anthroemis%traffic_rate_working, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(147_c_int, actual%anthroemis%traffic_rate_holiday, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(148_c_int, actual%anthroemis%traffic_units, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(149_c_int, actual%anthroemis%traff_prof_24hr_working, int(lbound(actual%anthroemis%traff_prof_24hr_working, 1), c_int), int(ubound(actual%anthroemis%traff_prof_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(150_c_int, actual%anthroemis%traff_prof_24hr_holiday, int(lbound(actual%anthroemis%traff_prof_24hr_holiday, 1), c_int), int(ubound(actual%anthroemis%traff_prof_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(151_c_int, actual%snow%water_holding_capacity_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(152_c_int, actual%snow%water_holding_capacity_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(153_c_int, actual%snow%narp_emis_snow, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(154_c_int, actual%snow%temperature_rain_snow_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(155_c_int, actual%snow%precipitation_threshold_albedo_reset, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(156_c_int, actual%snow%snow_albedo_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(157_c_int, actual%snow%snow_albedo_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(158_c_int, actual%snow%snow_density_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(159_c_int, actual%snow%snow_density_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(160_c_int, actual%snow%snow_depth_limit_building, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(161_c_int, actual%snow%snow_depth_limit_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(162_c_int, actual%snow%snowpack_limit, int(lbound(actual%snow%snowpack_limit, 1), c_int), int(ubound(actual%snow%snowpack_limit, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(163_c_int, actual%snow%snowprof_24hr_working, int(lbound(actual%snow%snowprof_24hr_working, 1), c_int), int(ubound(actual%snow%snowprof_24hr_working, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(164_c_int, actual%snow%snowprof_24hr_holiday, int(lbound(actual%snow%snowprof_24hr_holiday, 1), c_int), int(ubound(actual%snow%snowprof_24hr_holiday, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(165_c_int, actual%snow%tau_a, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(166_c_int, actual%snow%tau_f, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(167_c_int, actual%snow%tau_r, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(168_c_int, actual%snow%temperature_melt_factor, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(169_c_int, actual%snow%radiation_melt_factor, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(170_c_int, actual%snow%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(171_c_int, actual%snow%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(172_c_int, actual%snow%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(173_c_int, actual%snow%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(174_c_int, actual%snow%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(175_c_int, actual%snow%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(176_c_int, actual%snow%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(177_c_int, actual%snow%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(178_c_int, actual%snow%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(179_c_int, actual%snow%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(180_c_int, actual%snow%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(181_c_int, actual%snow%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(182_c_int, actual%snow%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(183_c_int, actual%snow%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(184_c_int, actual%snow%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(185_c_int, actual%snow%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(186_c_int, actual%snow%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(187_c_int, actual%conductance%g_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(188_c_int, actual%conductance%g_k, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(189_c_int, actual%conductance%g_q_base, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(190_c_int, actual%conductance%g_q_shape, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(191_c_int, actual%conductance%g_t, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(192_c_int, actual%conductance%g_sm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(193_c_int, actual%conductance%k_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(194_c_int, actual%conductance%gs_model, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(195_c_int, actual%conductance%s1, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(196_c_int, actual%conductance%s2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(197_c_int, actual%conductance%TH, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(198_c_int, actual%conductance%TL, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(199_c_int, actual%lc_paved%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(200_c_int, actual%lc_paved%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(201_c_int, actual%lc_paved%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(202_c_int, actual%lc_paved%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(203_c_int, actual%lc_paved%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(204_c_int, actual%lc_paved%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(205_c_int, actual%lc_paved%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(206_c_int, actual%lc_paved%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(207_c_int, actual%lc_paved%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(208_c_int, actual%lc_paved%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(209_c_int, actual%lc_paved%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(210_c_int, actual%lc_paved%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(211_c_int, actual%lc_paved%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(212_c_int, actual%lc_paved%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(213_c_int, actual%lc_paved%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(214_c_int, actual%lc_paved%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(215_c_int, actual%lc_paved%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(216_c_int, actual%lc_paved%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(217_c_int, actual%lc_paved%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(218_c_int, actual%lc_paved%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(219_c_int, actual%lc_paved%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(220_c_int, actual%lc_paved%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(221_c_int, actual%lc_paved%state, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(222_c_int, actual%lc_paved%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(223_c_int, actual%lc_paved%irrigation_fraction_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(224_c_int, actual%lc_paved%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(225_c_int, actual%lc_paved%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(226_c_int, actual%lc_paved%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(227_c_int, actual%lc_paved%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(228_c_int, actual%lc_paved%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(229_c_int, actual%lc_paved%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(230_c_int, actual%lc_paved%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(231_c_int, actual%lc_paved%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(232_c_int, actual%lc_paved%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(233_c_int, actual%lc_bldg%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(234_c_int, actual%lc_bldg%fai_building, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(235_c_int, actual%lc_bldg%height_building, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(236_c_int, actual%lc_bldg%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(237_c_int, actual%lc_bldg%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(238_c_int, actual%lc_bldg%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(239_c_int, actual%lc_bldg%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(240_c_int, actual%lc_bldg%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(241_c_int, actual%lc_bldg%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(242_c_int, actual%lc_bldg%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(243_c_int, actual%lc_bldg%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(244_c_int, actual%lc_bldg%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(245_c_int, actual%lc_bldg%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(246_c_int, actual%lc_bldg%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(247_c_int, actual%lc_bldg%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(248_c_int, actual%lc_bldg%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(249_c_int, actual%lc_bldg%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(250_c_int, actual%lc_bldg%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(251_c_int, actual%lc_bldg%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(252_c_int, actual%lc_bldg%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(253_c_int, actual%lc_bldg%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(254_c_int, actual%lc_bldg%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(255_c_int, actual%lc_bldg%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(256_c_int, actual%lc_bldg%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(257_c_int, actual%lc_bldg%state, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(258_c_int, actual%lc_bldg%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(259_c_int, actual%lc_bldg%irrigation_fraction_bldgs, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(260_c_int, actual%lc_bldg%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(261_c_int, actual%lc_bldg%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(262_c_int, actual%lc_bldg%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(263_c_int, actual%lc_bldg%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(264_c_int, actual%lc_bldg%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(265_c_int, actual%lc_bldg%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(266_c_int, actual%lc_bldg%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(267_c_int, actual%lc_bldg%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(268_c_int, actual%lc_bldg%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(269_c_int, actual%lc_dectr%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(270_c_int, actual%lc_dectr%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(271_c_int, actual%lc_dectr%fai_deciduous_tree, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(272_c_int, actual%lc_dectr%height_deciduous_tree, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(273_c_int, actual%lc_dectr%porosity_min_deciduous, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(274_c_int, actual%lc_dectr%porosity_max_deciduous, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(275_c_int, actual%lc_dectr%alb_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(276_c_int, actual%lc_dectr%alb_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(277_c_int, actual%lc_dectr%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(278_c_int, actual%lc_dectr%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(279_c_int, actual%lc_dectr%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(280_c_int, actual%lc_dectr%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(281_c_int, actual%lc_dectr%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(282_c_int, actual%lc_dectr%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(283_c_int, actual%lc_dectr%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(284_c_int, actual%lc_dectr%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(285_c_int, actual%lc_dectr%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(286_c_int, actual%lc_dectr%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(287_c_int, actual%lc_dectr%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(288_c_int, actual%lc_dectr%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(289_c_int, actual%lc_dectr%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(290_c_int, actual%lc_dectr%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(291_c_int, actual%lc_dectr%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(292_c_int, actual%lc_dectr%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(293_c_int, actual%lc_dectr%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(294_c_int, actual%lc_dectr%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(295_c_int, actual%lc_dectr%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(296_c_int, actual%lc_dectr%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(297_c_int, actual%lc_dectr%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(298_c_int, actual%lc_dectr%capacity_max_deciduous, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(299_c_int, actual%lc_dectr%capacity_min_deciduous, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(300_c_int, actual%lc_dectr%irrigation_fraction_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(301_c_int, actual%lc_dectr%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(302_c_int, actual%lc_dectr%bioco2%beta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(303_c_int, actual%lc_dectr%bioco2%beta_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(304_c_int, actual%lc_dectr%bioco2%alpha_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(305_c_int, actual%lc_dectr%bioco2%alpha_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(306_c_int, actual%lc_dectr%bioco2%resp_a, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(307_c_int, actual%lc_dectr%bioco2%resp_b, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(308_c_int, actual%lc_dectr%bioco2%theta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(309_c_int, actual%lc_dectr%bioco2%min_res_bioCO2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(310_c_int, actual%lc_dectr%max_conductance, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(311_c_int, actual%lc_dectr%lai%base_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(312_c_int, actual%lc_dectr%lai%gdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(313_c_int, actual%lc_dectr%lai%base_temperature_senescence, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(314_c_int, actual%lc_dectr%lai%sdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(315_c_int, actual%lc_dectr%lai%lai_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(316_c_int, actual%lc_dectr%lai%lai_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(317_c_int, actual%lc_dectr%lai%lai_power, int(lbound(actual%lc_dectr%lai%lai_power, 1), c_int), int(ubound(actual%lc_dectr%lai%lai_power, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(318_c_int, actual%lc_dectr%lai%lai_type, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(319_c_int, actual%lc_dectr%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(320_c_int, actual%lc_dectr%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(321_c_int, actual%lc_dectr%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(322_c_int, actual%lc_dectr%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(323_c_int, actual%lc_dectr%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(324_c_int, actual%lc_dectr%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(325_c_int, actual%lc_dectr%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(326_c_int, actual%lc_dectr%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(327_c_int, actual%lc_evetr%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(328_c_int, actual%lc_evetr%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(329_c_int, actual%lc_evetr%fai_evergreen_tree, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(330_c_int, actual%lc_evetr%height_evergreen_tree, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(331_c_int, actual%lc_evetr%alb_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(332_c_int, actual%lc_evetr%alb_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(333_c_int, actual%lc_evetr%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(334_c_int, actual%lc_evetr%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(335_c_int, actual%lc_evetr%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(336_c_int, actual%lc_evetr%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(337_c_int, actual%lc_evetr%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(338_c_int, actual%lc_evetr%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(339_c_int, actual%lc_evetr%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(340_c_int, actual%lc_evetr%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(341_c_int, actual%lc_evetr%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(342_c_int, actual%lc_evetr%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(343_c_int, actual%lc_evetr%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(344_c_int, actual%lc_evetr%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(345_c_int, actual%lc_evetr%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(346_c_int, actual%lc_evetr%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(347_c_int, actual%lc_evetr%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(348_c_int, actual%lc_evetr%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(349_c_int, actual%lc_evetr%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(350_c_int, actual%lc_evetr%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(351_c_int, actual%lc_evetr%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(352_c_int, actual%lc_evetr%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(353_c_int, actual%lc_evetr%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(354_c_int, actual%lc_evetr%irrigation_fraction_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(355_c_int, actual%lc_evetr%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(356_c_int, actual%lc_evetr%bioco2%beta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(357_c_int, actual%lc_evetr%bioco2%beta_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(358_c_int, actual%lc_evetr%bioco2%alpha_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(359_c_int, actual%lc_evetr%bioco2%alpha_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(360_c_int, actual%lc_evetr%bioco2%resp_a, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(361_c_int, actual%lc_evetr%bioco2%resp_b, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(362_c_int, actual%lc_evetr%bioco2%theta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(363_c_int, actual%lc_evetr%bioco2%min_res_bioCO2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(364_c_int, actual%lc_evetr%max_conductance, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(365_c_int, actual%lc_evetr%lai%base_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(366_c_int, actual%lc_evetr%lai%gdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(367_c_int, actual%lc_evetr%lai%base_temperature_senescence, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(368_c_int, actual%lc_evetr%lai%sdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(369_c_int, actual%lc_evetr%lai%lai_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(370_c_int, actual%lc_evetr%lai%lai_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(371_c_int, actual%lc_evetr%lai%lai_power, int(lbound(actual%lc_evetr%lai%lai_power, 1), c_int), int(ubound(actual%lc_evetr%lai%lai_power, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(372_c_int, actual%lc_evetr%lai%lai_type, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(373_c_int, actual%lc_evetr%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(374_c_int, actual%lc_evetr%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(375_c_int, actual%lc_evetr%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(376_c_int, actual%lc_evetr%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(377_c_int, actual%lc_evetr%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(378_c_int, actual%lc_evetr%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(379_c_int, actual%lc_evetr%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(380_c_int, actual%lc_evetr%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(381_c_int, actual%lc_grass%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(382_c_int, actual%lc_grass%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(383_c_int, actual%lc_grass%alb_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(384_c_int, actual%lc_grass%alb_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(385_c_int, actual%lc_grass%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(386_c_int, actual%lc_grass%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(387_c_int, actual%lc_grass%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(388_c_int, actual%lc_grass%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(389_c_int, actual%lc_grass%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(390_c_int, actual%lc_grass%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(391_c_int, actual%lc_grass%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(392_c_int, actual%lc_grass%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(393_c_int, actual%lc_grass%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(394_c_int, actual%lc_grass%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(395_c_int, actual%lc_grass%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(396_c_int, actual%lc_grass%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(397_c_int, actual%lc_grass%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(398_c_int, actual%lc_grass%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(399_c_int, actual%lc_grass%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(400_c_int, actual%lc_grass%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(401_c_int, actual%lc_grass%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(402_c_int, actual%lc_grass%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(403_c_int, actual%lc_grass%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(404_c_int, actual%lc_grass%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(405_c_int, actual%lc_grass%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(406_c_int, actual%lc_grass%irrigation_fraction_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(407_c_int, actual%lc_grass%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(408_c_int, actual%lc_grass%bioco2%beta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(409_c_int, actual%lc_grass%bioco2%beta_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(410_c_int, actual%lc_grass%bioco2%alpha_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(411_c_int, actual%lc_grass%bioco2%alpha_enh_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(412_c_int, actual%lc_grass%bioco2%resp_a, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(413_c_int, actual%lc_grass%bioco2%resp_b, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(414_c_int, actual%lc_grass%bioco2%theta_bioco2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(415_c_int, actual%lc_grass%bioco2%min_res_bioCO2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(416_c_int, actual%lc_grass%max_conductance, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(417_c_int, actual%lc_grass%lai%base_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(418_c_int, actual%lc_grass%lai%gdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(419_c_int, actual%lc_grass%lai%base_temperature_senescence, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(420_c_int, actual%lc_grass%lai%sdd_full, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(421_c_int, actual%lc_grass%lai%lai_min, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(422_c_int, actual%lc_grass%lai%lai_max, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(423_c_int, actual%lc_grass%lai%lai_power, int(lbound(actual%lc_grass%lai%lai_power, 1), c_int), int(ubound(actual%lc_grass%lai%lai_power, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(424_c_int, actual%lc_grass%lai%lai_type, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(425_c_int, actual%lc_grass%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(426_c_int, actual%lc_grass%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(427_c_int, actual%lc_grass%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(428_c_int, actual%lc_grass%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(429_c_int, actual%lc_grass%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(430_c_int, actual%lc_grass%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(431_c_int, actual%lc_grass%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(432_c_int, actual%lc_grass%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(433_c_int, actual%lc_bsoil%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(434_c_int, actual%lc_bsoil%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(435_c_int, actual%lc_bsoil%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(436_c_int, actual%lc_bsoil%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(437_c_int, actual%lc_bsoil%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(438_c_int, actual%lc_bsoil%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(439_c_int, actual%lc_bsoil%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(440_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(441_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(442_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(443_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(444_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(445_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(446_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(447_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(448_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(449_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(450_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(451_c_int, actual%lc_bsoil%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(452_c_int, actual%lc_bsoil%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(453_c_int, actual%lc_bsoil%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(454_c_int, actual%lc_bsoil%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(455_c_int, actual%lc_bsoil%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(456_c_int, actual%lc_bsoil%irrigation_fraction_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(457_c_int, actual%lc_bsoil%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(458_c_int, actual%lc_bsoil%waterdist%to_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(459_c_int, actual%lc_bsoil%waterdist%to_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(460_c_int, actual%lc_bsoil%waterdist%to_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(461_c_int, actual%lc_bsoil%waterdist%to_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(462_c_int, actual%lc_bsoil%waterdist%to_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(463_c_int, actual%lc_bsoil%waterdist%to_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(464_c_int, actual%lc_bsoil%waterdist%to_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(465_c_int, actual%lc_bsoil%waterdist%to_soilstore, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(466_c_int, actual%lc_water%sfr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(467_c_int, actual%lc_water%emis, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(468_c_int, actual%lc_water%ohm%ch_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(469_c_int, actual%lc_water%ohm%cp_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(470_c_int, actual%lc_water%ohm%kk_anohm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(471_c_int, actual%lc_water%ohm%ohm_threshsw, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(472_c_int, actual%lc_water%ohm%ohm_threshwd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(473_c_int, actual%lc_water%ohm%ohm_coef_lc(1)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(474_c_int, actual%lc_water%ohm%ohm_coef_lc(1)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(475_c_int, actual%lc_water%ohm%ohm_coef_lc(1)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(476_c_int, actual%lc_water%ohm%ohm_coef_lc(1)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(477_c_int, actual%lc_water%ohm%ohm_coef_lc(2)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(478_c_int, actual%lc_water%ohm%ohm_coef_lc(2)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(479_c_int, actual%lc_water%ohm%ohm_coef_lc(2)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(480_c_int, actual%lc_water%ohm%ohm_coef_lc(2)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(481_c_int, actual%lc_water%ohm%ohm_coef_lc(3)%summer_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(482_c_int, actual%lc_water%ohm%ohm_coef_lc(3)%summer_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(483_c_int, actual%lc_water%ohm%ohm_coef_lc(3)%winter_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(484_c_int, actual%lc_water%ohm%ohm_coef_lc(3)%winter_wet, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(485_c_int, actual%lc_water%soil%soil_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(486_c_int, actual%lc_water%soil%soil_store_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(487_c_int, actual%lc_water%soil%saturated_hydraulic_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(488_c_int, actual%lc_water%state_limit, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(489_c_int, actual%lc_water%irrigation_fraction_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(490_c_int, actual%lc_water%wet_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(491_c_int, actual%lc_water%flow_change, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(492_c_int, actual%building_archtype%building_count, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(493_c_int, actual%building_archtype%occupants_state, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(494_c_int, actual%building_archtype%hhs0, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(495_c_int, actual%building_archtype%age_0_4, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(496_c_int, actual%building_archtype%age_5_11, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(497_c_int, actual%building_archtype%age_12_18, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(498_c_int, actual%building_archtype%age_19_64, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(499_c_int, actual%building_archtype%age_65plus, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(500_c_int, actual%building_archtype%stebbs_Height, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(501_c_int, actual%building_archtype%footprint_area, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(502_c_int, actual%building_archtype%wall_external_area, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(503_c_int, actual%building_archtype%internal_volume_ratio, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(504_c_int, actual%building_archtype%internal_mass_area, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(505_c_int, actual%building_archtype%WWR, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(506_c_int, actual%building_archtype%wall_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(507_c_int, actual%building_archtype%wall_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(508_c_int, actual%building_archtype%wall_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(509_c_int, actual%building_archtype%wall_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(510_c_int, actual%building_archtype%wall_external_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(511_c_int, actual%building_archtype%wall_external_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(512_c_int, actual%building_archtype%wall_external_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(513_c_int, actual%building_archtype%wall_external_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(514_c_int, actual%building_archtype%wall_outer_heat_capacity_fraction, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(515_c_int, actual%building_archtype%wall_external_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(516_c_int, actual%building_archtype%wall_internal_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(517_c_int, actual%building_archtype%wall_transmissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(518_c_int, actual%building_archtype%wall_absorptivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(519_c_int, actual%building_archtype%wall_reflectivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(520_c_int, actual%building_archtype%roof_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(521_c_int, actual%building_archtype%roof_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(522_c_int, actual%building_archtype%roof_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(523_c_int, actual%building_archtype%roof_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(524_c_int, actual%building_archtype%roof_external_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(525_c_int, actual%building_archtype%roof_external_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(526_c_int, actual%building_archtype%roof_external_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(527_c_int, actual%building_archtype%roof_external_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(528_c_int, actual%building_archtype%roof_outer_heat_capacity_fraction, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(529_c_int, actual%building_archtype%roof_external_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(530_c_int, actual%building_archtype%roof_internal_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(531_c_int, actual%building_archtype%roof_transmissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(532_c_int, actual%building_archtype%roof_absorptivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(533_c_int, actual%building_archtype%roof_reflectivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(534_c_int, actual%building_archtype%ground_floor_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(535_c_int, actual%building_archtype%ground_floor_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(536_c_int, actual%building_archtype%ground_floor_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(537_c_int, actual%building_archtype%ground_floor_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(538_c_int, actual%building_archtype%window_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(539_c_int, actual%building_archtype%window_effective_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(540_c_int, actual%building_archtype%window_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(541_c_int, actual%building_archtype%window_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(542_c_int, actual%building_archtype%window_external_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(543_c_int, actual%building_archtype%window_internal_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(544_c_int, actual%building_archtype%window_transmissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(545_c_int, actual%building_archtype%window_absorptivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(546_c_int, actual%building_archtype%window_reflectivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(547_c_int, actual%building_archtype%internal_mass_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(548_c_int, actual%building_archtype%internal_mass_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(549_c_int, actual%building_archtype%internal_mass_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(550_c_int, actual%building_archtype%max_heating_power, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(551_c_int, actual%building_archtype%hot_water_tank_volume, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(552_c_int, actual%building_archtype%maximum_hot_water_heating_power, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(553_c_int, actual%building_archtype%heating_setpoint_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(554_c_int, actual%building_archtype%cooling_setpoint_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(555_c_int, actual%building_archtype%heating_setpoint_temperature_profile, int(lbound(actual%building_archtype%heating_setpoint_temperature_profile, 1), c_int), int(ubound(actual%building_archtype%heating_setpoint_temperature_profile, 1), c_int), int(lbound(actual%building_archtype%heating_setpoint_temperature_profile, 2), c_int), int(ubound(actual%building_archtype%heating_setpoint_temperature_profile, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(556_c_int, actual%building_archtype%cooling_setpoint_temperature_profile, int(lbound(actual%building_archtype%cooling_setpoint_temperature_profile, 1), c_int), int(ubound(actual%building_archtype%cooling_setpoint_temperature_profile, 1), c_int), int(lbound(actual%building_archtype%cooling_setpoint_temperature_profile, 2), c_int), int(ubound(actual%building_archtype%cooling_setpoint_temperature_profile, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(557_c_int, actual%building_archtype%lighting_power_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(558_c_int, actual%building_archtype%metabolism_profile, int(lbound(actual%building_archtype%metabolism_profile, 1), c_int), int(ubound(actual%building_archtype%metabolism_profile, 1), c_int), int(lbound(actual%building_archtype%metabolism_profile, 2), c_int), int(ubound(actual%building_archtype%metabolism_profile, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(559_c_int, actual%building_archtype%appliance_profile, int(lbound(actual%building_archtype%appliance_profile, 1), c_int), int(ubound(actual%building_archtype%appliance_profile, 1), c_int), int(lbound(actual%building_archtype%appliance_profile, 2), c_int), int(ubound(actual%building_archtype%appliance_profile, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(560_c_int, actual%building_archtype%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(561_c_int, actual%stebbs%wall_internal_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(562_c_int, actual%stebbs%roof_internal_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(563_c_int, actual%stebbs%internal_mass_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(564_c_int, actual%stebbs%floor_internal_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(565_c_int, actual%stebbs%window_internal_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(566_c_int, actual%stebbs%wall_external_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(567_c_int, actual%stebbs%roof_external_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(568_c_int, actual%stebbs%window_external_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(569_c_int, actual%stebbs%ground_depth, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(570_c_int, actual%stebbs%external_ground_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(571_c_int, actual%stebbs%indoor_air_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(572_c_int, actual%stebbs%indoor_air_cp, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(573_c_int, actual%stebbs%metabolism_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(574_c_int, actual%stebbs%latent_sensible_ratio, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(575_c_int, actual%stebbs%daylight_control, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(576_c_int, actual%stebbs%lighting_illuminance_threshold, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(577_c_int, actual%stebbs%internal_shading, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(578_c_int, actual%stebbs%reduction_factor_shading, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(579_c_int, actual%stebbs%temperature_threshold_shading, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(580_c_int, actual%stebbs%radiation_threshold_shading, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(581_c_int, actual%stebbs%heating_system_efficiency, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(582_c_int, actual%stebbs%destination_waste_heat, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(583_c_int, actual%stebbs%fraction_convective_heating, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(584_c_int, actual%stebbs%max_cooling_power, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(585_c_int, actual%stebbs%cooling_system_cop, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(586_c_int, actual%stebbs%ventilation_rate, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(587_c_int, actual%stebbs%hot_water_tank_wall_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(588_c_int, actual%stebbs%hot_water_tank_surface_area, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(589_c_int, actual%stebbs%hot_water_heating_setpoint_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(590_c_int, actual%stebbs%hot_water_tank_wall_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(591_c_int, actual%stebbs%hot_water_vessel_wall_thickness, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(592_c_int, actual%stebbs%hot_water_volume, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(593_c_int, actual%stebbs%hot_water_surface_area, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(594_c_int, actual%stebbs%hot_water_flow_rate, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(595_c_int, actual%stebbs%hot_water_flow_profile, int(lbound(actual%stebbs%hot_water_flow_profile, 1), c_int), int(ubound(actual%stebbs%hot_water_flow_profile, 1), c_int), int(lbound(actual%stebbs%hot_water_flow_profile, 2), c_int), int(ubound(actual%stebbs%hot_water_flow_profile, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(596_c_int, actual%stebbs%hot_water_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(597_c_int, actual%stebbs%hot_water_tank_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(598_c_int, actual%stebbs%hot_water_vessel_specific_heat_capacity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(599_c_int, actual%stebbs%hot_water_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(600_c_int, actual%stebbs%hot_water_tank_wall_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(601_c_int, actual%stebbs%hot_water_vessel_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(602_c_int, actual%stebbs%hot_water_tank_wall_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(603_c_int, actual%stebbs%hot_water_tank_internal_wall_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(604_c_int, actual%stebbs%hot_water_tank_external_wall_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(605_c_int, actual%stebbs%hot_water_vessel_wall_conductivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(606_c_int, actual%stebbs%hot_water_vessel_internal_wall_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(607_c_int, actual%stebbs%hot_water_vessel_external_wall_convection_coefficient, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(608_c_int, actual%stebbs%hot_water_vessel_wall_emissivity, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(609_c_int, actual%stebbs%hot_water_heating_efficiency, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(610_c_int, actual%stebbs%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_suews_site

   subroutine suews_obs_unpack_suews_state( &
      state_flat, state_flat_len, state_toc, state_toc_len, &
      state_member_count, nlayer, ndepth, &
      out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, &
      n_meta, cap, n_out, err) bind(C, name='suews_obs_unpack_suews_state')
      implicit none
      real(c_double), intent(in) :: state_flat(*)
      integer(c_int), value, intent(in) :: state_flat_len
      integer(c_int), intent(in) :: state_toc(*)
      integer(c_int), value, intent(in) :: state_toc_len
      integer(c_int), value, intent(in) :: state_member_count
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
      type(SUEWS_STATE) :: actual
      integer(c_int) :: pos
      integer(c_int) :: unpack_err

      n_out = 0_c_int
      pos = 0_c_int
      unpack_err = SUEWS_CAPI_OK
      err = SUEWS_CAPI_OK
      if (n_meta < 308_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call unpack_state_members( &
         actual, int(nlayer), int(ndepth), &
         state_flat, state_flat_len, state_toc, state_toc_len, state_member_count, unpack_err)
      if (unpack_err /= SUEWS_CAPI_OK) then
         err = unpack_err
         return
      end if

      call obs_emit_unallocated(1_c_int, out_meta, n_meta, err)
      call obs_emit_logical_scalar(2_c_int, actual%flagState%flag_converge, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(3_c_int, actual%flagState%i_iter, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(4_c_int, actual%flagState%stebbs_bldg_init, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(5_c_int, actual%flagState%snow_warning_shown, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(6_c_int, actual%flagState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(7_c_int, actual%anthroemisState%HDD_id, int(lbound(actual%anthroemisState%HDD_id, 1), c_int), int(ubound(actual%anthroemisState%HDD_id, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(8_c_int, actual%anthroemisState%Fc, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(9_c_int, actual%anthroemisState%Fc_anthro, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(10_c_int, actual%anthroemisState%Fc_biogen, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(11_c_int, actual%anthroemisState%Fc_build, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(12_c_int, actual%anthroemisState%Fc_metab, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(13_c_int, actual%anthroemisState%Fc_photo, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(14_c_int, actual%anthroemisState%Fc_point, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(15_c_int, actual%anthroemisState%Fc_respi, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(16_c_int, actual%anthroemisState%Fc_traff, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(17_c_int, actual%anthroemisState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(18_c_int, actual%ohmState%qn_av, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(19_c_int, actual%ohmState%dqndt, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(20_c_int, actual%ohmState%qn_surfs, int(lbound(actual%ohmState%qn_surfs, 1), c_int), int(ubound(actual%ohmState%qn_surfs, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(21_c_int, actual%ohmState%dqndt_surf, int(lbound(actual%ohmState%dqndt_surf, 1), c_int), int(ubound(actual%ohmState%dqndt_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(22_c_int, actual%ohmState%qn_s_av, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(23_c_int, actual%ohmState%dqnsdt, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(24_c_int, actual%ohmState%a1, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(25_c_int, actual%ohmState%a2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(26_c_int, actual%ohmState%a3, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(27_c_int, actual%ohmState%t2_prev, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(28_c_int, actual%ohmState%ws_rav, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(29_c_int, actual%ohmState%qn_rav, int(lbound(actual%ohmState%qn_rav, 1), c_int), int(ubound(actual%ohmState%qn_rav, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(30_c_int, actual%ohmState%a1_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(31_c_int, actual%ohmState%a2_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(32_c_int, actual%ohmState%a3_bldg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(33_c_int, actual%ohmState%a1_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(34_c_int, actual%ohmState%a2_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(35_c_int, actual%ohmState%a3_paved, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(36_c_int, actual%ohmState%a1_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(37_c_int, actual%ohmState%a2_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(38_c_int, actual%ohmState%a3_evetr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(39_c_int, actual%ohmState%a1_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(40_c_int, actual%ohmState%a2_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(41_c_int, actual%ohmState%a3_dectr, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(42_c_int, actual%ohmState%a1_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(43_c_int, actual%ohmState%a2_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(44_c_int, actual%ohmState%a3_grass, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(45_c_int, actual%ohmState%a1_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(46_c_int, actual%ohmState%a2_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(47_c_int, actual%ohmState%a3_bsoil, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(48_c_int, actual%ohmState%a1_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(49_c_int, actual%ohmState%a2_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(50_c_int, actual%ohmState%a3_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(51_c_int, actual%ohmState%anohm_working_day, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(52_c_int, actual%ohmState%anohm_working_count, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(53_c_int, actual%ohmState%anohm_working_tHr, int(lbound(actual%ohmState%anohm_working_tHr, 1), c_int), int(ubound(actual%ohmState%anohm_working_tHr, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(54_c_int, actual%ohmState%anohm_working_sd, int(lbound(actual%ohmState%anohm_working_sd, 1), c_int), int(ubound(actual%ohmState%anohm_working_sd, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(55_c_int, actual%ohmState%anohm_working_ta, int(lbound(actual%ohmState%anohm_working_ta, 1), c_int), int(ubound(actual%ohmState%anohm_working_ta, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(56_c_int, actual%ohmState%anohm_working_rh, int(lbound(actual%ohmState%anohm_working_rh, 1), c_int), int(ubound(actual%ohmState%anohm_working_rh, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(57_c_int, actual%ohmState%anohm_working_pres, int(lbound(actual%ohmState%anohm_working_pres, 1), c_int), int(ubound(actual%ohmState%anohm_working_pres, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(58_c_int, actual%ohmState%anohm_working_ws, int(lbound(actual%ohmState%anohm_working_ws, 1), c_int), int(ubound(actual%ohmState%anohm_working_ws, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(59_c_int, actual%ohmState%anohm_working_ah, int(lbound(actual%ohmState%anohm_working_ah, 1), c_int), int(ubound(actual%ohmState%anohm_working_ah, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(60_c_int, actual%ohmState%anohm_coeff_day, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_int_scalar(61_c_int, actual%ohmState%anohm_coeff_count, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(62_c_int, actual%ohmState%anohm_coeff_ready, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(63_c_int, actual%ohmState%anohm_coeff_tHr, int(lbound(actual%ohmState%anohm_coeff_tHr, 1), c_int), int(ubound(actual%ohmState%anohm_coeff_tHr, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(64_c_int, actual%ohmState%anohm_coeff_sd, int(lbound(actual%ohmState%anohm_coeff_sd, 1), c_int), int(ubound(actual%ohmState%anohm_coeff_sd, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(65_c_int, actual%ohmState%anohm_coeff_ta, int(lbound(actual%ohmState%anohm_coeff_ta, 1), c_int), int(ubound(actual%ohmState%anohm_coeff_ta, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(66_c_int, actual%ohmState%anohm_coeff_rh, int(lbound(actual%ohmState%anohm_coeff_rh, 1), c_int), int(ubound(actual%ohmState%anohm_coeff_rh, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(67_c_int, actual%ohmState%anohm_coeff_pres, int(lbound(actual%ohmState%anohm_coeff_pres, 1), c_int), int(ubound(actual%ohmState%anohm_coeff_pres, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(68_c_int, actual%ohmState%anohm_coeff_ws, int(lbound(actual%ohmState%anohm_coeff_ws, 1), c_int), int(ubound(actual%ohmState%anohm_coeff_ws, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(69_c_int, actual%ohmState%anohm_coeff_ah, int(lbound(actual%ohmState%anohm_coeff_ah, 1), c_int), int(ubound(actual%ohmState%anohm_coeff_ah, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(70_c_int, actual%ohmState%anohm_a1_surf, int(lbound(actual%ohmState%anohm_a1_surf, 1), c_int), int(ubound(actual%ohmState%anohm_a1_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(71_c_int, actual%ohmState%anohm_a2_surf, int(lbound(actual%ohmState%anohm_a2_surf, 1), c_int), int(ubound(actual%ohmState%anohm_a2_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(72_c_int, actual%ohmState%anohm_a3_surf, int(lbound(actual%ohmState%anohm_a3_surf, 1), c_int), int(ubound(actual%ohmState%anohm_a3_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(73_c_int, actual%ohmState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(74_c_int, actual%solarState%azimuth_deg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(75_c_int, actual%solarState%zenith_deg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(76_c_int, actual%solarState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(77_c_int, actual%atmState%f_cloud, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(78_c_int, actual%atmState%av_cp, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(79_c_int, actual%atmState%dens_dry, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(80_c_int, actual%atmState%av_density, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(81_c_int, actual%atmState%dq, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(82_c_int, actual%atmState%Ea_hPa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(83_c_int, actual%atmState%Es_hPa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(84_c_int, actual%atmState%lv_J_kg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(85_c_int, actual%atmState%lv_s_j_kg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(86_c_int, actual%atmState%tlv, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(87_c_int, actual%atmState%psyc_hPa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(88_c_int, actual%atmState%psyc_ice_h_pa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(89_c_int, actual%atmState%s_Pa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(90_c_int, actual%atmState%s_hpa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(91_c_int, actual%atmState%s_ice_hpa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(92_c_int, actual%atmState%vpd_hPa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(93_c_int, actual%atmState%vpd_pa, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(94_c_int, actual%atmState%U10_ms, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(95_c_int, actual%atmState%U_hbh, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(96_c_int, actual%atmState%T2_C, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(97_c_int, actual%atmState%T_half_bldg_C, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(98_c_int, actual%atmState%q2_gkg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(99_c_int, actual%atmState%RH2, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(100_c_int, actual%atmState%L_mod, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(101_c_int, actual%atmState%z_l, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(102_c_int, actual%atmState%RA_h, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(103_c_int, actual%atmState%RS, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(104_c_int, actual%atmState%u_star, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(105_c_int, actual%atmState%t_star, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(106_c_int, actual%atmState%RB, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(107_c_int, actual%atmState%Tair_av, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(108_c_int, actual%atmState%rss_surf, int(lbound(actual%atmState%rss_surf, 1), c_int), int(ubound(actual%atmState%rss_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(109_c_int, actual%atmState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(110_c_int, actual%phenState%alb, int(lbound(actual%phenState%alb, 1), c_int), int(ubound(actual%phenState%alb, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(111_c_int, actual%phenState%lai_id, int(lbound(actual%phenState%lai_id, 1), c_int), int(ubound(actual%phenState%lai_id, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(112_c_int, actual%phenState%GDD_id, int(lbound(actual%phenState%GDD_id, 1), c_int), int(ubound(actual%phenState%GDD_id, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(113_c_int, actual%phenState%SDD_id, int(lbound(actual%phenState%SDD_id, 1), c_int), int(ubound(actual%phenState%SDD_id, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(114_c_int, actual%phenState%veg_phen_lumps, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(115_c_int, actual%phenState%porosity_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(116_c_int, actual%phenState%decidcap_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(117_c_int, actual%phenState%albDecTr_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(118_c_int, actual%phenState%albEveTr_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(119_c_int, actual%phenState%albGrass_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(120_c_int, actual%phenState%Tmin_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(121_c_int, actual%phenState%Tmax_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(122_c_int, actual%phenState%lenDay_id, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(123_c_int, actual%phenState%temp_veg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_2d(124_c_int, actual%phenState%storage_drain_params, int(lbound(actual%phenState%storage_drain_params, 1), c_int), int(ubound(actual%phenState%storage_drain_params, 1), c_int), int(lbound(actual%phenState%storage_drain_params, 2), c_int), int(ubound(actual%phenState%storage_drain_params, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(125_c_int, actual%phenState%g_func, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(126_c_int, actual%phenState%gsc, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(127_c_int, actual%phenState%g_kdown, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(128_c_int, actual%phenState%g_dq, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(129_c_int, actual%phenState%g_ta, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(130_c_int, actual%phenState%g_smd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(131_c_int, actual%phenState%g_lai, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(132_c_int, actual%phenState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(133_c_int, actual%snowState%snowfall_cum, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(134_c_int, actual%snowState%snow_albedo, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(135_c_int, actual%snowState%chSnow_per_interval, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(136_c_int, actual%snowState%mwh, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(137_c_int, actual%snowState%melt_water_store, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(138_c_int, actual%snowState%qn_snow, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(139_c_int, actual%snowState%qm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(140_c_int, actual%snowState%qm_freeze, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(141_c_int, actual%snowState%qm_rain, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(142_c_int, actual%snowState%swe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(143_c_int, actual%snowState%z0v_snow, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(144_c_int, actual%snowState%ra_snow, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(145_c_int, actual%snowState%snow_removal, int(lbound(actual%snowState%snow_removal, 1), c_int), int(ubound(actual%snowState%snow_removal, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(146_c_int, actual%snowState%ice_frac, int(lbound(actual%snowState%ice_frac, 1), c_int), int(ubound(actual%snowState%ice_frac, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(147_c_int, actual%snowState%snow_density, int(lbound(actual%snowState%snow_density, 1), c_int), int(ubound(actual%snowState%snow_density, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(148_c_int, actual%snowState%snow_fraction, int(lbound(actual%snowState%snow_fraction, 1), c_int), int(ubound(actual%snowState%snow_fraction, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(149_c_int, actual%snowState%snow_pack, int(lbound(actual%snowState%snow_pack, 1), c_int), int(ubound(actual%snowState%snow_pack, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(150_c_int, actual%snowState%snow_water, int(lbound(actual%snowState%snow_water, 1), c_int), int(ubound(actual%snowState%snow_water, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(151_c_int, actual%snowState%kup_ind_snow, int(lbound(actual%snowState%kup_ind_snow, 1), c_int), int(ubound(actual%snowState%kup_ind_snow, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(152_c_int, actual%snowState%qn_ind_snow, int(lbound(actual%snowState%qn_ind_snow, 1), c_int), int(ubound(actual%snowState%qn_ind_snow, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(153_c_int, actual%snowState%delta_qi, int(lbound(actual%snowState%delta_qi, 1), c_int), int(ubound(actual%snowState%delta_qi, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(154_c_int, actual%snowState%Tsurf_ind_snow, int(lbound(actual%snowState%Tsurf_ind_snow, 1), c_int), int(ubound(actual%snowState%Tsurf_ind_snow, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(155_c_int, actual%snowState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(156_c_int, actual%hydroState%soil_store_surf, int(lbound(actual%hydroState%soil_store_surf, 1), c_int), int(ubound(actual%hydroState%soil_store_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(157_c_int, actual%hydroState%state_surf, int(lbound(actual%hydroState%state_surf, 1), c_int), int(ubound(actual%hydroState%state_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(158_c_int, actual%hydroState%WUDay_id, int(lbound(actual%hydroState%WUDay_id, 1), c_int), int(ubound(actual%hydroState%WUDay_id, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      if (allocated(actual%hydroState%soil_store_roof)) then
         call obs_emit_real_1d(159_c_int, actual%hydroState%soil_store_roof, int(lbound(actual%hydroState%soil_store_roof, 1), c_int), int(ubound(actual%hydroState%soil_store_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(159_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%hydroState%state_roof)) then
         call obs_emit_real_1d(160_c_int, actual%hydroState%state_roof, int(lbound(actual%hydroState%state_roof, 1), c_int), int(ubound(actual%hydroState%state_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(160_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%hydroState%soil_store_wall)) then
         call obs_emit_real_1d(161_c_int, actual%hydroState%soil_store_wall, int(lbound(actual%hydroState%soil_store_wall, 1), c_int), int(ubound(actual%hydroState%soil_store_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(161_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%hydroState%state_wall)) then
         call obs_emit_real_1d(162_c_int, actual%hydroState%state_wall, int(lbound(actual%hydroState%state_wall, 1), c_int), int(ubound(actual%hydroState%state_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(162_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%hydroState%ev_roof)) then
         call obs_emit_real_1d(163_c_int, actual%hydroState%ev_roof, int(lbound(actual%hydroState%ev_roof, 1), c_int), int(ubound(actual%hydroState%ev_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(163_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%hydroState%ev_wall)) then
         call obs_emit_real_1d(164_c_int, actual%hydroState%ev_wall, int(lbound(actual%hydroState%ev_wall, 1), c_int), int(ubound(actual%hydroState%ev_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(164_c_int, out_meta, n_meta, err)
      end if
      call obs_emit_real_1d(165_c_int, actual%hydroState%ev0_surf, int(lbound(actual%hydroState%ev0_surf, 1), c_int), int(ubound(actual%hydroState%ev0_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(166_c_int, actual%hydroState%ev_surf, int(lbound(actual%hydroState%ev_surf, 1), c_int), int(ubound(actual%hydroState%ev_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(167_c_int, actual%hydroState%wu_surf, int(lbound(actual%hydroState%wu_surf, 1), c_int), int(ubound(actual%hydroState%wu_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(168_c_int, actual%hydroState%runoff_soil, int(lbound(actual%hydroState%runoff_soil, 1), c_int), int(ubound(actual%hydroState%runoff_soil, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(169_c_int, actual%hydroState%smd_surf, int(lbound(actual%hydroState%smd_surf, 1), c_int), int(ubound(actual%hydroState%smd_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(170_c_int, actual%hydroState%drain_surf, int(lbound(actual%hydroState%drain_surf, 1), c_int), int(ubound(actual%hydroState%drain_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(171_c_int, actual%hydroState%drain_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(172_c_int, actual%hydroState%ev_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(173_c_int, actual%hydroState%wu_ext, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(174_c_int, actual%hydroState%wu_int, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(175_c_int, actual%hydroState%runoff_ag_veg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(176_c_int, actual%hydroState%runoff_ag_impervious, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(177_c_int, actual%hydroState%runoff_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(178_c_int, actual%hydroState%runoff_pipes, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(179_c_int, actual%hydroState%runoff_soil_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(180_c_int, actual%hydroState%runoff_waterbody, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(181_c_int, actual%hydroState%smd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(182_c_int, actual%hydroState%soil_state, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(183_c_int, actual%hydroState%state_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(184_c_int, actual%hydroState%surf_chang_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(185_c_int, actual%hydroState%tot_chang_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(186_c_int, actual%hydroState%runoff_per_interval, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(187_c_int, actual%hydroState%nw_state_per_tstep, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(188_c_int, actual%hydroState%soil_moist_cap, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(189_c_int, actual%hydroState%vsmd, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(190_c_int, actual%hydroState%additional_water, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(191_c_int, actual%hydroState%add_impervious, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(192_c_int, actual%hydroState%add_pipes, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(193_c_int, actual%hydroState%add_veg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(194_c_int, actual%hydroState%add_water_body, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(195_c_int, actual%hydroState%add_water, int(lbound(actual%hydroState%add_water, 1), c_int), int(ubound(actual%hydroState%add_water, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(196_c_int, actual%hydroState%frac_water2runoff, int(lbound(actual%hydroState%frac_water2runoff, 1), c_int), int(ubound(actual%hydroState%frac_water2runoff, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(197_c_int, actual%hydroState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      if (allocated(actual%heatState%temp_roof)) then
         call obs_emit_real_2d(198_c_int, actual%heatState%temp_roof, int(lbound(actual%heatState%temp_roof, 1), c_int), int(ubound(actual%heatState%temp_roof, 1), c_int), int(lbound(actual%heatState%temp_roof, 2), c_int), int(ubound(actual%heatState%temp_roof, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(198_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%temp_wall)) then
         call obs_emit_real_2d(199_c_int, actual%heatState%temp_wall, int(lbound(actual%heatState%temp_wall, 1), c_int), int(ubound(actual%heatState%temp_wall, 1), c_int), int(lbound(actual%heatState%temp_wall, 2), c_int), int(ubound(actual%heatState%temp_wall, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(199_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%temp_surf)) then
         call obs_emit_real_2d(200_c_int, actual%heatState%temp_surf, int(lbound(actual%heatState%temp_surf, 1), c_int), int(ubound(actual%heatState%temp_surf, 1), c_int), int(lbound(actual%heatState%temp_surf, 2), c_int), int(ubound(actual%heatState%temp_surf, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(200_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%temp_surf_dyohm)) then
         call obs_emit_real_2d(201_c_int, actual%heatState%temp_surf_dyohm, int(lbound(actual%heatState%temp_surf_dyohm, 1), c_int), int(ubound(actual%heatState%temp_surf_dyohm, 1), c_int), int(lbound(actual%heatState%temp_surf_dyohm, 2), c_int), int(ubound(actual%heatState%temp_surf_dyohm, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(201_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%temp_surf_ehc_fast)) then
         call obs_emit_real_2d(202_c_int, actual%heatState%temp_surf_ehc_fast, int(lbound(actual%heatState%temp_surf_ehc_fast, 1), c_int), int(ubound(actual%heatState%temp_surf_ehc_fast, 1), c_int), int(lbound(actual%heatState%temp_surf_ehc_fast, 2), c_int), int(ubound(actual%heatState%temp_surf_ehc_fast, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(202_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%temp_surf_ehc_slow)) then
         call obs_emit_real_2d(203_c_int, actual%heatState%temp_surf_ehc_slow, int(lbound(actual%heatState%temp_surf_ehc_slow, 1), c_int), int(ubound(actual%heatState%temp_surf_ehc_slow, 1), c_int), int(lbound(actual%heatState%temp_surf_ehc_slow, 2), c_int), int(ubound(actual%heatState%temp_surf_ehc_slow, 2), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(203_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%tsfc_roof)) then
         call obs_emit_real_1d(204_c_int, actual%heatState%tsfc_roof, int(lbound(actual%heatState%tsfc_roof, 1), c_int), int(ubound(actual%heatState%tsfc_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(204_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%tsfc_wall)) then
         call obs_emit_real_1d(205_c_int, actual%heatState%tsfc_wall, int(lbound(actual%heatState%tsfc_wall, 1), c_int), int(ubound(actual%heatState%tsfc_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(205_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%tsfc_surf)) then
         call obs_emit_real_1d(206_c_int, actual%heatState%tsfc_surf, int(lbound(actual%heatState%tsfc_surf, 1), c_int), int(ubound(actual%heatState%tsfc_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(206_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%tsfc_surf_dyohm)) then
         call obs_emit_real_1d(207_c_int, actual%heatState%tsfc_surf_dyohm, int(lbound(actual%heatState%tsfc_surf_dyohm, 1), c_int), int(ubound(actual%heatState%tsfc_surf_dyohm, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(207_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%tsfc_roof_stepstart)) then
         call obs_emit_real_1d(208_c_int, actual%heatState%tsfc_roof_stepstart, int(lbound(actual%heatState%tsfc_roof_stepstart, 1), c_int), int(ubound(actual%heatState%tsfc_roof_stepstart, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(208_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%tsfc_wall_stepstart)) then
         call obs_emit_real_1d(209_c_int, actual%heatState%tsfc_wall_stepstart, int(lbound(actual%heatState%tsfc_wall_stepstart, 1), c_int), int(ubound(actual%heatState%tsfc_wall_stepstart, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(209_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%tsfc_surf_stepstart)) then
         call obs_emit_real_1d(210_c_int, actual%heatState%tsfc_surf_stepstart, int(lbound(actual%heatState%tsfc_surf_stepstart, 1), c_int), int(ubound(actual%heatState%tsfc_surf_stepstart, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(210_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%QS_roof)) then
         call obs_emit_real_1d(211_c_int, actual%heatState%QS_roof, int(lbound(actual%heatState%QS_roof, 1), c_int), int(ubound(actual%heatState%QS_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(211_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%QN_roof)) then
         call obs_emit_real_1d(212_c_int, actual%heatState%QN_roof, int(lbound(actual%heatState%QN_roof, 1), c_int), int(ubound(actual%heatState%QN_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(212_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%qe_roof)) then
         call obs_emit_real_1d(213_c_int, actual%heatState%qe_roof, int(lbound(actual%heatState%qe_roof, 1), c_int), int(ubound(actual%heatState%qe_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(213_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%qh_roof)) then
         call obs_emit_real_1d(214_c_int, actual%heatState%qh_roof, int(lbound(actual%heatState%qh_roof, 1), c_int), int(ubound(actual%heatState%qh_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(214_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%qh_resist_roof)) then
         call obs_emit_real_1d(215_c_int, actual%heatState%qh_resist_roof, int(lbound(actual%heatState%qh_resist_roof, 1), c_int), int(ubound(actual%heatState%qh_resist_roof, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(215_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%QS_wall)) then
         call obs_emit_real_1d(216_c_int, actual%heatState%QS_wall, int(lbound(actual%heatState%QS_wall, 1), c_int), int(ubound(actual%heatState%QS_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(216_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%QN_wall)) then
         call obs_emit_real_1d(217_c_int, actual%heatState%QN_wall, int(lbound(actual%heatState%QN_wall, 1), c_int), int(ubound(actual%heatState%QN_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(217_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%qe_wall)) then
         call obs_emit_real_1d(218_c_int, actual%heatState%qe_wall, int(lbound(actual%heatState%qe_wall, 1), c_int), int(ubound(actual%heatState%qe_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(218_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%qh_wall)) then
         call obs_emit_real_1d(219_c_int, actual%heatState%qh_wall, int(lbound(actual%heatState%qh_wall, 1), c_int), int(ubound(actual%heatState%qh_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(219_c_int, out_meta, n_meta, err)
      end if
      if (allocated(actual%heatState%qh_resist_wall)) then
         call obs_emit_real_1d(220_c_int, actual%heatState%qh_resist_wall, int(lbound(actual%heatState%qh_resist_wall, 1), c_int), int(ubound(actual%heatState%qh_resist_wall, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      else
         call obs_emit_unallocated(220_c_int, out_meta, n_meta, err)
      end if
      call obs_emit_real_1d(221_c_int, actual%heatState%qs_surf, int(lbound(actual%heatState%qs_surf, 1), c_int), int(ubound(actual%heatState%qs_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(222_c_int, actual%heatState%QN_surf, int(lbound(actual%heatState%QN_surf, 1), c_int), int(ubound(actual%heatState%QN_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(223_c_int, actual%heatState%qe0_surf, int(lbound(actual%heatState%qe0_surf, 1), c_int), int(ubound(actual%heatState%qe0_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(224_c_int, actual%heatState%qe_surf, int(lbound(actual%heatState%qe_surf, 1), c_int), int(ubound(actual%heatState%qe_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(225_c_int, actual%heatState%qh_surf, int(lbound(actual%heatState%qh_surf, 1), c_int), int(ubound(actual%heatState%qh_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(226_c_int, actual%heatState%qh_resist_surf, int(lbound(actual%heatState%qh_resist_surf, 1), c_int), int(ubound(actual%heatState%qh_resist_surf, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(227_c_int, actual%heatState%tsurf_ind, int(lbound(actual%heatState%tsurf_ind, 1), c_int), int(ubound(actual%heatState%tsurf_ind, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(228_c_int, actual%heatState%QH_LUMPS, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(229_c_int, actual%heatState%QE_LUMPS, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(230_c_int, actual%heatState%k_clear, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(231_c_int, actual%heatState%k_up, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(232_c_int, actual%heatState%l_down, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(233_c_int, actual%heatState%l_up, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(234_c_int, actual%heatState%qe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(235_c_int, actual%heatState%qf, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(236_c_int, actual%heatState%QF_SAHP, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(237_c_int, actual%heatState%qh, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(238_c_int, actual%heatState%qh_residual, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(239_c_int, actual%heatState%qh_resist, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(240_c_int, actual%heatState%ehc_iter_dif_tsfc, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(241_c_int, actual%heatState%ehc_iter_dif_qh, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(242_c_int, actual%heatState%ehc_iter_score, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(243_c_int, actual%heatState%qn, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(244_c_int, actual%heatState%qn_snowfree, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(245_c_int, actual%heatState%qs, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(246_c_int, actual%heatState%TSfc_C, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(247_c_int, actual%heatState%tsurf, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(248_c_int, actual%heatState%QH_Init, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(249_c_int, actual%heatState%roof_in_sw_spc, int(lbound(actual%heatState%roof_in_sw_spc, 1), c_int), int(ubound(actual%heatState%roof_in_sw_spc, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(250_c_int, actual%heatState%roof_in_lw_spc, int(lbound(actual%heatState%roof_in_lw_spc, 1), c_int), int(ubound(actual%heatState%roof_in_lw_spc, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(251_c_int, actual%heatState%wall_in_sw_spc, int(lbound(actual%heatState%wall_in_sw_spc, 1), c_int), int(ubound(actual%heatState%wall_in_sw_spc, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(252_c_int, actual%heatState%wall_in_lw_spc, int(lbound(actual%heatState%wall_in_lw_spc, 1), c_int), int(ubound(actual%heatState%wall_in_lw_spc, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(253_c_int, actual%heatState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(254_c_int, actual%roughnessState%fai_bldg_use, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(255_c_int, actual%roughnessState%fai_evetree_use, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(256_c_int, actual%roughnessState%fai_dectree_use, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(257_c_int, actual%roughnessState%FAI, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(258_c_int, actual%roughnessState%PAI, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(259_c_int, actual%roughnessState%Zh, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(260_c_int, actual%roughnessState%z0m, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(261_c_int, actual%roughnessState%z0v, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(262_c_int, actual%roughnessState%zdm, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(263_c_int, actual%roughnessState%ZZD, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(264_c_int, actual%roughnessState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(265_c_int, actual%stebbsState%kdown_2d, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(266_c_int, actual%stebbsState%kup_2d, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(267_c_int, actual%stebbsState%k_west, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(268_c_int, actual%stebbsState%k_south, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(269_c_int, actual%stebbsState%k_north, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(270_c_int, actual%stebbsState%k_east, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(271_c_int, actual%stebbsState%ldown_2d, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(272_c_int, actual%stebbsState%lup_2d, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(273_c_int, actual%stebbsState%l_west, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(274_c_int, actual%stebbsState%l_south, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(275_c_int, actual%stebbsState%l_north, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(276_c_int, actual%stebbsState%l_east, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(277_c_int, actual%stebbsState%z_array, int(lbound(actual%stebbsState%z_array, 1), c_int), int(ubound(actual%stebbsState%z_array, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(278_c_int, actual%stebbsState%dataout_line_u_rsl, int(lbound(actual%stebbsState%dataout_line_u_rsl, 1), c_int), int(ubound(actual%stebbsState%dataout_line_u_rsl, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(279_c_int, actual%stebbsState%dataout_line_t_rsl, int(lbound(actual%stebbsState%dataout_line_t_rsl, 1), c_int), int(ubound(actual%stebbsState%dataout_line_t_rsl, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_1d(280_c_int, actual%stebbsState%dataout_line_q_rsl, int(lbound(actual%stebbsState%dataout_line_q_rsl, 1), c_int), int(ubound(actual%stebbsState%dataout_line_q_rsl, 1), c_int), out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(281_c_int, actual%stebbsState%deep_soil_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(282_c_int, actual%stebbsState%MonthMeanAirTemperature_diffmax, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(283_c_int, actual%stebbsState%outdoor_air_start_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(284_c_int, actual%stebbsState%indoor_air_start_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(285_c_int, actual%stebbsState%indoor_mass_start_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(286_c_int, actual%stebbsState%wall_indoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(287_c_int, actual%stebbsState%wall_outdoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(288_c_int, actual%stebbsState%roof_indoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(289_c_int, actual%stebbsState%roof_outdoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(290_c_int, actual%stebbsState%window_indoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(291_c_int, actual%stebbsState%window_outdoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(292_c_int, actual%stebbsState%ground_floor_indoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(293_c_int, actual%stebbsState%ground_floor_outdoor_surface_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(294_c_int, actual%stebbsState%water_tank_temperature_state, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(295_c_int, actual%stebbsState%internal_wall_water_tank_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(296_c_int, actual%stebbsState%external_wall_water_tank_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(297_c_int, actual%stebbsState%mains_water_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(298_c_int, actual%stebbsState%domestic_hot_water_temperature_in_use_in_building, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(299_c_int, actual%stebbsState%internal_wall_dhw_vessel_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(300_c_int, actual%stebbsState%external_wall_dhw_vessel_temperature, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(301_c_int, actual%stebbsState%QS_stebbs, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      if (allocated(actual%stebbsState%buildings)) then
         call obs_emit_alloc_bounds_1d(302_c_int, int(lbound(actual%stebbsState%buildings, 1), c_int), int(ubound(actual%stebbsState%buildings, 1), c_int), out_meta, n_meta, err)
      else
         call obs_emit_unallocated(302_c_int, out_meta, n_meta, err)
      end if
      call obs_emit_logical_scalar(303_c_int, actual%stebbsState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(304_c_int, actual%nhoodState%U_hbh_1dravg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(305_c_int, actual%nhoodState%QN_1dravg, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(306_c_int, actual%nhoodState%Tair_mn_prev, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_real_scalar(307_c_int, actual%nhoodState%iter_count, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)
      call obs_emit_logical_scalar(308_c_int, actual%nhoodState%iter_safe, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, n_meta, cap, pos, err)

      n_out = pos
   end subroutine suews_obs_unpack_suews_state

end module module_test_capi_field_observer
