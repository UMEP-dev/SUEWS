! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for batch DTS simulation.
!
! Notes:
! - This adapter exposes the batch entry point `SUEWS_cal_multitsteps_dts`.
! - Site/state member payloads are transferred via concatenated buffers and TOC
!   arrays. The current implementation applies the conductance member to the
!   runtime site object and round-trips state buffers for deterministic Rust
!   codec compatibility while state packing is finalised.
! -----------------------------------------------------------------------------
module module_c_api_driver
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer
use module_ctrl_const_allocate, only: ncolumnsDataOutSUEWS, nspec, nsurf, nvegsurf
use module_ctrl_type, only: SUEWS_TIMER, SUEWS_CONFIG, SUEWS_SITE, SUEWS_STATE, &
                            flag_STATE, anthroEmis_STATE, OHM_STATE, solar_State, atm_state, PHENOLOGY_STATE, &
                            SNOW_STATE, HYDRO_STATE, HEAT_STATE, ROUGHNESS_STATE, STEBBS_STATE, NHOOD_STATE
use module_c_api_spartacus_prm, only: spartacus_prm_unpack
use module_c_api_lumps, only: lumps_prm_unpack
use module_c_api_ehc_prm, only: ehc_prm_unpack
use module_c_api_spartacus_layer_prm, only: spartacus_layer_prm_unpack
use module_c_api_surf_store, only: surf_store_prm_unpack
use module_c_api_irrigation_prm, only: irrigation_prm_unpack
use module_c_api_anthro_emis_prm, only: anthro_emis_prm_unpack
use module_c_api_snow_prm, only: snow_prm_unpack
use module_c_api_lc_paved_prm, only: lc_paved_prm_unpack
use module_c_api_lc_bldg_prm, only: lc_bldg_prm_unpack
use module_c_api_lc_dectr_prm, only: lc_dectr_prm_unpack
use module_c_api_lc_evetr_prm, only: lc_evetr_prm_unpack
use module_c_api_lc_grass_prm, only: lc_grass_prm_unpack
use module_c_api_lc_bsoil_prm, only: lc_bsoil_prm_unpack
use module_c_api_lc_water_prm, only: lc_water_prm_unpack
use module_c_api_building_archetype_prm, only: building_archetype_prm_unpack
use module_c_api_conductance, only: conductance_prm_unpack
use module_c_api_stebbs_prm, only: stebbs_prm_unpack
use module_c_api_flag, only: flag_state_unpack
use module_c_api_anthro_emis_state, only: anthroemis_state_unpack
use module_c_api_ohm, only: ohm_state_unpack
use module_c_api_solar, only: solar_state_unpack
use module_c_api_atm, only: atm_state_unpack
use module_c_api_phenology, only: phenology_state_unpack
use module_c_api_snow, only: snow_state_unpack
use module_c_api_hydro_state, only: hydro_state_unpack
use module_c_api_heat_state, only: heat_state_unpack
use module_c_api_roughness, only: roughness_state_unpack
use module_c_api_stebbs_state, only: stebbs_state_unpack
use module_c_api_nhood, only: nhood_state_unpack
use SUEWS_Driver, only: SUEWS_cal_multitsteps_dts

implicit none

private

integer(c_int), parameter :: SUEWS_CAPI_TIMER_LEN = 18_c_int
integer(c_int), parameter :: SUEWS_CAPI_CONFIG_LEN = 21_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_SCALARS_LEN = 24_c_int
integer(c_int), parameter :: SUEWS_CAPI_FORCING_COLS = 21_c_int

integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_COUNT = 18_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_COUNT = 13_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_SPARTACUS = 1_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_LUMPS = 2_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_EHC = 3_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_SPARTACUS_LAYER = 4_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_SURF_STORE = 5_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_IRRIGATION = 6_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_ANTHROEMIS = 7_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_SNOW = 8_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_CONDUCTANCE = 9_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_LC_PAVED = 10_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_LC_BLDG = 11_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_LC_DECTR = 12_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_LC_EVETR = 13_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_LC_GRASS = 14_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_LC_BSOIL = 15_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_LC_WATER = 16_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_BUILDING_ARCHTYPE = 17_c_int
integer(c_int), parameter :: SUEWS_CAPI_SITE_MEMBER_STEBBS = 18_c_int

integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_ERROR_STATE = 1_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_FLAG = 2_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_ANTHRO_EMIS = 3_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_OHM = 4_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_SOLAR = 5_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_ATM = 6_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_PHENOLOGY = 7_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_SNOW = 8_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_HYDRO = 9_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_HEAT = 10_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_ROUGHNESS = 11_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_STEBBS = 12_c_int
integer(c_int), parameter :: SUEWS_CAPI_STATE_MEMBER_NHOOD = 13_c_int

integer(c_int), parameter :: SUEWS_CAPI_FLAG_STATE_LEN = 5_c_int
integer(c_int), parameter :: SUEWS_CAPI_ANTHROEMIS_STATE_LEN = 22_c_int
integer(c_int), parameter :: SUEWS_CAPI_OHM_STATE_LEN = 53_c_int
integer(c_int), parameter :: SUEWS_CAPI_SOLAR_STATE_LEN = 3_c_int
integer(c_int), parameter :: SUEWS_CAPI_ATM_STATE_LEN = 39_c_int
integer(c_int), parameter :: SUEWS_CAPI_PHENOLOGY_STATE_LEN = 76_c_int
integer(c_int), parameter :: SUEWS_CAPI_SNOW_STATE_LEN = 79_c_int
integer(c_int), parameter :: SUEWS_CAPI_HYDRO_STATE_BASE_LEN = 10_c_int * int(nsurf, c_int) + 34_c_int
integer(c_int), parameter :: SUEWS_CAPI_HEAT_STATE_BASE_LEN = 7_c_int * int(nsurf, c_int) + 79_c_int
integer(c_int), parameter :: SUEWS_CAPI_ROUGHNESS_STATE_LEN = 11_c_int
integer(c_int), parameter :: SUEWS_CAPI_STEBBS_STATE_LEN = 154_c_int
integer(c_int), parameter :: SUEWS_CAPI_NHOOD_STATE_LEN = 5_c_int

public :: suews_cal_multitsteps_c

contains

subroutine suews_cal_multitsteps_c( &
   timer_flat, timer_len, &
   config_flat, config_len, &
   site_scalars_flat, site_scalars_len, &
   site_flat, site_flat_len, &
   site_toc, site_toc_len, site_member_count, &
   state_flat, state_flat_len, &
   state_toc, state_toc_len, state_member_count, &
   forcing_flat, len_sim, forcing_cols, &
   nlayer, ndepth, &
   timer_out, timer_out_len, &
   state_out_flat, state_out_len, &
   output_flat, output_len, &
   sim_err_code, sim_err_message, sim_err_message_len, &
   err) bind(C, name='suews_cal_multitsteps_c')

   implicit none

   real(c_double), intent(in) :: timer_flat(*)
   integer(c_int), value, intent(in) :: timer_len
   real(c_double), intent(in) :: config_flat(*)
   integer(c_int), value, intent(in) :: config_len
   real(c_double), intent(in) :: site_scalars_flat(*)
   integer(c_int), value, intent(in) :: site_scalars_len

   real(c_double), intent(in) :: site_flat(*)
   integer(c_int), value, intent(in) :: site_flat_len
   integer(c_int), intent(in) :: site_toc(*)
   integer(c_int), value, intent(in) :: site_toc_len
   integer(c_int), value, intent(in) :: site_member_count

   real(c_double), intent(in) :: state_flat(*)
   integer(c_int), value, intent(in) :: state_flat_len
   integer(c_int), intent(in) :: state_toc(*)
   integer(c_int), value, intent(in) :: state_toc_len
   integer(c_int), value, intent(in) :: state_member_count

   real(c_double), intent(in) :: forcing_flat(*)
   integer(c_int), value, intent(in) :: len_sim
   integer(c_int), value, intent(in) :: forcing_cols
   integer(c_int), value, intent(in) :: nlayer
   integer(c_int), value, intent(in) :: ndepth

   real(c_double), intent(out) :: timer_out(*)
   integer(c_int), value, intent(in) :: timer_out_len
   real(c_double), intent(out) :: state_out_flat(*)
   integer(c_int), value, intent(in) :: state_out_len
   real(c_double), intent(out) :: output_flat(*)
   integer(c_int), value, intent(in) :: output_len

   integer(c_int), intent(out) :: sim_err_code
   character(c_char), intent(out) :: sim_err_message(*)
   integer(c_int), value, intent(in) :: sim_err_message_len
   integer(c_int), intent(out) :: err

   type(SUEWS_TIMER) :: timer_local
   type(SUEWS_CONFIG) :: config_local
   type(SUEWS_SITE) :: site_local
   type(SUEWS_STATE) :: state_local

   real(c_double), dimension(:, :), allocatable :: metforcing_block
   real(c_double), dimension(:, :), allocatable :: dataout_block

   integer :: len_sim_i
   integer :: forcing_cols_i
   integer :: nlayer_i
   integer :: ndepth_i
   integer :: ir
   integer :: ic
   integer :: idx
   integer(c_int) :: local_err

   sim_err_code = 0_c_int
   call copy_to_c_buffer('', sim_err_message, sim_err_message_len)
   err = SUEWS_CAPI_OK

   len_sim_i = int(len_sim)
   forcing_cols_i = int(forcing_cols)
   nlayer_i = int(nlayer)
   ndepth_i = int(ndepth)

   if (timer_len<SUEWS_CAPI_TIMER_LEN .or. timer_out_len<SUEWS_CAPI_TIMER_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   if (config_len<SUEWS_CAPI_CONFIG_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   if (site_scalars_len<SUEWS_CAPI_SITE_SCALARS_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   if (len_sim_i<=0 .or. forcing_cols_i/=int(SUEWS_CAPI_FORCING_COLS)) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   if (nlayer_i<=0 .or. ndepth_i<=0) then
      err = SUEWS_CAPI_BAD_STATE
      return
   end if

   if (site_member_count/=SUEWS_CAPI_SITE_MEMBER_COUNT) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   if (state_member_count/=SUEWS_CAPI_STATE_MEMBER_COUNT) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   call validate_member_toc(site_flat_len, site_toc, site_toc_len, site_member_count, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call validate_member_toc(state_flat_len, state_toc, state_toc_len, state_member_count, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   if (state_out_len<state_flat_len) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   if (output_len<len_sim * ncolumnsDataOutSUEWS) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   call unpack_timer(timer_flat, timer_len, timer_local, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call unpack_config(config_flat, config_len, config_local, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   allocate (metforcing_block(len_sim_i, forcing_cols_i))
   allocate (dataout_block(len_sim_i, ncolumnsDataOutSUEWS))

   idx = 1
   do ir = 1, len_sim_i
      do ic = 1, forcing_cols_i
         metforcing_block(ir, ic) = forcing_flat(idx)
         idx = idx + 1
      end do
   end do

   call initialise_site_state( &
      site_local, state_local, nlayer_i, ndepth_i, config_local, &
      site_scalars_flat, site_scalars_len, &
      site_flat, site_flat_len, site_toc, site_toc_len, site_member_count, &
      local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call unpack_state_members( &
      state_local, nlayer_i, ndepth_i, &
      state_flat, state_flat_len, state_toc, state_toc_len, state_member_count, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call state_local%errorState%reset()
   call SUEWS_cal_multitsteps_dts( &
      timer_local, metforcing_block, len_sim_i, &
      config_local, site_local, &
      state_local, dataout_block)

   call pack_timer(timer_local, timer_out, timer_out_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   idx = 1
   do ir = 1, len_sim_i
      do ic = 1, ncolumnsDataOutSUEWS
         output_flat(idx) = dataout_block(ir, ic)
         idx = idx + 1
      end do
   end do

   if (state_local%errorState%flag) then
      sim_err_code = int(state_local%errorState%code, c_int)
      call copy_to_c_buffer(state_local%errorState%message, sim_err_message, sim_err_message_len)
   end if

   call copy_state_input_to_output( &
      state_flat, state_flat_len, state_out_flat, state_out_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   err = SUEWS_CAPI_OK

end subroutine suews_cal_multitsteps_c

subroutine validate_member_toc(flat_len, toc, toc_len, member_count, err)
   implicit none

   integer(c_int), intent(in) :: flat_len
   integer(c_int), intent(in) :: toc(*)
   integer(c_int), intent(in) :: toc_len
   integer(c_int), intent(in) :: member_count
   integer(c_int), intent(out) :: err

   integer(c_int) :: required_toc_len
   integer(c_int) :: i_member
   integer(c_int) :: toc_base
   integer(c_int) :: offset
   integer(c_int) :: member_len
   integer(KIND=8) :: member_end

   if (flat_len<0_c_int .or. member_count<0_c_int .or. toc_len<0_c_int) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   required_toc_len = 2_c_int * member_count
   if (toc_len<required_toc_len) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   do i_member = 1_c_int, member_count
      toc_base = 2_c_int * (i_member - 1_c_int)
      offset = toc(int(toc_base + 1_c_int))
      member_len = toc(int(toc_base + 2_c_int))

      if (offset<0_c_int .or. member_len<0_c_int) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if

      member_end = int(offset, KIND=8) + int(member_len, KIND=8)
      if (member_end>int(flat_len, KIND=8)) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
   end do

   err = SUEWS_CAPI_OK

end subroutine validate_member_toc

subroutine member_span_from_toc( &
   flat_len, toc, toc_len, member_count, member_index, member_offset, member_len, err)
   implicit none

   integer(c_int), intent(in) :: flat_len
   integer(c_int), intent(in) :: toc(*)
   integer(c_int), intent(in) :: toc_len
   integer(c_int), intent(in) :: member_count
   integer(c_int), intent(in) :: member_index
   integer(c_int), intent(out) :: member_offset
   integer(c_int), intent(out) :: member_len
   integer(c_int), intent(out) :: err

   integer(c_int) :: toc_base
   integer(KIND=8) :: member_end

   call validate_member_toc(flat_len, toc, toc_len, member_count, err)
   if (err/=SUEWS_CAPI_OK) return

   if (member_index<1_c_int .or. member_index>member_count) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   toc_base = 2_c_int * (member_index - 1_c_int)
   member_offset = toc(int(toc_base + 1_c_int))
   member_len = toc(int(toc_base + 2_c_int))

   member_end = int(member_offset, KIND=8) + int(member_len, KIND=8)
   if (member_offset<0_c_int .or. member_len<0_c_int .or. &
       member_end>int(flat_len, KIND=8)) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   err = SUEWS_CAPI_OK

end subroutine member_span_from_toc

subroutine copy_state_input_to_output(state_in, state_in_len, state_out, state_out_len, err)
   implicit none

   real(c_double), intent(in) :: state_in(*)
   integer(c_int), intent(in) :: state_in_len
   real(c_double), intent(out) :: state_out(*)
   integer(c_int), intent(in) :: state_out_len
   integer(c_int), intent(out) :: err

   integer(c_int) :: i

   if (state_in_len<0_c_int .or. state_out_len<0_c_int) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   if (state_out_len<state_in_len) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   do i = 1_c_int, state_in_len
      state_out(int(i)) = state_in(int(i))
   end do

   err = SUEWS_CAPI_OK

end subroutine copy_state_input_to_output

subroutine initialise_site_state( &
   site, state, nlayer, ndepth, config, &
   site_scalars_flat, site_scalars_len, &
   site_flat, site_flat_len, site_toc, site_toc_len, site_member_count, &
   err)
   implicit none

   type(SUEWS_SITE), intent(inout) :: site
   type(SUEWS_STATE), intent(inout) :: state
   integer, intent(in) :: nlayer
   integer, intent(in) :: ndepth
   type(SUEWS_CONFIG), intent(in) :: config
   real(c_double), intent(in) :: site_scalars_flat(*)
   integer(c_int), intent(in) :: site_scalars_len
   real(c_double), intent(in) :: site_flat(*)
   integer(c_int), intent(in) :: site_flat_len
   integer(c_int), intent(in) :: site_toc(*)
   integer(c_int), intent(in) :: site_toc_len
   integer(c_int), intent(in) :: site_member_count
   integer(c_int), intent(out) :: err

   integer :: ib

   call site%allocate(nlayer)
   site%nlayer = nlayer
   call site%ehc%allocate(7, ndepth)
   call site%spartacus_layer%allocate(nlayer)
   call site%spartacus%allocate(nlayer)

   call state%allocate(nlayer, ndepth)
   call state%stebbsState%allocate(1, nlayer)
   if (allocated(state%stebbsState%buildings)) then
      do ib = 1, size(state%stebbsState%buildings)
         call state%stebbsState%buildings(ib)%allocate(nlayer)
      end do
   end if

   call unpack_site_scalars(site_scalars_flat, site_scalars_len, site, err)
   if (err/=SUEWS_CAPI_OK) return

   call unpack_site_members( &
      site, nlayer, ndepth, &
      site_flat, site_flat_len, site_toc, site_toc_len, site_member_count, err)
   if (err/=SUEWS_CAPI_OK) return

   call site%cal_surf(config)
   err = SUEWS_CAPI_OK

end subroutine initialise_site_state

subroutine unpack_site_members( &
   site, nlayer, ndepth, &
   site_flat, site_flat_len, site_toc, site_toc_len, site_member_count, err)
   implicit none

   type(SUEWS_SITE), intent(inout) :: site
   integer, intent(in) :: nlayer
   integer, intent(in) :: ndepth
   real(c_double), intent(in) :: site_flat(*)
   integer(c_int), intent(in) :: site_flat_len
   integer(c_int), intent(in) :: site_toc(*)
   integer(c_int), intent(in) :: site_toc_len
   integer(c_int), intent(in) :: site_member_count
   integer(c_int), intent(out) :: err

   integer(c_int) :: member_offset
   integer(c_int) :: member_len
   integer(c_int) :: local_err
   integer(c_int) :: required_len

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_SPARTACUS, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   required_len = 14_c_int + int(nlayer, c_int) + 1_c_int
   if (member_len>=required_len) then
      call spartacus_prm_unpack(site_flat(int(member_offset) + 1), member_len, int(nlayer, c_int), site%spartacus, local_err)
      if (local_err/=SUEWS_CAPI_OK) then
         err = local_err
         return
      end if
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_LUMPS, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call lumps_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%lumps, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_EHC, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   required_len = 9_c_int * int(nsurf, c_int) + 9_c_int * int(nsurf * ndepth, c_int)
   if (member_len>=required_len) then
      call ehc_prm_unpack(site_flat(int(member_offset) + 1), member_len, int(nsurf, c_int), int(ndepth, c_int), site%ehc, local_err)
      if (local_err/=SUEWS_CAPI_OK) then
         err = local_err
         return
      end if
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_SPARTACUS_LAYER, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   required_len = 8_c_int * int(nlayer, c_int) + 2_c_int * int(nspec, c_int) * int(nlayer, c_int)
   if (member_len>=required_len) then
      call spartacus_layer_prm_unpack(site_flat(int(member_offset) + 1), member_len, int(nlayer, c_int), &
                                      int(nspec, c_int), site%spartacus_layer, local_err)
      if (local_err/=SUEWS_CAPI_OK) then
         err = local_err
         return
      end if
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_SURF_STORE, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call surf_store_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%surf_store, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_IRRIGATION, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call irrigation_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%irrigation, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_ANTHROEMIS, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call anthro_emis_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%anthroemis, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_SNOW, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call snow_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%snow, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_CONDUCTANCE, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call conductance_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%conductance, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_LC_PAVED, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call lc_paved_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%lc_paved, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_LC_BLDG, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call lc_bldg_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%lc_bldg, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_LC_DECTR, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call lc_dectr_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%lc_dectr, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_LC_EVETR, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call lc_evetr_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%lc_evetr, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_LC_GRASS, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call lc_grass_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%lc_grass, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_LC_BSOIL, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call lc_bsoil_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%lc_bsoil, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_LC_WATER, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call lc_water_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%lc_water, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_BUILDING_ARCHTYPE, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call building_archetype_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%building_archtype, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(site_flat_len, site_toc, site_toc_len, site_member_count, &
                             SUEWS_CAPI_SITE_MEMBER_STEBBS, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call stebbs_prm_unpack(site_flat(int(member_offset) + 1), member_len, site%stebbs, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   err = SUEWS_CAPI_OK

end subroutine unpack_site_members

subroutine unpack_state_members( &
   state, nlayer, ndepth, &
   state_flat, state_flat_len, state_toc, state_toc_len, state_member_count, err)
   implicit none

   type(SUEWS_STATE), intent(inout) :: state
   integer, intent(in) :: nlayer
   integer, intent(in) :: ndepth
   real(c_double), intent(in) :: state_flat(*)
   integer(c_int), intent(in) :: state_flat_len
   integer(c_int), intent(in) :: state_toc(*)
   integer(c_int), intent(in) :: state_toc_len
   integer(c_int), intent(in) :: state_member_count
   integer(c_int), intent(out) :: err

   integer(c_int) :: member_offset
   integer(c_int) :: member_len
   integer(c_int) :: required_len
   integer(c_int) :: local_err

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_ERROR_STATE, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   if (member_len/=0_c_int) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_FLAG, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_FLAG_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call flag_state_unpack(state_flat(int(member_offset) + 1), member_len, state%flagState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_ANTHRO_EMIS, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_ANTHROEMIS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call anthroemis_state_unpack(state_flat(int(member_offset) + 1), member_len, state%anthroemisState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_OHM, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_OHM_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call ohm_state_unpack(state_flat(int(member_offset) + 1), member_len, state%ohmState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_SOLAR, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_SOLAR_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call solar_state_unpack(state_flat(int(member_offset) + 1), member_len, state%solarState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_ATM, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_ATM_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call atm_state_unpack(state_flat(int(member_offset) + 1), member_len, state%atmState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_PHENOLOGY, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_PHENOLOGY_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call phenology_state_unpack(state_flat(int(member_offset) + 1), member_len, state%phenState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_SNOW, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_SNOW_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call snow_state_unpack(state_flat(int(member_offset) + 1), member_len, state%snowState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_HYDRO, member_offset, member_len, local_err)
   required_len = SUEWS_CAPI_HYDRO_STATE_BASE_LEN + 6_c_int * int(nlayer, c_int)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<required_len) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call hydro_state_unpack(state_flat(int(member_offset) + 1), member_len, int(nlayer, c_int), state%hydroState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_HEAT, member_offset, member_len, local_err)
   if (nlayer>0) then
      required_len = SUEWS_CAPI_HEAT_STATE_BASE_LEN + 2_c_int * int(nlayer * ndepth, c_int) + &
                     2_c_int * int(nsurf * ndepth, c_int) + 14_c_int * int(nlayer, c_int) + &
                     3_c_int * int(nsurf, c_int)
   else
      required_len = SUEWS_CAPI_HEAT_STATE_BASE_LEN
   end if
   if (local_err/=SUEWS_CAPI_OK .or. member_len<required_len) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call heat_state_unpack(state_flat(int(member_offset) + 1), member_len, &
                          int(nlayer, c_int), int(ndepth, c_int), state%heatState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_ROUGHNESS, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_ROUGHNESS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call roughness_state_unpack(state_flat(int(member_offset) + 1), member_len, state%roughnessState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_STEBBS, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_STEBBS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call stebbs_state_unpack(state_flat(int(member_offset) + 1), member_len, state%stebbsState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   call member_span_from_toc(state_flat_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_NHOOD, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_NHOOD_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call nhood_state_unpack(state_flat(int(member_offset) + 1), member_len, state%nhoodState, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   err = SUEWS_CAPI_OK

end subroutine unpack_state_members

subroutine unpack_timer(flat, n_flat, timer, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(SUEWS_TIMER), intent(out) :: timer
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_TIMER_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   timer%id = int(nint(flat(1)))
   timer%imin = int(nint(flat(2)))
   timer%isec = int(nint(flat(3)))
   timer%it = int(nint(flat(4)))
   timer%iy = int(nint(flat(5)))
   timer%tstep = int(nint(flat(6)))
   timer%tstep_prev = int(nint(flat(7)))
   timer%dt_since_start = int(nint(flat(8)))
   timer%dt_since_start_prev = int(nint(flat(9)))
   timer%nsh = int(nint(flat(10)))
   timer%nsh_real = flat(11)
   timer%tstep_real = flat(12)
   timer%dectime = flat(13)
   timer%dayofWeek_id(1) = int(nint(flat(14)))
   timer%dayofWeek_id(2) = int(nint(flat(15)))
   timer%dayofWeek_id(3) = int(nint(flat(16)))
   timer%DLS = int(nint(flat(17)))
   timer%new_day = int(nint(flat(18)))

   err = SUEWS_CAPI_OK

end subroutine unpack_timer

subroutine pack_timer(timer, flat, n_flat, err)
   implicit none

   type(SUEWS_TIMER), intent(in) :: timer
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_TIMER_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   flat(1) = real(timer%id, c_double)
   flat(2) = real(timer%imin, c_double)
   flat(3) = real(timer%isec, c_double)
   flat(4) = real(timer%it, c_double)
   flat(5) = real(timer%iy, c_double)
   flat(6) = real(timer%tstep, c_double)
   flat(7) = real(timer%tstep_prev, c_double)
   flat(8) = real(timer%dt_since_start, c_double)
   flat(9) = real(timer%dt_since_start_prev, c_double)
   flat(10) = real(timer%nsh, c_double)
   flat(11) = timer%nsh_real
   flat(12) = timer%tstep_real
   flat(13) = timer%dectime
   flat(14) = real(timer%dayofWeek_id(1), c_double)
   flat(15) = real(timer%dayofWeek_id(2), c_double)
   flat(16) = real(timer%dayofWeek_id(3), c_double)
   flat(17) = real(timer%DLS, c_double)
   flat(18) = real(timer%new_day, c_double)

   err = SUEWS_CAPI_OK

end subroutine pack_timer

subroutine unpack_config(flat, n_flat, config, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(SUEWS_CONFIG), intent(out) :: config
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_CONFIG_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   config%RSLMethod = int(nint(flat(1)))
   config%EmissionsMethod = int(nint(flat(2)))
   config%RoughLenHeatMethod = int(nint(flat(3)))
   config%RoughLenMomMethod = int(nint(flat(4)))
   config%FAIMethod = int(nint(flat(5)))
   config%SMDMethod = int(nint(flat(6)))
   config%WaterUseMethod = int(nint(flat(7)))
   config%NetRadiationMethod = int(nint(flat(8)))
   config%StabilityMethod = int(nint(flat(9)))
   config%StorageHeatMethod = int(nint(flat(10)))
   config%Diagnose = int(nint(flat(11)))
   config%SnowUse = int(nint(flat(12)))
   config%use_sw_direct_albedo = flat(13)>=0.5_c_double
   config%ohmIncQF = int(nint(flat(14)))
   config%DiagQS = int(nint(flat(15)))
   config%EvapMethod = int(nint(flat(16)))
   config%LAImethod = int(nint(flat(17)))
   config%RSLLevel = int(nint(flat(18)))
   config%stebbsmethod = int(nint(flat(19)))
   config%rcmethod = int(nint(flat(20)))
   config%flag_test = flat(21)>=0.5_c_double

   err = SUEWS_CAPI_OK

end subroutine unpack_config

subroutine unpack_site_scalars(flat, n_flat, site, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(SUEWS_SITE), intent(inout) :: site
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_SITE_SCALARS_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   site%lat = flat(1)
   site%lon = flat(2)
   site%alt = flat(3)
   site%timezone = flat(4)
   site%surfacearea = flat(5)
   site%z = flat(6)
   site%z0m_in = flat(7)
   site%zdm_in = flat(8)
   site%pipecapacity = flat(9)
   site%runofftowater = flat(10)
   site%narp_trans_site = flat(11)
   site%co2pointsource = flat(12)
   site%flowchange = flat(13)
   site%n_buildings = flat(14)
   site%h_std = flat(15)
   site%lambda_c = flat(16)
   site%sfr_surf(1:7) = flat(17:23)
   site%gridiv = int(nint(flat(24)))

   err = SUEWS_CAPI_OK

end subroutine unpack_site_scalars

end module module_c_api_driver

module c_api_driver_module
use module_c_api_driver
end module c_api_driver_module
