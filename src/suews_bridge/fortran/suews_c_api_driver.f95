! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for batch DTS simulation.
!
! Notes:
! - This adapter exposes the batch entry point `SUEWS_cal_multitsteps_dts`.
! - Site/state member payloads are transferred via concatenated buffers and TOC
!   arrays. Post-simulation state is packed from the modified Fortran types
!   back into the flat output buffer for Rust codec round-trip.
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
integer(c_int), parameter :: SUEWS_CAPI_STEBBS_STATE_RSL_LEN = 30_c_int

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

   call pack_state_to_output( &
      state_local, nlayer_i, ndepth_i, &
      state_out_flat, state_out_len, &
      state_toc, state_toc_len, state_member_count, local_err)
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


! -----------------------------------------------------------------------------
! Inline helpers for packing vectors and matrices into flat output buffers.
! These mirror the unpack_vec/unpack_mat helpers in the heat_state module
! but work with allocatable arrays from the real Fortran types.
! -----------------------------------------------------------------------------

subroutine pack_vec_inline(field, flat, idx, n)
   implicit none

   real(c_double), dimension(:), allocatable, intent(in) :: field
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(inout) :: idx
   integer(c_int), intent(in) :: n
   integer(c_int) :: i

   if (n<=0_c_int) return
   do i = 1_c_int, n
      flat(idx) = field(i)
      idx = idx + 1_c_int
   end do

end subroutine pack_vec_inline

subroutine pack_mat_inline(field, flat, idx, n1, n2)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(in) :: field
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(inout) :: idx
   integer(c_int), intent(in) :: n1
   integer(c_int), intent(in) :: n2
   integer(c_int) :: i
   integer(c_int) :: j

   if (n1<=0_c_int .or. n2<=0_c_int) return
   do i = 1_c_int, n1
      do j = 1_c_int, n2
         flat(idx) = field(i, j)
         idx = idx + 1_c_int
      end do
   end do

end subroutine pack_mat_inline

! -----------------------------------------------------------------------------
! pack_state_to_output: pack the post-simulation state_local (SUEWS_STATE)
! into the flat output buffer, using the same TOC layout as the input buffer.
! This is the REVERSE of unpack_state_members.
! -----------------------------------------------------------------------------
subroutine pack_state_to_output( &
   state, nlayer, ndepth, &
   state_out_flat, state_out_len, &
   state_toc, state_toc_len, state_member_count, err)
   implicit none

   type(SUEWS_STATE), intent(in) :: state
   integer, intent(in) :: nlayer
   integer, intent(in) :: ndepth
   real(c_double), intent(out) :: state_out_flat(*)
   integer(c_int), intent(in) :: state_out_len
   integer(c_int), intent(in) :: state_toc(*)
   integer(c_int), intent(in) :: state_toc_len
   integer(c_int), intent(in) :: state_member_count
   integer(c_int), intent(out) :: err

   integer(c_int) :: member_offset
   integer(c_int) :: member_len
   integer(c_int) :: local_err

   ! --- Member 1: error_state (empty, length 0) ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_ERROR_STATE, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 2: flag_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_FLAG, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_FLAG_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call pack_flag_state(state%flagState, state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 3: anthroemis_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_ANTHRO_EMIS, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_ANTHROEMIS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call pack_anthroemis_state(state%anthroemisState, state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 4: ohm_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_OHM, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_OHM_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call pack_ohm_state(state%ohmState, state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 5: solar_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_SOLAR, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_SOLAR_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call pack_solar_state(state%solarState, state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 6: atm_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_ATM, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_ATM_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call pack_atm_state(state%atmState, state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 7: phenology_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_PHENOLOGY, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_PHENOLOGY_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call pack_phenology_state(state%phenState, state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 8: snow_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_SNOW, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_SNOW_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call pack_snow_state(state%snowState, state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 9: hydro_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_HYDRO, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call pack_hydro_state(state%hydroState, int(nlayer, c_int), &
                         state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 10: heat_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_HEAT, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if
   call pack_heat_state(state%heatState, int(nlayer, c_int), int(ndepth, c_int), &
                        state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 11: roughness_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_ROUGHNESS, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_ROUGHNESS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call pack_roughness_state(state%roughnessState, state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 12: stebbs_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_STEBBS, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_STEBBS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call pack_stebbs_state(state%stebbsState, state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   ! --- Member 13: nhood_state ---
   call member_span_from_toc(state_out_len, state_toc, state_toc_len, state_member_count, &
                             SUEWS_CAPI_STATE_MEMBER_NHOOD, member_offset, member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. member_len<SUEWS_CAPI_NHOOD_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   call pack_nhood_state(state%nhoodState, state_out_flat(int(member_offset) + 1), member_len, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      err = local_err
      return
   end if

   err = SUEWS_CAPI_OK

end subroutine pack_state_to_output

! ---- Individual state member pack subroutines ----
! Each reverses the corresponding *_state_unpack, using the REAL types
! from module_ctrl_type (not the shadow types).

subroutine pack_flag_state(s, flat, n_flat, err)
   implicit none
   type(flag_STATE), intent(in) :: s
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err
   integer(c_int) :: idx

   if (n_flat<SUEWS_CAPI_FLAG_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   flat(idx) = merge(1.0_c_double, 0.0_c_double, s%flag_converge); idx = idx + 1_c_int
   flat(idx) = real(s%i_iter, c_double); idx = idx + 1_c_int
   flat(idx) = real(s%stebbs_bldg_init, c_double); idx = idx + 1_c_int
   flat(idx) = merge(1.0_c_double, 0.0_c_double, s%snow_warning_shown); idx = idx + 1_c_int
   flat(idx) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_flag_state

subroutine pack_anthroemis_state(s, flat, n_flat, err)
   implicit none
   type(anthroEmis_STATE), intent(in) :: s
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err
   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_ANTHROEMIS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   do i = 1_c_int, 12_c_int
      flat(idx) = s%HDD_id(i)
      idx = idx + 1_c_int
   end do
   flat(idx) = s%Fc; idx = idx + 1_c_int
   flat(idx) = s%Fc_anthro; idx = idx + 1_c_int
   flat(idx) = s%Fc_biogen; idx = idx + 1_c_int
   flat(idx) = s%Fc_build; idx = idx + 1_c_int
   flat(idx) = s%Fc_metab; idx = idx + 1_c_int
   flat(idx) = s%Fc_photo; idx = idx + 1_c_int
   flat(idx) = s%Fc_point; idx = idx + 1_c_int
   flat(idx) = s%Fc_respi; idx = idx + 1_c_int
   flat(idx) = s%Fc_traff; idx = idx + 1_c_int
   flat(idx) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_anthroemis_state

subroutine pack_ohm_state(s, flat, n_flat, err)
   implicit none
   type(OHM_STATE), intent(in) :: s
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err
   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_OHM_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   flat(idx) = s%qn_av; idx = idx + 1_c_int
   flat(idx) = s%dqndt; idx = idx + 1_c_int

   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%qn_surfs(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%dqndt_surf(i); idx = idx + 1_c_int
   end do

   flat(idx) = s%qn_s_av; idx = idx + 1_c_int
   flat(idx) = s%dqnsdt; idx = idx + 1_c_int
   flat(idx) = s%a1; idx = idx + 1_c_int
   flat(idx) = s%a2; idx = idx + 1_c_int
   flat(idx) = s%a3; idx = idx + 1_c_int
   flat(idx) = s%t2_prev; idx = idx + 1_c_int
   flat(idx) = s%ws_rav; idx = idx + 1_c_int
   flat(idx) = s%tair_prev; idx = idx + 1_c_int

   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%qn_rav(i); idx = idx + 1_c_int
   end do

   flat(idx) = s%a1_paved; idx = idx + 1_c_int
   flat(idx) = s%a1_bldg; idx = idx + 1_c_int
   flat(idx) = s%a1_evetr; idx = idx + 1_c_int
   flat(idx) = s%a1_dectr; idx = idx + 1_c_int
   flat(idx) = s%a1_grass; idx = idx + 1_c_int
   flat(idx) = s%a1_bsoil; idx = idx + 1_c_int
   flat(idx) = s%a1_water; idx = idx + 1_c_int

   flat(idx) = s%a2_paved; idx = idx + 1_c_int
   flat(idx) = s%a2_bldg; idx = idx + 1_c_int
   flat(idx) = s%a2_evetr; idx = idx + 1_c_int
   flat(idx) = s%a2_dectr; idx = idx + 1_c_int
   flat(idx) = s%a2_grass; idx = idx + 1_c_int
   flat(idx) = s%a2_bsoil; idx = idx + 1_c_int
   flat(idx) = s%a2_water; idx = idx + 1_c_int

   flat(idx) = s%a3_paved; idx = idx + 1_c_int
   flat(idx) = s%a3_bldg; idx = idx + 1_c_int
   flat(idx) = s%a3_evetr; idx = idx + 1_c_int
   flat(idx) = s%a3_dectr; idx = idx + 1_c_int
   flat(idx) = s%a3_grass; idx = idx + 1_c_int
   flat(idx) = s%a3_bsoil; idx = idx + 1_c_int
   flat(idx) = s%a3_water; idx = idx + 1_c_int

   flat(idx) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_ohm_state

subroutine pack_solar_state(s, flat, n_flat, err)
   implicit none
   type(solar_State), intent(in) :: s
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_SOLAR_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   flat(1) = s%azimuth_deg
   flat(2) = s%zenith_deg
   flat(3) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_solar_state

subroutine pack_atm_state(s, flat, n_flat, err)
   implicit none
   type(atm_state), intent(in) :: s
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err
   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_ATM_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   flat(idx) = s%fcld; idx = idx + 1_c_int
   flat(idx) = s%avcp; idx = idx + 1_c_int
   flat(idx) = s%dens_dry; idx = idx + 1_c_int
   flat(idx) = s%avdens; idx = idx + 1_c_int
   flat(idx) = s%dq; idx = idx + 1_c_int
   flat(idx) = s%ea_hpa; idx = idx + 1_c_int
   flat(idx) = s%es_hpa; idx = idx + 1_c_int
   flat(idx) = s%lv_j_kg; idx = idx + 1_c_int
   flat(idx) = s%lvs_j_kg; idx = idx + 1_c_int
   flat(idx) = s%tlv; idx = idx + 1_c_int
   flat(idx) = s%psyc_hpa; idx = idx + 1_c_int
   flat(idx) = s%psycice_hpa; idx = idx + 1_c_int
   flat(idx) = s%s_pa; idx = idx + 1_c_int
   flat(idx) = s%s_hpa; idx = idx + 1_c_int
   flat(idx) = s%sice_hpa; idx = idx + 1_c_int
   flat(idx) = s%vpd_hpa; idx = idx + 1_c_int
   flat(idx) = s%vpd_pa; idx = idx + 1_c_int
   flat(idx) = s%u10_ms; idx = idx + 1_c_int
   flat(idx) = s%u_hbh; idx = idx + 1_c_int
   flat(idx) = s%t2_c; idx = idx + 1_c_int
   flat(idx) = s%t_half_bldg_c; idx = idx + 1_c_int
   flat(idx) = s%q2_gkg; idx = idx + 1_c_int
   flat(idx) = s%rh2; idx = idx + 1_c_int
   flat(idx) = s%l_mod; idx = idx + 1_c_int
   flat(idx) = s%zl; idx = idx + 1_c_int
   flat(idx) = s%ra_h; idx = idx + 1_c_int
   flat(idx) = s%rs; idx = idx + 1_c_int
   flat(idx) = s%ustar; idx = idx + 1_c_int
   flat(idx) = s%tstar; idx = idx + 1_c_int
   flat(idx) = s%rb; idx = idx + 1_c_int
   flat(idx) = s%tair_av; idx = idx + 1_c_int

   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%rss_surf(i)
      idx = idx + 1_c_int
   end do

   flat(idx) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_atm_state

subroutine pack_phenology_state(s, flat, n_flat, err)
   implicit none
   type(PHENOLOGY_STATE), intent(in) :: s
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err
   integer(c_int) :: idx
   integer(c_int) :: i
   integer(c_int) :: j

   if (n_flat<SUEWS_CAPI_PHENOLOGY_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int

   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%alb(i)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nvegsurf, c_int)
      flat(idx) = s%lai_id(i)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nvegsurf, c_int)
      flat(idx) = s%gdd_id(i)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nvegsurf, c_int)
      flat(idx) = s%sdd_id(i)
      idx = idx + 1_c_int
   end do

   flat(idx) = s%vegphenlumps; idx = idx + 1_c_int
   flat(idx) = s%porosity_id; idx = idx + 1_c_int
   flat(idx) = s%decidcap_id; idx = idx + 1_c_int
   flat(idx) = s%albdectr_id; idx = idx + 1_c_int
   flat(idx) = s%albevetr_id; idx = idx + 1_c_int
   flat(idx) = s%albgrass_id; idx = idx + 1_c_int
   flat(idx) = s%tmin_id; idx = idx + 1_c_int
   flat(idx) = s%tmax_id; idx = idx + 1_c_int
   flat(idx) = s%lenday_id; idx = idx + 1_c_int
   flat(idx) = s%tempveg; idx = idx + 1_c_int

   do j = 1_c_int, int(nsurf, c_int)
      do i = 1_c_int, 6_c_int
         flat(idx) = s%storedrainprm(i, j)
         idx = idx + 1_c_int
      end do
   end do

   flat(idx) = s%gfunc; idx = idx + 1_c_int
   flat(idx) = s%gsc; idx = idx + 1_c_int
   flat(idx) = s%g_kdown; idx = idx + 1_c_int
   flat(idx) = s%g_dq; idx = idx + 1_c_int
   flat(idx) = s%g_ta; idx = idx + 1_c_int
   flat(idx) = s%g_smd; idx = idx + 1_c_int
   flat(idx) = s%g_lai; idx = idx + 1_c_int
   flat(idx) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_phenology_state

subroutine pack_snow_state(s, flat, n_flat, err)
   implicit none
   type(SNOW_STATE), intent(in) :: s
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err
   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_SNOW_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   flat(idx) = s%snowfallcum; idx = idx + 1_c_int
   flat(idx) = s%snowalb; idx = idx + 1_c_int
   flat(idx) = s%chsnow_per_interval; idx = idx + 1_c_int
   flat(idx) = s%mwh; idx = idx + 1_c_int
   flat(idx) = s%mwstore; idx = idx + 1_c_int
   flat(idx) = s%qn_snow; idx = idx + 1_c_int
   flat(idx) = s%qm; idx = idx + 1_c_int
   flat(idx) = s%qmfreez; idx = idx + 1_c_int
   flat(idx) = s%qmrain; idx = idx + 1_c_int
   flat(idx) = s%swe; idx = idx + 1_c_int
   flat(idx) = s%z0vsnow; idx = idx + 1_c_int
   flat(idx) = s%rasnow; idx = idx + 1_c_int
   flat(idx) = s%sice_hpa; idx = idx + 1_c_int

   do i = 1_c_int, 2_c_int
      flat(idx) = s%snowremoval(i)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%icefrac(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%snowdens(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%snowfrac(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%snowpack(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%snowwater(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%kup_ind_snow(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%qn_ind_snow(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%deltaqi(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%tsurf_ind_snow(i); idx = idx + 1_c_int
   end do

   flat(idx) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_snow_state

subroutine pack_hydro_state(s, nlayer_c, flat, n_flat, err)
   implicit none
   type(HYDRO_STATE), intent(in) :: s
   integer(c_int), intent(in) :: nlayer_c
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err
   integer(c_int) :: idx
   integer(c_int) :: i
   integer(c_int) :: n_expected

   n_expected = SUEWS_CAPI_HYDRO_STATE_BASE_LEN + 6_c_int * nlayer_c
   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int

   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%soilstore_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%state_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, 9_c_int
      flat(idx) = s%wuday_id(i); idx = idx + 1_c_int
   end do

   do i = 1_c_int, nlayer_c
      flat(idx) = s%soilstore_roof(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, nlayer_c
      flat(idx) = s%state_roof(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, nlayer_c
      flat(idx) = s%soilstore_wall(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, nlayer_c
      flat(idx) = s%state_wall(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, nlayer_c
      flat(idx) = s%ev_roof(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, nlayer_c
      flat(idx) = s%ev_wall(i); idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%ev0_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%ev_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%wu_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%runoffsoil(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%smd_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%drain_surf(i); idx = idx + 1_c_int
   end do

   flat(idx) = s%drain_per_tstep; idx = idx + 1_c_int
   flat(idx) = s%ev_per_tstep; idx = idx + 1_c_int
   flat(idx) = s%wu_ext; idx = idx + 1_c_int
   flat(idx) = s%wu_int; idx = idx + 1_c_int

   flat(idx) = s%runoffagveg; idx = idx + 1_c_int
   flat(idx) = s%runoffagimpervious; idx = idx + 1_c_int
   flat(idx) = s%runoff_per_tstep; idx = idx + 1_c_int
   flat(idx) = s%runoffpipes; idx = idx + 1_c_int
   flat(idx) = s%runoffsoil_per_tstep; idx = idx + 1_c_int
   flat(idx) = s%runoffwaterbody; idx = idx + 1_c_int
   flat(idx) = s%smd; idx = idx + 1_c_int
   flat(idx) = s%soilstate; idx = idx + 1_c_int
   flat(idx) = s%state_per_tstep; idx = idx + 1_c_int
   flat(idx) = s%surf_chang_per_tstep; idx = idx + 1_c_int
   flat(idx) = s%tot_chang_per_tstep; idx = idx + 1_c_int
   flat(idx) = s%runoff_per_interval; idx = idx + 1_c_int
   flat(idx) = s%nwstate_per_tstep; idx = idx + 1_c_int

   flat(idx) = s%soilmoistcap; idx = idx + 1_c_int
   flat(idx) = s%vsmd; idx = idx + 1_c_int

   flat(idx) = s%additionalwater; idx = idx + 1_c_int
   flat(idx) = s%addimpervious; idx = idx + 1_c_int
   flat(idx) = s%addpipes; idx = idx + 1_c_int
   flat(idx) = s%addveg; idx = idx + 1_c_int
   flat(idx) = s%addwaterbody; idx = idx + 1_c_int

   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%addwater(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%frac_water2runoff(i); idx = idx + 1_c_int
   end do

   flat(idx) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_hydro_state

subroutine pack_heat_state(s, nlayer_c, ndepth_c, flat, n_flat, err)
   implicit none
   type(HEAT_STATE), intent(in) :: s
   integer(c_int), intent(in) :: nlayer_c
   integer(c_int), intent(in) :: ndepth_c
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err
   integer(c_int) :: idx
   integer(c_int) :: surf_vec_len
   integer(c_int) :: i
   integer(c_int) :: n_expected

   if (nlayer_c>0_c_int) then
      n_expected = SUEWS_CAPI_HEAT_STATE_BASE_LEN + &
                   2_c_int * nlayer_c * ndepth_c + 2_c_int * int(nsurf, c_int) * ndepth_c + &
                   14_c_int * nlayer_c + 3_c_int * int(nsurf, c_int)
   else
      n_expected = SUEWS_CAPI_HEAT_STATE_BASE_LEN
   end if
   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   surf_vec_len = 0_c_int
   if (nlayer_c>0_c_int) surf_vec_len = int(nsurf, c_int)

   idx = 1_c_int

   call pack_mat_inline(s%temp_roof, flat, idx, nlayer_c, ndepth_c)
   call pack_mat_inline(s%temp_wall, flat, idx, nlayer_c, ndepth_c)
   call pack_mat_inline(s%temp_surf, flat, idx, int(nsurf, c_int), ndepth_c)
   call pack_mat_inline(s%temp_surf_dyohm, flat, idx, int(nsurf, c_int), ndepth_c)

   call pack_vec_inline(s%tsfc_roof, flat, idx, nlayer_c)
   call pack_vec_inline(s%tsfc_wall, flat, idx, nlayer_c)
   call pack_vec_inline(s%tsfc_surf, flat, idx, surf_vec_len)
   call pack_vec_inline(s%tsfc_surf_dyohm, flat, idx, surf_vec_len)
   call pack_vec_inline(s%tsfc_roof_stepstart, flat, idx, nlayer_c)
   call pack_vec_inline(s%tsfc_wall_stepstart, flat, idx, nlayer_c)
   call pack_vec_inline(s%tsfc_surf_stepstart, flat, idx, surf_vec_len)

   call pack_vec_inline(s%qs_roof, flat, idx, nlayer_c)
   call pack_vec_inline(s%qn_roof, flat, idx, nlayer_c)
   call pack_vec_inline(s%qe_roof, flat, idx, nlayer_c)
   call pack_vec_inline(s%qh_roof, flat, idx, nlayer_c)
   call pack_vec_inline(s%qh_resist_roof, flat, idx, nlayer_c)

   call pack_vec_inline(s%qs_wall, flat, idx, nlayer_c)
   call pack_vec_inline(s%qn_wall, flat, idx, nlayer_c)
   call pack_vec_inline(s%qe_wall, flat, idx, nlayer_c)
   call pack_vec_inline(s%qh_wall, flat, idx, nlayer_c)
   call pack_vec_inline(s%qh_resist_wall, flat, idx, nlayer_c)

   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%qs_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%qn_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%qe0_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%qe_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%qh_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%qh_resist_surf(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      flat(idx) = s%tsurf_ind(i); idx = idx + 1_c_int
   end do

   flat(idx) = s%qh_lumps; idx = idx + 1_c_int
   flat(idx) = s%qe_lumps; idx = idx + 1_c_int
   flat(idx) = s%kclear; idx = idx + 1_c_int
   flat(idx) = s%kup; idx = idx + 1_c_int
   flat(idx) = s%ldown; idx = idx + 1_c_int
   flat(idx) = s%lup; idx = idx + 1_c_int
   flat(idx) = s%qe; idx = idx + 1_c_int
   flat(idx) = s%qf; idx = idx + 1_c_int
   flat(idx) = s%qf_sahp; idx = idx + 1_c_int
   flat(idx) = s%qh; idx = idx + 1_c_int
   flat(idx) = s%qh_residual; idx = idx + 1_c_int
   flat(idx) = s%qh_resist; idx = idx + 1_c_int
   flat(idx) = s%qn; idx = idx + 1_c_int
   flat(idx) = s%qn_snowfree; idx = idx + 1_c_int
   flat(idx) = s%qs; idx = idx + 1_c_int
   flat(idx) = s%tsfc_c; idx = idx + 1_c_int
   flat(idx) = s%tsurf; idx = idx + 1_c_int
   flat(idx) = s%qh_init; idx = idx + 1_c_int

   do i = 1_c_int, 15_c_int
      flat(idx) = s%roof_in_sw_spc(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, 15_c_int
      flat(idx) = s%roof_in_lw_spc(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, 15_c_int
      flat(idx) = s%wall_in_sw_spc(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, 15_c_int
      flat(idx) = s%wall_in_lw_spc(i); idx = idx + 1_c_int
   end do

   flat(idx) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_heat_state

subroutine pack_roughness_state(s, flat, n_flat, err)
   implicit none
   type(ROUGHNESS_STATE), intent(in) :: s
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_ROUGHNESS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   flat(1) = s%faibldg_use
   flat(2) = s%faievetree_use
   flat(3) = s%faidectree_use
   flat(4) = s%fai
   flat(5) = s%pai
   flat(6) = s%zh
   flat(7) = s%z0m
   flat(8) = s%z0v
   flat(9) = s%zdm
   flat(10) = s%zzd
   flat(11) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_roughness_state

subroutine pack_stebbs_state(s, flat, n_flat, err)
   implicit none
   type(STEBBS_STATE), intent(in) :: s
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err
   integer(c_int) :: idx
   integer(c_int) :: i
   integer(c_int) :: bldg_count

   if (n_flat<SUEWS_CAPI_STEBBS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int

   flat(idx) = s%kdown2d; idx = idx + 1_c_int
   flat(idx) = s%kup2d; idx = idx + 1_c_int
   flat(idx) = s%kwest; idx = idx + 1_c_int
   flat(idx) = s%ksouth; idx = idx + 1_c_int
   flat(idx) = s%knorth; idx = idx + 1_c_int
   flat(idx) = s%keast; idx = idx + 1_c_int
   flat(idx) = s%ldown2d; idx = idx + 1_c_int
   flat(idx) = s%lup2d; idx = idx + 1_c_int
   flat(idx) = s%lwest; idx = idx + 1_c_int
   flat(idx) = s%lsouth; idx = idx + 1_c_int
   flat(idx) = s%lnorth; idx = idx + 1_c_int
   flat(idx) = s%least; idx = idx + 1_c_int

   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = s%zarray(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = s%dataoutlineursl(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = s%dataoutlinetrsl(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = s%dataoutlineqrsl(i); idx = idx + 1_c_int
   end do

   flat(idx) = s%deepsoiltemperature; idx = idx + 1_c_int
   flat(idx) = s%outdoorairstarttemperature; idx = idx + 1_c_int
   flat(idx) = s%indoorairstarttemperature; idx = idx + 1_c_int
   flat(idx) = s%indoormassstarttemperature; idx = idx + 1_c_int
   flat(idx) = s%wallindoorsurfacetemperature; idx = idx + 1_c_int
   flat(idx) = s%walloutdoorsurfacetemperature; idx = idx + 1_c_int
   flat(idx) = s%roofindoorsurfacetemperature; idx = idx + 1_c_int
   flat(idx) = s%roofoutdoorsurfacetemperature; idx = idx + 1_c_int
   flat(idx) = s%windowindoorsurfacetemperature; idx = idx + 1_c_int
   flat(idx) = s%windowoutdoorsurfacetemperature; idx = idx + 1_c_int
   flat(idx) = s%groundfloorindoorsurfacetemperature; idx = idx + 1_c_int
   flat(idx) = s%groundflooroutdoorsurfacetemperature; idx = idx + 1_c_int
   flat(idx) = s%watertanktemperature; idx = idx + 1_c_int
   flat(idx) = s%internalwallwatertanktemperature; idx = idx + 1_c_int
   flat(idx) = s%externalwallwatertanktemperature; idx = idx + 1_c_int
   flat(idx) = s%mainswatertemperature; idx = idx + 1_c_int
   flat(idx) = s%domestichotwatertemperatureinuseinbuilding; idx = idx + 1_c_int
   flat(idx) = s%internalwalldhwvesseltemperature; idx = idx + 1_c_int
   flat(idx) = s%externalwalldhwvesseltemperature; idx = idx + 1_c_int
   flat(idx) = s%qs_stebbs; idx = idx + 1_c_int

   bldg_count = 0_c_int
   if (allocated(s%buildings)) bldg_count = int(size(s%buildings), c_int)
   flat(idx) = real(bldg_count, c_double); idx = idx + 1_c_int

   flat(idx) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_stebbs_state

subroutine pack_nhood_state(s, flat, n_flat, err)
   implicit none
   type(NHOOD_STATE), intent(in) :: s
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_NHOOD_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   flat(1) = s%u_hbh_1dravg
   flat(2) = s%qn_1dravg
   flat(3) = s%tair_mn_prev
   flat(4) = s%iter_count
   flat(5) = merge(1.0_c_double, 0.0_c_double, s%iter_safe)

   err = SUEWS_CAPI_OK
end subroutine pack_nhood_state

end module module_c_api_driver

module c_api_driver_module
use module_c_api_driver
end module c_api_driver_module
