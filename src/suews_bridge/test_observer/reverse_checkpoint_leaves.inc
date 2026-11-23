! Test-only host procedure in module_c_api_driver. Initialise the aggregate via the
! normal input codec, then write named kernel members independently of its layout.
subroutine suews_obs_reverse_checkpoint(nlayer, ndepth, seeds, source, nflat, toc, ntoc, packed, err) bind(C)
   integer(c_int), value :: nlayer, ndepth, nflat, ntoc
   real(c_double), intent(in) :: seeds(3), source(nflat)
   integer(c_int), intent(in) :: toc(ntoc)
   real(c_double), intent(out) :: packed(nflat)
   integer(c_int), intent(out) :: err
   type(SUEWS_STATE) :: state

   call unpack_state_members(state, int(nlayer), int(ndepth), source, nflat, toc, ntoc, &
                             SUEWS_CAPI_STATE_MEMBER_COUNT, err)
   if (err /= SUEWS_CAPI_OK) return

   associate (stebbs => state%stebbsState, heat => state%heatState, hydro => state%hydroState)
      stebbs%indoor_air_start_temperature = seeds(1)
      stebbs%indoor_mass_start_temperature = seeds(2)
      stebbs%wall_indoor_surface_temperature = seeds(3)

      call set_matrix(heat%temp_roof, 1)
      call set_matrix(heat%temp_wall, 2)
      call set_matrix(heat%temp_surf, 3)
      call set_matrix(heat%temp_surf_dyohm, 4)
      call set_vector(heat%tsfc_roof, 5)
      call set_vector(heat%tsfc_wall, 6)
      call set_vector(heat%tsfc_surf, 7)
      call set_vector(heat%tsfc_surf_dyohm, 8)
      call set_vector(heat%tsfc_roof_stepstart, 9)
      call set_vector(heat%tsfc_wall_stepstart, 10)
      call set_vector(heat%tsfc_surf_stepstart, 11)
      call set_vector(heat%qs_roof, 12)
      call set_vector(heat%qn_roof, 13)
      call set_vector(heat%qe_roof, 14)
      call set_vector(heat%qh_roof, 15)
      call set_vector(heat%qh_resist_roof, 16)
      call set_vector(heat%qs_wall, 17)
      call set_vector(heat%qn_wall, 18)
      call set_vector(heat%qe_wall, 19)
      call set_vector(heat%qh_wall, 20)
      call set_vector(heat%qh_resist_wall, 21)

      call set_vector(hydro%soil_store_roof, 22)
      call set_vector(hydro%state_roof, 23)
      call set_vector(hydro%soil_store_wall, 24)
      call set_vector(hydro%state_wall, 25)
      call set_vector(hydro%ev_roof, 26)
      call set_vector(hydro%ev_wall, 27)
   end associate

   call pack_state_to_output(state, int(nlayer), int(ndepth), packed, nflat, toc, ntoc, &
                             SUEWS_CAPI_STATE_MEMBER_COUNT, err)
contains
   ! The integer part identifies the field. Each index has its own binary fraction.
   subroutine set_vector(field, tag)
      real(c_double), intent(out) :: field(:)
      integer, intent(in) :: tag
      integer :: i
      do i = 1, size(field)
         field(i) = tag + i / 16.0_c_double
      end do
   end subroutine set_vector

   subroutine set_matrix(field, tag)
      real(c_double), intent(out) :: field(:, :)
      integer, intent(in) :: tag
      integer :: i, j
      do j = 1, size(field, 2)
         do i = 1, size(field, 1)
            field(i, j) = tag + i / 16.0_c_double + j / 1024.0_c_double
         end do
      end do
   end subroutine set_matrix
end subroutine suews_obs_reverse_checkpoint
