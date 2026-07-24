! radsurf_simple_spectrum.f90 - Simplest calculation of spectral properties
!
! (C) Copyright 2020- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
!

module radsurf_simple_spectrum

contains
  
  subroutine calc_simple_spectrum_lw(config, canopy_props, lw_spectral_props, &
       &                             istartcol, iendcol)
    
    use parkind1,                   only : jpim, jprb
    use yomhook,                    only : lhook, dr_hook
    use radiation_io,               only : radiation_abort
    use radsurf_config,             only : config_type
    use radsurf_canopy_properties,  only : canopy_properties_type
    use radsurf_lw_spectral_properties,only: lw_spectral_properties_type
    use radiation_constants,        only : StefanBoltzmann
    
    implicit none
    
    type(config_type),             intent(in) :: config
    type(canopy_properties_type),  intent(in) :: canopy_props
    type(lw_spectral_properties_type),intent(inout) :: lw_spectral_props
    integer(kind=jpim),            intent(in) :: istartcol, iendcol
    
    ! Start and end layers for current column
    integer(kind=jpim) :: ilay1, ilay2

    ilay1 = canopy_props%istartlay(istartcol)
    ilay2 = canopy_props%istartlay(iendcol) + canopy_props%nlay(iendcol) - 1

    if (lw_spectral_props%nspec > 1) then
      call radiation_abort('Simple longwave spectrum only possible with one input spectral interval')
    end if
    
    lw_spectral_props%ground_emission(1,istartcol:iendcol) = StefanBoltzmann &
         &  * lw_spectral_props%ground_emissivity(1,istartcol:iendcol) &
         &  * canopy_props%ground_temperature(istartcol:iendcol) ** 4
    if (ilay2 >= ilay1) then
          if (config%do_urban) then
               if (allocated(lw_spectral_props%roof_emission)) then
               lw_spectral_props%roof_emission(1,ilay1:ilay2) = StefanBoltzmann &
                    & * lw_spectral_props%roof_emissivity(1,ilay1:ilay2) &
                    & * canopy_props%roof_temperature(ilay1:ilay2) ** 4
               end if
               if (allocated(lw_spectral_props%wall_emission)) then
               lw_spectral_props%wall_emission(1,ilay1:ilay2) = StefanBoltzmann &
                    & * lw_spectral_props%wall_emissivity(1,ilay1:ilay2) &
                    & * canopy_props%wall_temperature(ilay1:ilay2) ** 4
               end if
          end if
      lw_spectral_props%clear_air_planck(1,ilay1:ilay2) = StefanBoltzmann &
           &  * canopy_props%clear_air_temperature(ilay1:ilay2) ** 4
      if (config%do_vegetation) then
        lw_spectral_props%veg_planck(1,ilay1:ilay2) = StefanBoltzmann &
             &  * canopy_props%veg_temperature(ilay1:ilay2) ** 4
        lw_spectral_props%veg_air_planck(1,ilay1:ilay2) = StefanBoltzmann &
             &  * canopy_props%veg_air_temperature(ilay1:ilay2) ** 4
      end if
    end if

  end subroutine calc_simple_spectrum_lw

end module radsurf_simple_spectrum
