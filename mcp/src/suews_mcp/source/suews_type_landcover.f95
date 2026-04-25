module module_type_landcover

   use module_type_surface, only: OHM_PRM
   use module_type_soil, only: SOIL_PRM
   use module_type_waterdist, only: WATER_DIST_PRM
   use module_type_vegetation, only: bioCO2_PRM, LAI_PRM

   implicit none

   TYPE, PUBLIC :: LC_PAVED_PRM
      ! land cover specific parameters for paved surfaces
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: state = 0.0D0
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracpaved = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_PAVED_PRM

   TYPE, PUBLIC :: LC_BLDG_PRM
      ! land cover specific parameters for buildings
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: faibldg = 0.0D0
      REAL(KIND(1D0)) :: bldgh = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: state = 0.0D0
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracbldgs = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_BLDG_PRM

   TYPE, PUBLIC :: LC_DECTR_PRM
      ! land cover specific parameters for deciduous trees
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      REAL(KIND(1D0)) :: faidectree = 0.0D0
      REAL(KIND(1D0)) :: dectreeh = 0.0D0
      REAL(KIND(1D0)) :: pormin_dec = 0.0D0 ! absent for evergreen trees ??
      REAL(KIND(1D0)) :: pormax_dec = 0.0D0
      REAL(KIND(1D0)) :: alb_min = 0.0D0
      REAL(KIND(1D0)) :: alb_max = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm] ! ******* dummy variable *******
      REAL(KIND(1D0)) :: capmax_dec = 0.0D0 ! Maximum water storage capacity for upper surfaces (i.e. canopy) (absent for evergreen trees ??)
      REAL(KIND(1D0)) :: capmin_dec = 0.0D0 ! Minimum water storage capacity for upper surfaces (i.e. canopy).
      REAL(KIND(1D0)) :: irrfracdectr = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0
      TYPE(bioCO2_PRM) :: bioco2
      REAL(KIND(1D0)) :: maxconductance = 0.0D0 ! the maximum conductance of each vegetation or surface type. [mm s-1]
      ! TYPE(CONDUCTANCE_PRM) :: conductance
      TYPE(LAI_PRM) :: lai
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_DECTR_PRM

   TYPE, PUBLIC :: LC_EVETR_PRM
      ! land cover specific parameters for evergreen trees
      REAL(KIND(1D0)) :: sfr = 0.0D0 !surface cover fraction[-]
      REAL(KIND(1D0)) :: emis = 0.0D0 !Effective surface emissivity[-]
      REAL(KIND(1D0)) :: faievetree = 0.0D0 !frontal area index for evergreen tree [-]
      REAL(KIND(1D0)) :: evetreeh = 0.0D0 !height of evergreen tree [m]
      REAL(KIND(1D0)) :: alb_min = 0.0D0 !minimum albedo for evergreen tree [-]
      REAL(KIND(1D0)) :: alb_max = 0.0D0 !maximum albedo for evergreen tree [-]
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracevetr = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0 !surface wetness threshold [mm], When State > WetThresh, RS=0 limit in SUEWS_evap [mm]
      TYPE(bioCO2_PRM) :: bioco2
      ! TYPE(CONDUCTANCE_PRM) :: conductance
      REAL(KIND(1D0)) :: maxconductance = 0.0D0 ! the maximum conductance of each vegetation or surface type. [mm s-1]
      TYPE(LAI_PRM) :: lai
      TYPE(WATER_DIST_PRM) :: waterdist !Fraction of water redistribution [-]
   END TYPE LC_EVETR_PRM

   TYPE, PUBLIC :: LC_GRASS_PRM
      ! land cover specific parameters for grass
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      REAL(KIND(1D0)) :: alb_min = 0.0D0
      REAL(KIND(1D0)) :: alb_max = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracgrass = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0
      TYPE(bioCO2_PRM) :: bioco2
      ! TYPE(CONDUCTANCE_PRM) :: conductance
      REAL(KIND(1D0)) :: maxconductance = 0.0D0 ! the maximum conductance of each vegetation or surface type. [mm s-1]
      TYPE(LAI_PRM) :: lai
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_GRASS_PRM

   TYPE, PUBLIC :: LC_BSOIL_PRM
      ! land cover specific parameters for bare soil
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracbsoil = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_BSOIL_PRM

   TYPE, PUBLIC :: LC_WATER_PRM
      ! land cover specific parameters for water surface
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracwater = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0 ! ******* dummy variable *******
      REAL(KIND(1D0)) :: flowchange = 0.0D0 ! special term in water
   END TYPE LC_WATER_PRM

end module module_type_landcover
