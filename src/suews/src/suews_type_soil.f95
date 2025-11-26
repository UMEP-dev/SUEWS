module module_type_soil

   implicit none

   TYPE, PUBLIC :: SOIL_PRM
      REAL(KIND(1D0)) :: soildepth = 0.0D0 ! Depth of soil beneath the surface [mm]
      REAL(KIND(1D0)) :: soilstorecap = 0.0D0 ! Capacity of soil store [mm]
      REAL(KIND(1D0)) :: sathydraulicconduct = 0.0D0 ! !Hydraulic conductivity for saturated soil [mm s-1]
   END TYPE SOIL_PRM

end module module_type_soil
