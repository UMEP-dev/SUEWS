

SUEWS-SPARTACUS input options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



.. option:: nlayers

  :Requirement:
    `MD`

  :Type:
    Integer

  :Description:
    Number of vertical layers in the canopy

  :Configuration:
    For maximum values, see the used soil code in `SUEWS_Soil.txt`


.. option:: use_sw_direct_albedo

  :Requirement:
    `MD`
  :Type:
    Boolean
  :Description:
    Specify ground and roof albedos separately for direct solar radiation
  :Configuration:
    - Only considered when `sw_dn_direct_frac`>0.
    - False (default) - Same ground albedo for diffuse and direct solar radiation |Recmd|
    - True - Specify ground albedo separately for direct solar radiation

.. option:: n_vegetation_region_urban

  :Requirement:
    `MD`
  :Type:
    Integer
  :Description:
    Number of regions used to describe vegetation
  :Configuration:
    - 1 (default): heterogeneity of vegetation not considered. might be okay de pending on the level of accuracy needed. See :cite:t:`Hogan2018Jan` – details of SPARTACUS-Vegetation for more information.
    - 2: heterogeneity of vegetation considered


.. option:: n_stream_sw_urban

  :Requirement:
    `MD`
  :Type:
    Integer
  :Description:
    SW diffuse streams per hemisphere (note: this is the number of quadrature points so a value of 4 corresponds to an ‘8-stream scheme’)
  :Configuration:
    4 is recommended. At large computational cost small improvements in accuracy can be made by increasing from 4 :cite:`Hogan2019Oct`.



.. option:: n_stream_lw_urban

  :Requirement:
    `MD`
  :Type:
    Integer
  :Description:
    LW streams per hemisphere (note: this is the number of quadrature points so a value of 4 corresponds to an ‘8-stream scheme’)
  :Configuration:
    4 is recommended. At large computational cost small improvements in accuracy can be made by increasing from 4 :cite:`Hogan2019Oct`.


.. option:: sw_dn_direct_frac

  :Requirement:
    `MD`
  :Type:
    Float
  :Description:
    Fraction of down-welling shortwave radiation that is direct
  :Configuration:
    0.45 is based on :cite:t:`B20` (Belgium and Berlin annual average), but could be improved.


.. option:: air_ext_sw

  :Requirement:
    `MD`
  :Type:
    Float
  :Description:
    Shortwave wavelength-independent air extinction coefficient (|m^-1|) (i.e. number of radiance e-foldings per metre)
  :Configuration:
    0.0 :cite:`Hogan2019Oct`. Reasonable approximation (personal communication Robin Hogan).



.. option:: air_ssa_sw

  :Requirement:
    `MD`
  :Type:
    Float
  :Description:
    Sh ortwave single sca ttering albedo of air
  :Configuration:
    - 0.95 :cite:`Hogan2019Oct`
    - `air_ext_sw` is not used if `air_ext_sw=0.0 <air_ext_sw>`.


.. option:: veg_ssa_sw

  :Requirement:
    `MD`
  :Type:
    Float
  :Description:
    Shortwave single scattering albedo of leaves
  :Configuration:
    Broadband shortwave vegetation SSA values ranged between 0.41 and 0.52 for RAMI5 Järvselja birch stand forest trees. 0.46 is the default value but users can choose another value if the dominant tree type is one of the RAMI5 Järvselja birch stand forest trees (see `Vegetation single scattering albedo (SSA)` for details).



.. option:: air_ext_lw

  :Requirement:
    `MD`
  :Type:
    Float
  :Description:
    Longwave wavelength-independent air extinction coefficient [|m^-1|] (i.e. number of radiance e-foldings per metre)
  :Configuration:
    0.0 is a bad approximation :cite:`Hogan2019Oct` but better representation requires several band treatment which is not in SS yet.



.. option:: air_ssa_lw

  :Requirement:
    `MD`
  :Type:
    Float
  :Description:
    Longwave single scattering albedo of air

  :Configuration:
    - 0.0 is from :cite:t:`Hogan2019Oct`.
    - `air_ssa_lw` is not used when `air_ext_lw=0.0 <air_ext_lw>`.



.. option:: veg_ssa_lw

  :Requirement:
    `MD`
  :Type:
    Float
  :Description:
    Longwave single scattering albedo of vegetation

  :Configuration:
    - 0.06 (|Recmd|)
    - Should be estimated using a vegetation type in https://speclib.jpl.nasa.gov/library (see `Vegetation single scattering albedo (SSA)` for details).
      - Reflectance is ~0.04 for Acer Pensylvanicum,
      - ~0.02 for Quercus Robur and
      - ~0.04 for Betula Lenta.
    - SSA ~ 2*reflectance so 0.06 is chosen as the default.




.. option:: veg_fsd

  :Requirement:
    `MD`
  :Type:
    Float
  :Description:
    Fractional standard deviation of the vegetation extinction. Determines the extinction coefficient in the inner and outer layers of the tree crown when n_vegetation_region_urban=2.
  :Configuration:
    - 0.75 (|Recmd|)
    - Robin has used 0.75 in SS for the RAMI-V radiation-vegetation inter-comparison, but should be updated based on the findings.


.. option:: veg_contact_fraction

  :Requirement:
    `MD`
  :Type:
    Float
  :Description:
    Fraction of vegetation edge in contact with building walls

  :Configuration:
    - Can be updated from 0.
    - If detailed knowledge of the canopy geometry is available.


.. option:: ground_albedo_dir_mult_fact

  :Requirement:
    `MD`
  :Type:
    Float
  :Description:
    Ratio of the direct and diffuse albedo of the ground

  :Configuration:
    - 1.0 (|Recmd|)
    - Can be updated from 1: if detailed knowledge of the direct and diffuse albedo is available.


.. option:: height

  :Requirement:
    `MU`
  :Type:
    Float array (dim: `nlayers` +1)
  :Description:
    Height of the layer interfaces [m]

  :Configuration:
    to-add

.. option:: building_frac

  :Requirement:
    `MU`
  :Type:
    Float array (dim: `nlayers`)
  :Description:
    Building plan area density
  :Configuration:
    Layer 1 `building_frac` should equal SUEWS `Fr_Bldgs`



.. option:: veg_frac

  :Requirement:
    `MU`
  :Type:
    Float array (dim: `nlayers`)
  :Description:
    Tree plan area density
  :Configuration:
    Layer 1 `veg_frac` should equal SUEWS `Fr_EveTr` + `Fr_DecTr`



.. option:: building_scale

  :Requirement:
    `MU`
  :Type:
    Float array (dim: `nlayers`)
  :Description:
    Building horizontal scale [m]
  :Configuration:
    Effective building diameter. Values can be estimated from inspecting buildings using Google Maps or GIS. It is used along with `building_frac` to calculate the average building perimeter length following Eq. 8 of `Spartacus surface documentation <https://github.com/ecmwf/spartacus-surface/blob/master/doc/spartacus_surface_documentation.pdf>`_.


.. option:: veg_scale

  :Requirement:
    `MU`
  :Type:
    Float array (dim: `nlayers`)
  :Description:
    Vegetation horizontal scale [m]
  :Configuration:
    Vegetation scale. Values can be estimated from inspecting vegetation using Google street view. It is used along with veg_fraction to calculate the average vegetation perimeter length following Eq. 2 of :cite:t:`Hogan2018Jan`.


.. option:: roof_albedo

  :Requirement:
    `MU`
  :Type:
    Float array (dim: `nlayers`)
  :Description:
    Roof albedo
  :Configuration:
    If values are not known then sensible values can be found in `SUEWS_NonVeg.txt`.


.. option:: wall_albedo

  :Requirement:
    `MU`
  :Type:
    Float array (dim: `nlayers`)
  :Description:
    Roof albedo
  :Configuration:
    If values are not known then sensible values can be found in `SUEWS_NonVeg.txt`.



.. option:: roof_emissivity

  :Requirement:
    `MU`
  :Type:
    Float array (dim: `nlayers`)
  :Description:
    Roof emissivity
  :Configuration:
    If values are not known then sensible values can be found in `SUEWS_NonVeg.txt`.


.. option:: wall_emissivity

  :Requirement:
    `MU`
  :Type:
    Float array (dim: `nlayers`)
  :Description:
    Wall emissivity
  :Configuration:
    If values are not known then sensible values can be found in `SUEWS_NonVeg.txt`.



.. option:: roof_albedo_dir_mult_fact

  :Requirement:
    `MD`
  :Type:
    Float array (dim: `nlayers`)
  :Description:
    Ratio of the direct and diffuse albedo of the roof
  :Configuration:
    - 1 is the default value.
    - Can be updated from 1. if detailed knowledge of the direct and diffuse albedo is available.


.. option:: wall_specular_frac

  :Requirement:
    `MD`
  :Type:
    Float array (dim: `nlayers`)
  :Description:
    Fraction of wall reflection that is specular
  :Configuration:
    - 0 is the default value.
    - Can be updated from 0. if the specular reflection is known.





