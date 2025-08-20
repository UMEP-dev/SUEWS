.. meta::
   :description: SUEWS YAML configuration for spartacus parameters
   :keywords: SUEWS, YAML, spartacusparams, parameters, configuration

.. _spartacusparams:

.. index::
   single: SPARTACUSParams (YAML parameter)
   single: YAML; SPARTACUSParams

SPARTACUS
=========

SPARTACUS radiation model parameters.

Controls the SPARTACUS-Surface radiation scheme for detailed
3D radiation interactions in urban environments.

**Parameters:**

.. index::
   single: air_ext_lw (YAML parameter)
   single: SPARTACUSParams; air_ext_lw

.. option:: air_ext_lw

   Air extinction coefficient for longwave radiation

   :Unit: |m^-1|
   :Sample value: ``0.0``

.. index::
   single: air_ext_sw (YAML parameter)
   single: SPARTACUSParams; air_ext_sw

.. option:: air_ext_sw

   Air extinction coefficient for shortwave radiation

   :Unit: |m^-1|
   :Sample value: ``0.0``

.. index::
   single: air_ssa_lw (YAML parameter)
   single: SPARTACUSParams; air_ssa_lw

.. option:: air_ssa_lw

   Air single scattering albedo for longwave radiation

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: air_ssa_sw (YAML parameter)
   single: SPARTACUSParams; air_ssa_sw

.. option:: air_ssa_sw

   Air single scattering albedo for shortwave radiation

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: ground_albedo_dir_mult_fact (YAML parameter)
   single: SPARTACUSParams; ground_albedo_dir_mult_fact

.. option:: ground_albedo_dir_mult_fact

   Multiplication factor for direct ground albedo

   :Unit: dimensionless
   :Sample value: ``1.0``

.. index::
   single: n_stream_lw_urban (YAML parameter)
   single: SPARTACUSParams; n_stream_lw_urban

.. option:: n_stream_lw_urban

   Number of streams for longwave radiation in urban areas

   :Unit: dimensionless
   :Sample value: ``2``

.. index::
   single: n_stream_sw_urban (YAML parameter)
   single: SPARTACUSParams; n_stream_sw_urban

.. option:: n_stream_sw_urban

   Number of streams for shortwave radiation in urban areas

   :Unit: dimensionless
   :Sample value: ``2``

.. index::
   single: n_vegetation_region_urban (YAML parameter)
   single: SPARTACUSParams; n_vegetation_region_urban

.. option:: n_vegetation_region_urban

   Number of vegetation regions in urban areas

   :Unit: dimensionless
   :Sample value: ``1``

.. index::
   single: sw_dn_direct_frac (YAML parameter)
   single: SPARTACUSParams; sw_dn_direct_frac

.. option:: sw_dn_direct_frac

   Fraction of downward shortwave radiation that is direct

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: use_sw_direct_albedo (YAML parameter)
   single: SPARTACUSParams; use_sw_direct_albedo

.. option:: use_sw_direct_albedo

   Flag to use direct albedo for shortwave radiation

   :Unit: dimensionless
   :Sample value: ``1.0``

.. index::
   single: veg_contact_fraction_const (YAML parameter)
   single: SPARTACUSParams; veg_contact_fraction_const

.. option:: veg_contact_fraction_const

   Constant vegetation contact fraction

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: veg_fsd_const (YAML parameter)
   single: SPARTACUSParams; veg_fsd_const

.. option:: veg_fsd_const

   Constant vegetation fractional standard deviation

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: veg_ssa_lw (YAML parameter)
   single: SPARTACUSParams; veg_ssa_lw

.. option:: veg_ssa_lw

   Vegetation single scattering albedo for longwave radiation

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: veg_ssa_sw (YAML parameter)
   single: SPARTACUSParams; veg_ssa_sw

.. option:: veg_ssa_sw

   Vegetation single scattering albedo for shortwave radiation

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: ref (YAML parameter)
   single: SPARTACUSParams; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
