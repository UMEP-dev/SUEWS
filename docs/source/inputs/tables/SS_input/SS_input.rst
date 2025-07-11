SUEWS-SPARTACUS (SS) input files
--------------------------------



To run SUEWS-SS the SS specific files that need to be modified are:

- `RunControl.nml`

- `SUEWS_SPARTACUS.nml`

Non-SS specific SUEWS input file parameters also need to have appropriate values.
For example, LAI, albedos and emissivities are used by SUEWS-SS as explained in `more_SS_details`.


RunControl.nml
~~~~~~~~~~~~~~~
See `NetRadiationMethod` (sensible values are 1001, 1002 or 1003) in `RunControl.nml` parameter.


SUEWS_SPARTACUS.nml
~~~~~~~~~~~~~~~~~~~~


This file is used to specify the SS model options when coupled to SUEWS.


A sample file of **SUEWS_SPARTACUS.nml** is shown below:

.. literalinclude:: SUEWS_SPARTACUS.nml


The parameters and their setting instructions are provided through the links below:


Geometry-related options
^^^^^^^^^^^^^^^^^^^^^^^^^
.. hlist::
  + :option:`nlayers`
  + :option:`n_vegetation_region_urban`
  + :option:`height`
  + :option:`building_frac`
  + :option:`building_scale`
  + :option:`veg_frac`
  + :option:`veg_scale`
  + :option:`veg_contact_fraction`
  + :option:`wall_specular_frac`

Shortwave-related options
^^^^^^^^^^^^^^^^^^^^^^^^^

.. hlist::
  + :option:`use_sw_direct_albedo`
  + :option:`sw_dn_direct_frac`
  + :option:`n_stream_sw_urban`
  + :option:`air_ext_sw`
  + :option:`air_ssa_sw`
  + :option:`veg_ssa_sw`
  + :option:`ground_albedo_dir_mult_fact`
  + :option:`roof_albedo`
  + :option:`wall_albedo`

Longwave-related options
^^^^^^^^^^^^^^^^^^^^^^^^^

.. hlist::
  + :option:`n_stream_lw_urban`
  + :option:`air_ext_lw`
  + :option:`air_ssa_lw`
  + :option:`veg_ssa_lw`
  + :option:`veg_fsd`
  + :option:`roof_emissivity`
  + :option:`wall_emissivity`
  + :option:`roof_albedo_dir_mult_fact`



.. .. toctree::
..    :maxdepth: 1
..    :hidden:

..    SS_input
