ESTM input files
-----------------------------


SUEWS_ESTMCoefficients.txt
~~~~~~~~~~~~~~~~~~~~~~~~~~

**Note ESTM is under development in this release and should not be used!**

The Element Surface Temperature Method (ESTM) :cite:`O05`
calculates the net storage heat flux from surface temperatures. In the
method the three-dimensional urban volume is reduced to four 1-d
elements (i.e. building roofs, walls, and internal mass and ground
(road, vegetation, etc)). The storage heat flux is calculated from the
heat conduction through the different elements. For the inside surfaces
of the roof and walls, and both surfaces for the internal mass
(ceilings/floors, internal walls), the surface temperature of the
element is determined by setting the conductive heat transfer out of (in
to) the surface equal to the radiative and convective heat losses
(gains). Each element (roof, wall, internal element and ground) can have
maximum five layers and each layer has three parameters tied to it:
thickness (x), thermal conductivity (k), volumetric heat capacity
(rhoCp).

If ESTM is used (`StorageHeatMethod` =4), the files
`SUEWS_ESTMCoefficients.txt`,
`ESTMinput.nml` and
`SSss_YYYY_ESTM_Ts_data_tt.txt`
should be prepared.

SUEWS_ESTMCoefficients.txt contains the parameters for the layers of
each of the elements (roofs, wall, ground, internal mass).

-  If less than five layers are used, the parameters for unused layers
   should be set to -999.
-  The ESTM coefficients with the prefix *Surf\_* must be specified for
   each surface type (plus snow) but the *Wall\_* and *Internal\_*
   variables apply to the building surfaces only.
-  For each grid, one set of ESTM coefficients must be specified for
   each surface type; for paved and building surfaces it is possible to
   specify up to three and five sets of coefficients per grid (e.g. to
   represent different building materials) using the relevant columns in
   `SUEWS_SiteSelect.txt`. For the model to
   use these columns in site select, the ESTMCode column in
   `SUEWS_NonVeg.txt` should be set to zero.


The following input files are required if ESTM is used to calculate the
storage heat flux.



ESTMinput.nml
~~~~~~~~~~~~~

ESTMinput.nml specifies the model settings and default values.

A sample file of **ESTMinput.nml** looks like

.. literalinclude:: ESTMinput.nml

.. note::  The file contents can be in any order.

The parameters and their setting instructions
are provided through :ref:`the links below <ESTMinput>`:

.. hlist::
    + :option:`TsurfChoice`
    + :option:`evolveTibld`
    + :option:`IbldCHmod`
    + :option:`LBC_soil`
    + :option:`Theat_fix`
    + :option:`Theat_off`
    + :option:`Theat_on`

.. toctree::
   :maxdepth: 1
   :hidden:

   ESTMinput



SSss_YYYY_ESTM_Ts_data_tt.txt
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`SSss_YYYY_ESTM_Ts_data_tt.txt` contains a time-series of input surface
temperature for roof, wall, ground and internal elements.

.. csv-table::
  :file: SSss_YYYY_ESTM_Ts_data_tt.csv
  :header-rows: 1
  :widths: auto
