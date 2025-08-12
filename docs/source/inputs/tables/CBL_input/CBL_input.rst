

CBL input files
---------------

Main references for this part of the model: :cite:t:`O15` and :cite:t:`CG01`.

If CBL slab model is used (:option:`CBLUse = 1 <CBLUse>` in :ref:`RunControl.nml <RunControl>`) the following files are needed.


.. list-table::
   :widths: 50 50
   :header-rows: 1

   * - Filename
     - Purpose
   * - `CBL_initial_data.txt`
     - Gives initial data every morning
       * when CBL slab model starts running.
       * filename must match the InitialData_FileName in CBLInput.nml
       * fixed formats.
   * - `CBLInput.nml`
     - Specifies run options, parameters and input file names.
       * Can be in any order



CBL_initial_data.txt
~~~~~~~~~~~~~~~~~~~~

This file should give initial data every morning when CBL slab model starts running.
The file name should match the InitialData_FileName in CBLInput.nml.

Definitions and example file of initial values prepared for Sacramento.

.. list-table::
   :widths: auto
   :header-rows: 1

   * - No.
     - Column name
     - Description
   * - 1
     - id
     - Day of year [DOY]
   * - 2
     - zi0
     - Initial convective  boundary layer height (m)
   * - 3
     - gamt_Km
     - Vertical gradient of potential temperature (K |m^-1|) strength of the inversion
   * - 4
     - gamq_gkgm
     - Vertical gradient of specific humidity (g kg\ :sup:`-1` m\ :sup:`-1`)
   * - 5
     - Theta+_K
     - Potential temperature at the top of CBL (K)
   * - 6
     - q+_gkg
     - Specific humidity at the top of CBL (g kg\ :sup:`-1`)
   * - 7
     - Theta_K
     - Potential temperature in CBL (K)
   * - 8
     - q_gkg
     - Specific humidiy in CBL (g kg\ :sup:`-1`)

-  gamt_Km and gamq_gkgm written to two significant figures are required
   for the model performance in appropriate ranges :cite:`O15`.

.. list-table::
   :widths: auto
   :header-rows: 1

   * - id
     - zi0
     - gamt_Km
     - gamq_gkgm
     - Theta+_K
     - q+_gkg
     - theta_K
     - q_gkg
   * - 234
     - 188
     - 0.0032
     - 0.00082
     - 290.4
     - 9.6
     - 288.7
     - 8.3
   * - 235
     - 197
     - 0.0089
     - 0.089
     - 290.2
     - 8.4
     - 288.3
     - 8.7
   * - ︙
     - ︙
     - ︙
     - ︙
     - ︙
     - ︙
     - ︙
     - ︙
   * - ︙
     - ︙
     - ︙
     - ︙
     - ︙
     - ︙
     - ︙
     - ︙
   * -
     -
     -
     -
     -
     -
     -
     -



CBLInput.nml
~~~~~~~~~~~~~

sample file of **CBLInput.nml** looks like

.. literalinclude:: CBLInput.nml

.. note::  The file contents can be in any order.

The parameters and their setting instructions
are provided through :ref:`the links below <CBLinput>`:

.. hlist::
  + :option:`EntrainmentType`
  + :option:`QH_Choice`
  + :option:`InitialData_use`
  + :option:`Sondeflag`
  + :option:`CBLday(id)`
  + :option:`CO2_included`
  + :option:`FileSonde(id)`
  + :option:`InitialDataFileName`
  + :option:`Wsb`

.. toctree::
   :maxdepth: 1
   :hidden:

   CBLinput
