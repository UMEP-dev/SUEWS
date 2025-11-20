.. _output_text_format:

Text Format Output Files
=========================

When using text format, SUEWS produces separate output files for each year, grid, and output group.

File Naming Convention
----------------------

Text output files follow the pattern: ``SSss_YYYY_GROUP_TT.txt``

- **ss**: Grid ID from SiteSelect
- **YYYY**: Year
- **GROUP**: Output group name (SUEWS, snow, ESTM, RSL, BL, DailyState)
- **TT**: Time resolution in minutes (omitted for DailyState)

**Example**: ``London_KCL_2020_SUEWS_60.txt`` (London KCL site, year 2020, SUEWS group, hourly output)

Configuration
-------------

**YAML Configuration** (Recommended):

Control output via ``output_file`` in the YAML configuration:

.. code-block:: yaml

   model:
     control:
       output_file:
         format: txt              # Text format
         freq: 3600               # Output frequency (seconds)
         groups:                  # Which groups to save
           - SUEWS               # Main output (always recommended)
           - DailyState          # Daily state (always recommended)
           - snow                # Optional: snow output
           - ESTM                # Optional: surface temperature details
           - RSL                 # Optional: vertical profiles

See :doc:`../inputs/yaml/config-reference/outputconfig` for complete configuration options.

**Namelist Configuration** (Legacy):

For namelist-based runs, output is controlled by:

- :option:`WriteOutOption` - Output level (0=minimal, 1=extended, 2=all+snow)
- :option:`ResolutionFilesOut` - Time resolution in minutes

See :ref:`RunControl.nml` for details.

Output File Groups
------------------

SSss_YYYY_SUEWS_TT.txt
~~~~~~~~~~~~~~~~~~~~~~

**Content:** Core SUEWS energy balance, water balance, and meteorological outputs.

**Variables:** 85 variables including radiation components, energy fluxes, water fluxes, and meteorological variables.

**Documentation:** :doc:`variables/suews`

**Key variables:**

- Radiation: Kdown, Kup, Ldown, Lup, QN
- Energy balance: QH, QE, QS, QF
- Water balance: Rain, Evap, Runoff, SMD
- Meteorology: T2, RH2, U10, Press


SSss_DailyState.txt
~~~~~~~~~~~~~~~~~~~

**Content:** Daily accumulated state variables for surface, soil, and vegetation.

**Variables:** 47 variables including soil moisture, leaf area index, snow conditions, and accumulated fluxes.

**Documentation:** :doc:`variables/dailystate`

**Frequency:** Daily only (no time resolution suffix in filename)

**Key variables:**

- Soil state: SoilState_[Surface], Mw_[Surface]
- Vegetation: LAI_[Surface], GDD, SDD
- Snow: SnowWater, SnowFrac
- Accumulated values: DailyPrec, Tmax, Tmin

InitialConditionsSSss_YYYY.nml
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At the end of each year (or end of run), a new InitialConditions file is written to the input folder for each grid. This file contains the final model state and can be used to restart simulations. See :ref:`Initial_Conditions` for details.

SSss_YYYY_snow_TT.txt
~~~~~~~~~~~~~~~~~~~~~

**Content:** Snow-specific outputs for each surface type.

**Variables:** 98 variables including snow depth, density, albedo, and melt rates for each surface cover.

**Documentation:** :doc:`variables/snow`

**Enabled when:** :option:`SnowUse` = 1 in RunControl.nml or physics.snowuse in YAML

**Key variables:**

- Per-surface snow depth: SnowDepth_Paved, SnowDepth_Bldgs, etc.
- Snow properties: SnowDens_[Surface], SnowAlb_[Surface]
- Snow processes: Melt_[Surface], SnowRemoval_[Surface]

SSss_YYYY_RSL_TT.txt
~~~~~~~~~~~~~~~~~~~~

**Content:** Vertical profiles in the roughness sublayer at 30 height levels.

**Variables:** 135 variables (45 per variable × 3 variables: wind speed, temperature, humidity)

**Documentation:** :doc:`variables/rsl`

**Height levels:** See :ref:`rsl_mod` for the 30 standard levels

**Key variables:**

- Wind profiles: U_RSL_[Level] (30 levels)
- Temperature profiles: T_RSL_[Level] (30 levels)
- Humidity profiles: Q_RSL_[Level] (30 levels)

SSss_YYYY_BL_TT.txt
~~~~~~~~~~~~~~~~~~~

**Content:** Boundary layer meteorological variables from the CBL module.

**Variables:** 17 variables including mixed layer height, entrainment, and boundary layer structure.

**Documentation:** :doc:`variables/bl`

**Enabled when:** CBL module is active (see :ref:`CBL input files`)

**Key variables:**

- BL height: zi (mixed layer height)
- Entrainment: we (entrainment velocity)
- Temperature: Theta_BL, DeltaTheta


.. TODO: #63 add BEERS output description based on SOLWEIG output
.. SOLWEIG is fully removed since 2019a

.. SOLWEIGpoiOut.txt
.. ~~~~~~~~~~~~~~~~~

.. Calculated variables from POI, point of interest (row, col) stated in
.. `SOLWEIGinput.nml`.

.. SOLWEIG model output file format: SOLWEIGpoiOUT.txt


.. .. csv-table::
..   :file: SOLWEIGpoiOut.csv
..   :header-rows: 1
..   :widths: auto



SSss_YYYY_ESTM_TT.txt
~~~~~~~~~~~~~~~~~~~~~

**Content:** Element Surface Temperature Model outputs for detailed surface and building thermal analysis.

**Variables:** 27 variables including layer temperatures, storage heat fluxes, and indoor air temperature.

**Documentation:** :doc:`variables/estm`

**Enabled when:** ESTM model is activated in model configuration

.. note:: First time steps of storage output could give NaN values during the initial converging phase.

**Key variable categories:**

- **Temperature Layers (5 layers each):**
  - Twall1-5: Wall temperatures from outer-most (1) to inner-most (5) layer
  - Troof1-5: Roof temperatures from outer-most (1) to inner-most (5) layer
  - Tground1-5: Ground temperatures from outer-most (1) to inner-most (5) layer
  - Tibld1-5: Internal building element temperatures

- **Indoor Temperature:**
  - Tabld: Indoor air temperature within buildings

- **Storage Heat Fluxes:**
  - QSnet: Net storage heat flux (sum of all components)
  - QSwall/QSroof/QSground: Component-specific storage fluxes
  - QSair: Storage heat flux into air
  - QSibld: Storage heat flux into internal building elements

.. note::
   **Surface Temperature Convention**: ESTM uses detailed layer-specific temperatures (Twall1-5, Troof1-5, etc.) rather than the bulk `Tsurf` variable found in main SUEWS output. The layer temperatures provide much more detailed thermal analysis of urban facets.

.. note::
   These detailed temperature profiles enable analysis of heat transfer through urban facets and are particularly valuable for:

   - Building energy assessment
   - Urban heat island analysis
   - Validation against thermal imaging data
   - Surface temperature pattern studies


SSss_YYYY_SPARTACUS_TT.txt
~~~~~~~~~~~~~~~~~~~~~~~~~~

**Content:** SPARTACUS radiation model outputs (experimental).

**Variables:** 194 variables including detailed radiation profiles and urban canyon radiative transfer.

**Documentation:** :doc:`variables/spartacus`

**Enabled when:** SPARTACUS model is activated in model configuration
