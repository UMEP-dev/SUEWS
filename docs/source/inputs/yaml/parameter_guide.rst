.. _parameter_guide:

Parameter Configuration Guide
==============================

This guide provides practical guidance for configuring SUEWS parameters, organized by common use cases and simulation scenarios.

.. contents:: Quick Navigation
   :local:
   :depth: 2

Essential Parameters for Basic Simulations
-------------------------------------------

These parameters must be configured for any SUEWS simulation:

**Time and Location Settings**

* ``model.control.tstep`` - Model timestep in seconds (typically 3600 for hourly, 1800 for 30-min, 300 for 5-min)
* ``model.control.start_date`` - Simulation start date (YYYY-MM-DD format)
* ``model.control.end_date`` - Simulation end date (YYYY-MM-DD format)
* ``site.latitude`` - Site latitude in decimal degrees (-90 to 90)
* ``site.longitude`` - Site longitude in decimal degrees (-180 to 180)
* ``site.timezone`` - Timezone offset from UTC (e.g., 0 for UTC, 1 for CET)

**Input/Output Files**

* ``model.control.forcing_file`` - Path to meteorological forcing data file
* ``model.control.output_file`` - Output configuration (can be simple path or detailed config)

**Land Cover Fractions**

The seven surface types must sum to 1.0:

* ``site.land_cover.paved`` - Paved surfaces (roads, parking lots)
* ``site.land_cover.bldgs`` - Buildings
* ``site.land_cover.evetr`` - Evergreen trees
* ``site.land_cover.dectr`` - Deciduous trees  
* ``site.land_cover.grass`` - Grass/lawn areas
* ``site.land_cover.bsoil`` - Bare soil
* ``site.land_cover.water`` - Water bodies

Physics Method Selection
-------------------------

Choose appropriate methods based on your data availability and simulation needs:

**Radiation Calculations**

``model.physics.net_radiation_method``:

* **3** (LDOWN_AIR) - **Recommended for most users**: Estimates incoming longwave from air temperature and humidity
* **1** (LDOWN_OBSERVED) - Use when you have measured incoming longwave radiation
* **0** (OBSERVED) - Use when you have complete measured radiation components

**Anthropogenic Heat Flux (QF)**

``model.physics.emissions_method``:

* **0** (NO_EMISSIONS) - Use measured QF values or set to zero to exclude
* **1** (L11) - Simple temperature-dependent method with day/night profiles
* **2** (J11) - Includes heating/cooling degree days (good for seasonal variation)
* **4** (J19) - Most comprehensive: includes building energy, metabolism, traffic

**Storage Heat Flux**

``model.physics.storage_heat_method``:

* **1** (OHM_WITHOUT_QF) - Standard Objective Hysteresis Model (most common)
* **2** (OHM_WITH_QF) - OHM including anthropogenic heat (set ``ohm_inc_qf: 1``)

Urban Morphology Parameters
----------------------------

Essential for accurate urban climate simulations:

**Building Characteristics**

* ``site.properties.bldgs.height`` - Mean building height (m)
* ``site.properties.bldgs.fai`` - Frontal area index (dimensionless, typically 0.1-0.6)
* ``site.properties.z0m_summer`` - Summer roughness length for momentum (m, typically 0.5-2.0)
* ``site.properties.z0m_winter`` - Winter roughness length for momentum (m)

**Population and Energy Use**

* ``site.properties.pop_dens_daytime`` - Daytime population density (people/ha)
* ``site.properties.pop_dens_nighttime`` - Nighttime population density (people/ha)

Vegetation Parameters
---------------------

Critical for evapotranspiration and energy balance:

**LAI (Leaf Area Index) Configuration**

For each vegetation type (evetr, dectr, grass):

* ``site.properties.[veg_type].lai_max`` - Maximum LAI (typically 4-6 for trees, 2-3 for grass)
* ``site.properties.[veg_type].lai_min`` - Minimum LAI (typically 1-4 for evergreen, 0-1 for deciduous/grass)

**Growing Season**

* ``site.properties.[veg_type].gdd_base`` - Base temperature for growing degree days (°C)
* ``site.properties.[veg_type].sdd_base`` - Base temperature for senescence degree days (°C)
* ``site.properties.[veg_type].lai_eq`` - LAI equation parameters

Surface Properties
------------------

**Albedo Values**

For each surface type:

* ``site.properties.[surface].albedo_min`` - Minimum albedo (winter/wet conditions)
* ``site.properties.[surface].albedo_max`` - Maximum albedo (summer/dry conditions)

Typical ranges:

* Paved: 0.08-0.20
* Buildings: 0.10-0.30
* Vegetation: 0.10-0.25
* Bare soil: 0.10-0.30
* Water: 0.03-0.10

**Surface Conductance**

Controls evaporation rates:

* ``site.properties.conductance.g_max`` - Maximum surface conductance (mm/s, typically 5-15)
* ``site.properties.conductance.g_min`` - Minimum surface conductance (mm/s, typically 0.1-1)
* ``site.properties.conductance.lai_max`` - LAI for maximum conductance

Initial Conditions
------------------

Set appropriate starting values for state variables:

**Soil Moisture**

For each surface type:

* ``site.initial_states.[surface].soil_moisture`` - Initial soil moisture (mm, typically 50-150)

**Surface Temperature**

* ``site.initial_states.[surface].surface_temp`` - Initial surface temperature (°C)

Advanced Options
----------------

**Water Balance**

* ``model.physics.water_use_method`` - Include irrigation/water use (0=off, 1=on)
* ``site.properties.irrigation`` - Irrigation parameters if water use enabled

**Snow Processes**

* ``model.physics.snow_use`` - Enable snow module (0=off, 1=on)
* ``site.properties.snow`` - Snow parameters if snow module enabled

**Atmospheric Stability**

* ``model.physics.stability_method`` - Stability corrections (3=Campbell & Norman recommended)

**Diagnostic Variables**

* ``model.physics.diagnose_method`` - How to handle T2, Q2, U10 (0=use forcing, 1=calculate)

Output Configuration
--------------------

Control what data is saved and how frequently:

**Basic Configuration**

.. code-block:: yaml

   output_file: "output.txt"  # Simple text output

**Advanced Configuration**

.. code-block:: yaml

   output_file:
     format: parquet         # Efficient binary format
     freq: 1800             # Output every 30 minutes
     groups: ["SUEWS", "RSL", "debug"]  # Output groups to include

Common Configuration Examples
-----------------------------

**Dense Urban Site**

.. code-block:: yaml

   land_cover:
     paved: 0.45
     bldgs: 0.40
     grass: 0.10
     evetr: 0.03
     dectr: 0.02
     bsoil: 0.00
     water: 0.00

**Suburban Site**

.. code-block:: yaml

   land_cover:
     paved: 0.25
     bldgs: 0.25
     grass: 0.30
     evetr: 0.10
     dectr: 0.10
     bsoil: 0.00
     water: 0.00

**Park/Green Space**

.. code-block:: yaml

   land_cover:
     paved: 0.10
     bldgs: 0.05
     grass: 0.50
     evetr: 0.15
     dectr: 0.15
     bsoil: 0.05
     water: 0.00

Troubleshooting Common Issues
-----------------------------

**Energy Balance Not Closing**

* Check albedo values are realistic for your surfaces
* Verify OHM coefficients are appropriate
* Ensure radiation method matches your data availability

**Unrealistic Surface Temperatures**

* Check initial surface temperatures
* Verify thermal properties (heat capacity, conductivity)
* Review storage heat method selection

**No Evapotranspiration**

* Check LAI values are non-zero for vegetation
* Verify surface conductance parameters
* Ensure soil moisture is adequate

**Model Crashes**

* Verify land cover fractions sum to 1.0
* Check all required parameters are present
* Ensure forcing data covers simulation period

See Also
--------

* :doc:`schema/model` - Complete model configuration reference
* :doc:`schema/site` - Complete site configuration reference  
* :doc:`parameter_index` - Alphabetical parameter index
* :ref:`yaml_input` - YAML format overview