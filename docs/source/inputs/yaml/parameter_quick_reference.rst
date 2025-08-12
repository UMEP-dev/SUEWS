.. _parameter_quick_reference:

.. meta::
   :description: Quick reference guide for SUEWS YAML parameters organized by category
   :keywords: SUEWS parameters, YAML configuration, quick reference, parameter categories

Parameter Quick Reference
=========================

This page provides a quick reference to SUEWS YAML parameters organized by category. 
For detailed descriptions, units, and sample values, click on the category links below.

.. contents:: Parameter Categories
   :local:
   :depth: 1

Model Configuration
-------------------

**Model Control** (:doc:`schema/modelcontrol`)
   - ``model.control.tstep`` - Time step [s]
   - ``model.control.forcing_file`` - Path to meteorological forcing data
   - ``model.control.start_time`` - Simulation start time
   - ``model.control.end_time`` - Simulation end time
   - ``model.control.output_format`` - Output file format

**Model Physics** (:doc:`schema/modelphysics`)
   - ``model.physics.net_radiation_method`` - Net radiation calculation method
   - ``model.physics.storage_heat_method`` - Storage heat flux method
   - ``model.physics.surface_evaporation_method`` - Evaporation calculation method
   - ``model.physics.roughness_length_method`` - Roughness length calculation

**Output Configuration** (:doc:`schema/outputconfig`)
   - ``model.output.freq`` - Output frequency [min]
   - ``model.output.aggregate`` - Aggregation method
   - ``model.output.write_timestep`` - Timestep output flag

Site Properties
---------------

**Basic Site Information** (:doc:`schema/siteproperties`)
   - ``site.properties.lat`` - Latitude [degrees]
   - ``site.properties.lng`` - Longitude [degrees]
   - ``site.properties.alt`` - Altitude [m]
   - ``site.properties.timezone`` - Time zone offset [hours]
   - ``site.properties.surfacearea`` - Total surface area [m²]
   - ``site.properties.z`` - Measurement height [m]

**Land Cover** (:doc:`schema/landcover`)
   - ``site.properties.land_cover.paved`` - Paved surface fraction
   - ``site.properties.land_cover.bldgs`` - Buildings fraction
   - ``site.properties.land_cover.evetr`` - Evergreen trees fraction
   - ``site.properties.land_cover.dectr`` - Deciduous trees fraction
   - ``site.properties.land_cover.grass`` - Grass fraction
   - ``site.properties.land_cover.bsoil`` - Bare soil fraction
   - ``site.properties.land_cover.water`` - Water fraction

Surface Properties
------------------

**Buildings** (:doc:`schema/bldgsproperties`)
   - ``site.properties.land_cover.bldgs_properties.albedo`` - Building albedo
   - ``site.properties.land_cover.bldgs_properties.emissivity`` - Building emissivity
   - ``site.properties.land_cover.bldgs_properties.ohm`` - OHM coefficients

**Paved Surfaces** (:doc:`schema/pavedproperties`)
   - ``site.properties.land_cover.paved_properties.albedo`` - Paved albedo
   - ``site.properties.land_cover.paved_properties.emissivity`` - Paved emissivity
   - ``site.properties.land_cover.paved_properties.storage_capacity`` - Storage capacity

**Vegetation** 
   - **Evergreen Trees** (:doc:`schema/evetrproperties`)
   - **Deciduous Trees** (:doc:`schema/dectrproperties`)
   - **Grass** (:doc:`schema/grassproperties`)
   - **Bare Soil** (:doc:`schema/bsoilproperties`)

Initial Conditions
------------------

**Initial States** (:doc:`schema/initialstates`)
   - Surface temperatures
   - Soil moisture stores
   - Snow water equivalent
   - Leaf area index

For detailed initial state parameters by surface type:
   - :doc:`schema/initialstatepaved`
   - :doc:`schema/initialstatebldgs`
   - :doc:`schema/initialstateevetr`
   - :doc:`schema/initialstatedectr`
   - :doc:`schema/initialstategrass`
   - :doc:`schema/initialstatebsoil`
   - :doc:`schema/initialstatewater`

Special Models
--------------

**SPARTACUS** (:doc:`schema/spartacusparams`)
   Solar radiation through urban canopy

**LUMPS** (:doc:`schema/lumpsparams`)
   Local-scale Urban Meteorological Parameterization

**STEBBS** (:doc:`schema/stebbsproperties`)
   Building energy model

**Anthropogenic Emissions** (:doc:`schema/anthropogenicemissions`)
   - **Heat** (:doc:`schema/anthropogenicheat`)
   - **CO₂** (:doc:`schema/co2params`)

**Irrigation** (:doc:`schema/irrigationparams`)
   Irrigation scheduling and water use

**Snow** (:doc:`schema/snowparams`)
   Snow accumulation and melt

Parameter Search Tips
---------------------

To find a specific parameter:

1. **Use browser search** (Ctrl+F / Cmd+F) on this page for a quick overview
2. **Click category links** for detailed parameter descriptions
3. **Use the search box** to search across all documentation

.. note::

   Parameters shown with dot notation (e.g., ``site.properties.lat``) represent the 
   hierarchical structure in your YAML configuration file.

.. tip::

   For a complete example configuration, see the 
   `sample configuration file <https://github.com/UMEP-dev/SUEWS/blob/master/src/supy/sample_run/sample_config.yml>`_