.. _surface_data_preparation:

Preparing Surface Characteristics Data
======================================

This guide explains how to move from a real study area to site-specific SUEWS
surface parameters. It complements the :doc:`/inputs/yaml/index` guide, which
explains configuration structure and validation.

Use this page when you know *which* parameters are needed, but need practical
advice on where to obtain data and how to derive values.

Recommended Workflow
--------------------

1. Delineate the site footprint used for the SUEWS grid.
2. Compile geospatial layers (land cover, elevation, buildings, vegetation).
3. Derive surface fractions and morphology from the same footprint.
4. Derive seasonal biophysical parameters (albedo, LAI, phenology).
5. Fill the YAML configuration and run ``suews-validate``.
6. Compare against local observations and refine sensitive parameters.

For broader setup context, see :doc:`/workflow`.

Land Cover Fractions
--------------------

Physical meaning
~~~~~~~~~~~~~~~~
Fraction of grid area occupied by each of the seven SUEWS surface types.

YAML configuration path
~~~~~~~~~~~~~~~~~~~~~~~

Set one fraction for each surface type:

- ``sites.<site>.properties.land_cover.paved.sfr``
- ``sites.<site>.properties.land_cover.bldgs.sfr``
- ``sites.<site>.properties.land_cover.evetr.sfr``
- ``sites.<site>.properties.land_cover.dectr.sfr``
- ``sites.<site>.properties.land_cover.grass.sfr``
- ``sites.<site>.properties.land_cover.bsoil.sfr``
- ``sites.<site>.properties.land_cover.water.sfr``

Fractions should sum to 1.0 (the validator can correct small rounding errors).

Data sources
~~~~~~~~~~~~

- `UMEP`_ (QGIS tools for land cover fractions from raster/vector inputs)
- `CORINE Land Cover`_ (Europe)
- `NLCD`_ (United States)
- `OpenStreetMap`_ (global vector features)

Derivation method
~~~~~~~~~~~~~~~~~

1. Reproject all datasets to a metric CRS.
2. Clip to the SUEWS site footprint.
3. Map source classes to the seven SUEWS surface types.
4. Compute area fractions by class and normalise to 1.0.
5. Check consistency with local imagery.

Parameter reference
~~~~~~~~~~~~~~~~~~~

See :doc:`/inputs/yaml/config-reference/index` for full parameter definitions.

Albedo
------

Physical meaning
~~~~~~~~~~~~~~~~
Shortwave reflectance controlling net radiation partitioning at the surface.

YAML configuration path
~~~~~~~~~~~~~~~~~~~~~~~

- Non-vegetated surfaces (single albedo):

  - ``sites.<site>.properties.land_cover.paved.alb``
  - ``sites.<site>.properties.land_cover.bldgs.alb``
  - ``sites.<site>.properties.land_cover.bsoil.alb``
  - ``sites.<site>.properties.land_cover.water.alb``

- Vegetated surfaces (seasonal range):

  - ``sites.<site>.properties.land_cover.evetr.alb_min`` and ``alb_max``
  - ``sites.<site>.properties.land_cover.dectr.alb_min`` and ``alb_max``
  - ``sites.<site>.properties.land_cover.grass.alb_min`` and ``alb_max``

Data sources
~~~~~~~~~~~~

- `MODIS MCD43A3`_ (broadband albedo)
- `Landsat Collection 2 Surface Reflectance`_
- Field measurements (radiometers)

Derivation method
~~~~~~~~~~~~~~~~~

1. Extract albedo for the study footprint and quality-filter cloud/snow pixels.
2. Compute representative statistics for the simulation period.
3. Use a single representative ``alb`` for non-vegetated surfaces.
4. Use seasonal low/high values for ``alb_min`` and ``alb_max`` on vegetation.

Parameter reference
~~~~~~~~~~~~~~~~~~~

See :doc:`/inputs/yaml/config-reference/index`.

Urban Morphology
----------------

Physical meaning
~~~~~~~~~~~~~~~~
Building and vegetation structure controlling roughness, turbulence, and radiation.

YAML configuration path
~~~~~~~~~~~~~~~~~~~~~~~

- ``sites.<site>.properties.land_cover.bldgs.bldgh``
- ``sites.<site>.properties.land_cover.bldgs.faibldg``
- ``sites.<site>.properties.land_cover.evetr.evetreeh``
- ``sites.<site>.properties.land_cover.dectr.dectreeh``

Data sources
~~~~~~~~~~~~

- Airborne or national `LiDAR`_ products
- `OpenStreetMap`_ building footprints/attributes
- `UMEP`_ Morphometric Calculator tools
- `Global Human Settlement Layer (GHSL)`_ products

Derivation method
~~~~~~~~~~~~~~~~~

1. Build a DSM/DTM or equivalent elevation model.
2. Derive mean building height within the site.
3. Estimate frontal area index from geometry and wind-direction context.
4. Derive representative tree heights from canopy products or field surveys.

Parameter reference
~~~~~~~~~~~~~~~~~~~

See :doc:`/inputs/yaml/config-reference/index`.

Leaf Area Index (LAI) and Phenology
-----------------------------------

Physical meaning
~~~~~~~~~~~~~~~~
Seasonal vegetation state controlling transpiration, interception, and radiation.

YAML configuration path
~~~~~~~~~~~~~~~~~~~~~~~

For each vegetated surface (``evetr``, ``dectr``, ``grass``):

- ``sites.<site>.properties.land_cover.<surface>.lai.laimin``
- ``sites.<site>.properties.land_cover.<surface>.lai.laimax``
- ``sites.<site>.properties.land_cover.<surface>.lai.gddfull``
- ``sites.<site>.properties.land_cover.<surface>.lai.sddfull``

Data sources
~~~~~~~~~~~~

- `MODIS LAI`_ products
- `Copernicus LAI`_ products
- Field LAI measurements (e.g., LAI-2200, hemispherical photography)

Derivation method
~~~~~~~~~~~~~~~~~

1. Extract multi-year seasonal LAI trajectories for the footprint.
2. Set ``laimin`` from dormant-season values and ``laimax`` from peak values.
3. Estimate phenology thresholds (``gddfull``, ``sddfull``) from local climate
   and observed green-up/senescence timing.
4. Check that values are physically consistent for local vegetation types.

Parameter reference
~~~~~~~~~~~~~~~~~~~

See :doc:`/inputs/yaml/config-reference/index`.

OHM Coefficients
----------------

Physical meaning
~~~~~~~~~~~~~~~~
Coefficients ``a1``, ``a2``, and ``a3`` for storage heat flux parameterisation.

YAML configuration path
~~~~~~~~~~~~~~~~~~~~~~~

For each surface, coefficients are set by season and wetness state, for example:

- ``sites.<site>.properties.land_cover.<surface>.ohm_coef.summer_wet.a1``
- ``sites.<site>.properties.land_cover.<surface>.ohm_coef.summer_wet.a2``
- ``sites.<site>.properties.land_cover.<surface>.ohm_coef.summer_wet.a3``

The same pattern applies for ``summer_dry``, ``winter_wet``, and ``winter_dry``.

Data sources
~~~~~~~~~~~~

- Site-specific flux and radiation measurements (if available)
- Published coefficient sets in the SUEWS documentation/literature

Derivation method
~~~~~~~~~~~~~~~~~

If you have suitable observations, derive coefficients using
``supy.util.derive_ohm_coef()`` as shown in
:doc:`/inputs/tables/SUEWS_SiteInfo/SUEWS_OHMCoefficients`.

If you do not have local flux data, start from literature/default sets and
prioritise sensitivity testing before introducing custom coefficients.

Parameter reference
~~~~~~~~~~~~~~~~~~~

See :doc:`/inputs/yaml/config-reference/index` and
:doc:`/inputs/tables/SUEWS_SiteInfo/SUEWS_OHMCoefficients`.

Surface Conductance
-------------------

Physical meaning
~~~~~~~~~~~~~~~~
Parameters controlling potential and realised stomatal/surface conductance.

YAML configuration path
~~~~~~~~~~~~~~~~~~~~~~~

- Site-level conductance parameter:

  - ``sites.<site>.properties.conductance.g_max``

- Vegetation surface conductance limits:

  - ``sites.<site>.properties.land_cover.evetr.maxconductance``
  - ``sites.<site>.properties.land_cover.dectr.maxconductance``
  - ``sites.<site>.properties.land_cover.grass.maxconductance``

Data sources
~~~~~~~~~~~~

- Eddy covariance inversions
- Leaf- or canopy-level gas exchange observations
- Published parameter sets for similar vegetation and climate regimes

Derivation method
~~~~~~~~~~~~~~~~~

Direct local estimation is data-intensive. In most applications, begin with
published values for similar sites, then calibrate within physically realistic
ranges against local fluxes where available.

Parameter reference
~~~~~~~~~~~~~~~~~~~

See :doc:`/inputs/yaml/config-reference/index`.

SUEWS-database (Under Development)
----------------------------------

A dedicated repository for curated SUEWS surface parameter datasets is under
active development:

- `UMEP-dev/SUEWS-database`_

Use it as a starting point where relevant, but still verify representativeness
for your site and period.

References and Tools
--------------------

The following resources are commonly used when preparing SUEWS surface data:

- `UMEP`_ (QGIS pre-processing tools)
- `CORINE Land Cover`_
- `NLCD`_
- `OpenStreetMap`_
- `MODIS MCD43A3`_ (albedo)
- `MODIS LAI`_ and `Copernicus LAI`_
- `Landsat Collection 2 Surface Reflectance`_
- `Global Human Settlement Layer (GHSL)`_
- `UMEP-dev/SUEWS-database`_

.. _UMEP: https://umep-docs.readthedocs.io/en/latest/
.. _CORINE Land Cover: https://land.copernicus.eu/en/products/corine-land-cover
.. _NLCD: https://www.usgs.gov/centers/eros/science/national-land-cover-database
.. _OpenStreetMap: https://www.openstreetmap.org/
.. _MODIS MCD43A3: https://lpdaac.usgs.gov/products/mcd43a3v061/
.. _MODIS LAI: https://lpdaac.usgs.gov/products/mcd15a3hv061/
.. _Copernicus LAI: https://land.copernicus.eu/global/products/lai
.. _Landsat Collection 2 Surface Reflectance: https://www.usgs.gov/landsat-missions/landsat-collection-2-surface-reflectance
.. _LiDAR: https://www.usgs.gov/programs/3d-elevation-program
.. _Global Human Settlement Layer (GHSL): https://ghsl.jrc.ec.europa.eu/
.. _UMEP-dev/SUEWS-database: https://github.com/UMEP-dev/SUEWS-database
