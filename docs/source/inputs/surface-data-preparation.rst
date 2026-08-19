.. _surface_data_preparation:

======================================
Preparing Surface Characteristics Data
======================================

This guide explains how to move from a real study area to site-specific SUEWS
(Surface Urban Energy and Water Balance Scheme) surface parameters. It
complements the :doc:`/inputs/yaml/index` guide, which explains configuration
structure and validation.

Use this page when you know *which* parameters are needed, but need practical
advice on where to obtain data and how to derive values.

Recommended Workflow
====================

1. Delineate the site footprint used for the SUEWS grid.
2. Compile compatible land-cover, building, and vegetation datasets.
3. Derive surface fractions and morphology from the same footprint.
4. Derive seasonal biophysical parameters (albedo, LAI, phenology).
5. Fill the YAML configuration and run ``suews validate --format json config.yml``.
6. Compare against local observations and refine sensitive parameters.

For broader setup context, see :doc:`/workflow`.

Land Cover Fractions
====================

Land-cover meaning
------------------
Fraction of grid area occupied by each of the seven SUEWS surface types.

Land-cover YAML paths
---------------------

Set one fraction for each surface type:

- ``sites.<site>.properties.land_cover.paved.sfr``
- ``sites.<site>.properties.land_cover.bldgs.sfr``
- ``sites.<site>.properties.land_cover.evetr.sfr``
- ``sites.<site>.properties.land_cover.dectr.sfr``
- ``sites.<site>.properties.land_cover.grass.sfr``
- ``sites.<site>.properties.land_cover.bsoil.sfr``
- ``sites.<site>.properties.land_cover.water.sfr``

Fractions must sum to 1.0. Re-run the validator after normalising them.

Land-cover sources
------------------

- `ESA WorldCover`_ (global land-cover maps at 10 m resolution)
- `Copernicus Global Dynamic Land Cover`_ (global land-cover maps and fraction
  layers)

Land-cover derivation
---------------------

1. Reproject all datasets to a metric CRS.
2. Clip to the SUEWS site footprint.
3. Map source classes to the seven SUEWS surface types.
4. Compute area fractions by class and normalise to 1.0.
5. Check consistency with local imagery.

Land-cover parameter reference
------------------------------

See :doc:`/inputs/yaml/config-reference/index` for full parameter definitions.

Albedo
======

Albedo meaning
--------------
Shortwave reflectance controlling net radiation partitioning at the surface.

Albedo YAML paths
-----------------

- Non-vegetated surfaces (single albedo):

  - ``sites.<site>.properties.land_cover.paved.alb``
  - ``sites.<site>.properties.land_cover.bldgs.alb``
  - ``sites.<site>.properties.land_cover.bsoil.alb``
  - ``sites.<site>.properties.land_cover.water.alb``

- Vegetated surfaces (seasonal range):

  - ``sites.<site>.properties.land_cover.evetr.alb_min`` and ``alb_max``
  - ``sites.<site>.properties.land_cover.dectr.alb_min`` and ``alb_max``
  - ``sites.<site>.properties.land_cover.grass.alb_min`` and ``alb_max``

Albedo sources
--------------

- Site-specific radiometer measurements
- The `SUEWS input database`_ for curated values by surface and urban typology

Albedo derivation
-----------------

1. Aggregate local radiometer measurements over representative periods, where
   available.
2. Otherwise select the closest matching surface or urban typology from the
   SUEWS input database and record that assumption.
3. Use a single representative ``alb`` for non-vegetated surfaces.
4. Use seasonal low/high values for ``alb_min`` and ``alb_max`` on vegetation.

Albedo parameter reference
--------------------------

See :doc:`/inputs/yaml/config-reference/index`.

Urban Morphology
================

Morphology meaning
------------------
Building and vegetation structure controlling roughness, turbulence, and radiation.

Morphology YAML paths
---------------------

- ``sites.<site>.properties.land_cover.bldgs.bldgh``
- ``sites.<site>.properties.land_cover.bldgs.faibldg``
- ``sites.<site>.properties.land_cover.evetr.height_evergreen_tree``
- ``sites.<site>.properties.land_cover.dectr.height_deciduous_tree``

Morphology sources
------------------

- `GLAMOUR`_ for global building height and plan-area fraction at 100 m
  resolution
- `GEDI canopy height`_ for a global 30 m vegetation-height product
- Site-specific building and vegetation surveys where available

Morphology derivation
---------------------

1. Derive mean building height and plan-area fraction for the site footprint.
2. Estimate frontal area index from compatible building geometry and the wind
   directions represented by the model. Do not infer it from plan-area
   fraction alone.
3. Derive representative tree heights from canopy products or field surveys.
4. Check GEDI-derived heights against local information in dense urban areas,
   where buildings can contaminate the canopy-height signal.

Morphology parameter reference
------------------------------

See :doc:`/inputs/yaml/config-reference/index`.

Leaf Area Index (LAI) and Phenology
===================================

LAI and phenology meaning
-------------------------
Seasonal vegetation state controlling transpiration, interception, and radiation.

LAI and phenology YAML paths
----------------------------

For each vegetated surface (``evetr``, ``dectr``, ``grass``):

- ``sites.<site>.properties.land_cover.<surface>.lai.lai_min``
- ``sites.<site>.properties.land_cover.<surface>.lai.lai_max``
- ``sites.<site>.properties.land_cover.<surface>.lai.gdd_full``
- ``sites.<site>.properties.land_cover.<surface>.lai.sdd_full``

LAI and phenology sources
-------------------------

- Site-specific LAI and phenology measurements
- The `SUEWS input database`_ for curated values by vegetation typology

LAI and phenology derivation
----------------------------

1. Compile multi-year seasonal LAI trajectories for the footprint.
2. Set ``lai_min`` from dormant-season values and ``lai_max`` from peak values.
3. Estimate phenology thresholds (``gdd_full``, ``sdd_full``) from local climate
   and observed green-up/senescence timing.
4. Check that values are physically consistent for local vegetation types.

LAI and phenology parameter reference
-------------------------------------

See :doc:`/inputs/yaml/config-reference/index`.

Storage-heat (OHM) Coefficients
===============================

OHM-coefficient meaning
-----------------------
Coefficients ``a1``, ``a2``, and ``a3`` for storage heat flux parameterisation.

OHM-coefficient YAML paths
--------------------------

For each surface, coefficients are set by season and wetness state, for example:

- ``sites.<site>.properties.land_cover.<surface>.ohm_coef.summer_wet.a1``
- ``sites.<site>.properties.land_cover.<surface>.ohm_coef.summer_wet.a2``
- ``sites.<site>.properties.land_cover.<surface>.ohm_coef.summer_wet.a3``

The same pattern applies for ``summer_dry``, ``winter_wet``, and ``winter_dry``.

OHM-coefficient sources
-----------------------

- Site-specific flux and radiation measurements (if available)
- Published coefficient sets in the SUEWS documentation/literature

OHM-coefficient derivation
--------------------------

If you have suitable observations, derive coefficients using
:func:`~supy.util.derive_ohm_coef` as shown in
:doc:`/inputs/tables/SUEWS_SiteInfo/SUEWS_OHMCoefficients`.

If you do not have local flux data, start from literature/default sets and
prioritise sensitivity testing before introducing custom coefficients.

OHM-coefficient parameter reference
-----------------------------------

See :doc:`/inputs/yaml/config-reference/index` and
:doc:`/inputs/tables/SUEWS_SiteInfo/SUEWS_OHMCoefficients`.

Surface-conductance Parameters
==============================

Surface-conductance meaning
---------------------------
Parameters controlling potential and realised stomatal/surface conductance.

Surface-conductance YAML paths
------------------------------

- Site-level conductance parameter:

  - ``sites.<site>.properties.conductance.g_max``

- Vegetation surface conductance limits:

  - ``sites.<site>.properties.land_cover.evetr.max_conductance``
  - ``sites.<site>.properties.land_cover.dectr.max_conductance``
  - ``sites.<site>.properties.land_cover.grass.max_conductance``

Surface-conductance sources
---------------------------

- Eddy covariance inversions
- Leaf- or canopy-level gas exchange observations
- The `SUEWS input database`_ for curated values by vegetation typology

Surface-conductance derivation
------------------------------

Direct local estimation is data-intensive. In most applications, begin with
published values for similar sites, then calibrate within physically realistic
ranges against local fluxes where available.

Surface-conductance parameter reference
---------------------------------------

See :doc:`/inputs/yaml/config-reference/index`.

SUEWS Input Database
====================

A dedicated repository provides the evolving input database used by the SUEWS
Database Manager and SUEWS Database Prepare plugins:

- `SUEWS input database`_

Use it as a starting point where relevant, but still verify representativeness
for your site and period.

References and Tools
====================

Use these project-supported resources when preparing surface data:

- `ESA WorldCover`_ and `Copernicus Global Dynamic Land Cover`_ for land cover
- `GLAMOUR`_ for building morphology
- `GEDI canopy height`_ for vegetation height
- The `SUEWS input database`_ for parameter values and typologies

Other datasets and manual GIS routes may be useful, but confirm their suitability
with the SUEWS team before relying on them.

.. _ESA WorldCover: https://esa-worldcover.org/en/data-access
.. _Copernicus Global Dynamic Land Cover: https://land.copernicus.eu/en/products/global-dynamic-land-cover
.. _GLAMOUR: https://zenodo.org/records/10396451
.. _GEDI canopy height: https://glad.umd.edu/dataset/gedi/
.. _SUEWS input database: https://github.com/UMEP-dev/SUEWS-database
