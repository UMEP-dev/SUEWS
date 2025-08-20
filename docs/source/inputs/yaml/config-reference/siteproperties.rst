.. meta::
   :description: SUEWS YAML configuration for site properties parameters
   :keywords: SUEWS, YAML, siteproperties, parameters, configuration

.. _siteproperties:

.. index::
   single: SiteProperties (YAML parameter)
   single: YAML; SiteProperties

Site Properties
===============

Physical and geographical characteristics of the simulation site.

Defines the location, dimensions, surface characteristics, and environmental
parameters for a specific urban site being modelled in SUEWS.

**Parameters:**

.. index::
   single: lat (YAML parameter)
   single: SiteProperties; lat

.. option:: lat

   Latitude of the site in degrees

   :Unit: degrees
   :Sample value: ``51.5``

.. index::
   single: lng (YAML parameter)
   single: SiteProperties; lng

.. option:: lng

   Longitude of the site in degrees

   :Unit: degrees
   :Sample value: ``-0.13``

.. index::
   single: alt (YAML parameter)
   single: SiteProperties; alt

.. option:: alt

   Altitude of the site above sea level

   :Unit: m
   :Sample value: ``40.0``

.. index::
   single: timezone (YAML parameter)
   single: SiteProperties; timezone

.. option:: timezone

   Time zone offset from UTC

   :Unit: hours
   :Sample value: ``0.0`` (UTC)

.. index::
   single: surfacearea (YAML parameter)
   single: SiteProperties; surfacearea

.. option:: surfacearea

   Total surface area of the site

   :Unit: |m^2|
   :Sample value: ``10000.0``

.. index::
   single: z (YAML parameter)
   single: SiteProperties; z

.. option:: z

   Measurement height

   :Unit: m
   :Sample value: ``10.0``

.. index::
   single: z0m_in (YAML parameter)
   single: SiteProperties; z0m_in

.. option:: z0m_in

   Momentum roughness length

   :Unit: m
   :Sample value: ``1.0``

.. index::
   single: zdm_in (YAML parameter)
   single: SiteProperties; zdm_in

.. option:: zdm_in

   Zero-plane displacement height

   :Unit: m
   :Sample value: ``5.0``

.. index::
   single: pipecapacity (YAML parameter)
   single: SiteProperties; pipecapacity

.. option:: pipecapacity

   Maximum capacity of drainage pipes

   :Unit: mm |h^-1|
   :Sample value: ``100.0``

.. index::
   single: runofftowater (YAML parameter)
   single: SiteProperties; runofftowater

.. option:: runofftowater

   Fraction of excess water going to water bodies

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: narp_trans_site (YAML parameter)
   single: SiteProperties; narp_trans_site

.. option:: narp_trans_site

   Site-specific NARP transmission coefficient

   :Unit: dimensionless
   :Sample value: ``0.2``

.. index::
   single: lumps (YAML parameter)
   single: SiteProperties; lumps

.. option:: lumps

   Parameters for Local-scale Urban Meteorological Parameterization Scheme

   :Sample value: ``PydanticUndefined``

   The ``lumps`` parameter group is defined by the :doc:`lumpsparams` structure.

.. index::
   single: spartacus (YAML parameter)
   single: SiteProperties; spartacus

.. option:: spartacus

   Parameters for Solar Parametrizations for Radiative Transfer through Urban Canopy Scheme

   :Sample value: ``PydanticUndefined``

   The ``spartacus`` parameter group is defined by the :doc:`spartacusparams` structure.

.. index::
   single: stebbs (YAML parameter)
   single: SiteProperties; stebbs

.. option:: stebbs

   Parameters for the STEBBS building energy model

   :Sample value: ``PydanticUndefined``

   The ``stebbs`` parameter group is defined by the :doc:`stebbsproperties` structure.

.. index::
   single: building_archetype (YAML parameter)
   single: SiteProperties; building_archetype

.. option:: building_archetype

   Parameters for building archetypes

   :Sample value: ``PydanticUndefined``

   The ``building_archetype`` parameter group is defined by the :doc:`archetypeproperties` structure.

.. index::
   single: conductance (YAML parameter)
   single: SiteProperties; conductance

.. option:: conductance

   Parameters for surface conductance calculations

   :Sample value: ``PydanticUndefined``

   The ``conductance`` parameter group is defined by the :doc:`conductance` structure.

.. index::
   single: irrigation (YAML parameter)
   single: SiteProperties; irrigation

.. option:: irrigation

   Parameters for irrigation modelling

   :Sample value: ``PydanticUndefined``

   The ``irrigation`` parameter group is defined by the :doc:`irrigationparams` structure.

.. index::
   single: anthropogenic_emissions (YAML parameter)
   single: SiteProperties; anthropogenic_emissions

.. option:: anthropogenic_emissions

   Parameters for anthropogenic heat and water emissions

   :Sample value: ``PydanticUndefined``

   The ``anthropogenic_emissions`` parameter group is defined by the :doc:`anthropogenicemissions` structure.

.. index::
   single: snow (YAML parameter)
   single: SiteProperties; snow

.. option:: snow

   Parameters for snow modelling

   :Sample value: ``PydanticUndefined``

   The ``snow`` parameter group is defined by the :doc:`snowparams` structure.

.. index::
   single: land_cover (YAML parameter)
   single: SiteProperties; land_cover

.. option:: land_cover

   Parameters for land cover characteristics

   :Sample value: ``PydanticUndefined``

   The ``land_cover`` parameter group is defined by the :doc:`landcover` structure.

.. index::
   single: vertical_layers (YAML parameter)
   single: SiteProperties; vertical_layers

.. option:: vertical_layers

   Parameters for vertical layer structure

   :Sample value: ``PydanticUndefined``

   The ``vertical_layers`` parameter group is defined by the :doc:`verticallayers` structure.

.. index::
   single: n_buildings (YAML parameter)
   single: SiteProperties; n_buildings

.. option:: n_buildings

   Number of buildings in the site

   :Unit: dimensionless
   :Sample value: ``1``

.. index::
   single: h_std (YAML parameter)
   single: SiteProperties; h_std

.. option:: h_std

   Standard deviation of building heights in the site

   :Unit: m
   :Sample value: ``10.0``

.. index::
   single: lambda_c (YAML parameter)
   single: SiteProperties; lambda_c

.. option:: lambda_c

   External building surface area to plan area ratio

   :Unit: |m^2| |m^-2|
   :Sample value: ``0``

.. index::
   single: ref (YAML parameter)
   single: SiteProperties; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
