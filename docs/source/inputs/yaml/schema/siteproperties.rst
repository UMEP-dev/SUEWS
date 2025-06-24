Siteproperties
==============

**Parameters:**

.. option:: lat <RefValue[float]>

   Latitude of the site in degrees

   :Unit: degrees
   :Default: ``51.5``

.. option:: lng <RefValue[float]>

   Longitude of the site in degrees

   :Unit: degrees
   :Default: ``-0.13``

.. option:: alt <RefValue[float]>

   Altitude of the site above sea level

   :Unit: m
   :Default: ``40.0``

.. option:: timezone <RefValue[int]>

   Time zone offset from UTC

   :Unit: hour
   :Default: ``0``

.. option:: surfacearea <RefValue[float]>

   Total surface area of the site

   :Unit: m^2
   :Default: ``10000.0``

.. option:: z <RefValue[float]>

   Measurement height

   :Unit: m
   :Default: ``10.0``

.. option:: z0m_in <RefValue[float]>

   Momentum roughness length

   :Unit: m
   :Default: ``1.0``

.. option:: zdm_in <RefValue[float]>

   Zero-plane displacement height

   :Unit: m
   :Default: ``5.0``

.. option:: pipecapacity <RefValue[float]>

   Maximum capacity of drainage pipes

   :Unit: mm h^-1
   :Default: ``100.0``

.. option:: runofftowater <RefValue[float]>

   Fraction of excess water going to water bodies

   :Unit: dimensionless
   :Default: ``0.0``

.. option:: narp_trans_site <RefValue[float]>

   Site-specific NARP transmission coefficient

   :Unit: dimensionless
   :Default: ``0.2``

.. option:: lumps <LUMPSParams>

   Parameters for Local-scale Urban Meteorological Parameterization Scheme

   :Default: ``PydanticUndefined``

   The ``lumps`` parameter group is defined by the :doc:`lumpsparams` structure.

.. option:: spartacus <SPARTACUSParams>

   Parameters for Solar Parametrizations for Radiative Transfer through Urban Canopy Scheme

   :Default: ``PydanticUndefined``

   The ``spartacus`` parameter group is defined by the :doc:`spartacusparams` structure.

.. option:: stebbs <StebbsProperties>

   Parameters for the STEBBS building energy model

   :Default: ``PydanticUndefined``

   The ``stebbs`` parameter group is defined by the :doc:`stebbsproperties` structure.

.. option:: building_archetype <ArchetypeProperties>

   Parameters for building archetypes

   :Default: ``PydanticUndefined``

   The ``building_archetype`` parameter group is defined by the :doc:`archetypeproperties` structure.

.. option:: conductance <Conductance>

   Parameters for surface conductance calculations

   :Default: ``PydanticUndefined``

   The ``conductance`` parameter group is defined by the :doc:`conductance` structure.

.. option:: irrigation <IrrigationParams>

   Parameters for irrigation modelling

   :Default: ``PydanticUndefined``

   The ``irrigation`` parameter group is defined by the :doc:`irrigationparams` structure.

.. option:: anthropogenic_emissions <AnthropogenicEmissions>

   Parameters for anthropogenic heat and water emissions

   :Default: ``PydanticUndefined``

   The ``anthropogenic_emissions`` parameter group is defined by the :doc:`anthropogenicemissions` structure.

.. option:: snow <SnowParams>

   Parameters for snow modelling

   :Default: ``PydanticUndefined``

   The ``snow`` parameter group is defined by the :doc:`snowparams` structure.

.. option:: land_cover <LandCover>

   Parameters for land cover characteristics

   :Default: ``PydanticUndefined``

   The ``land_cover`` parameter group is defined by the :doc:`landcover` structure.

.. option:: vertical_layers <VerticalLayers>

   Parameters for vertical layer structure

   :Default: ``PydanticUndefined``

   The ``vertical_layers`` parameter group is defined by the :doc:`verticallayers` structure.

.. option:: n_buildings <RefValue[int]>

   Number of buildings in the site

   :Unit: dimensionless
   :Default: ``1``

.. option:: h_std <RefValue[float]>

   Standard deviation of building heights in the site

   :Unit: m
   :Default: ``10.0``

.. option:: lambda_c <RefValue[float]>

   External building surface area to plan area ratio

   :Unit: m^2 m^-2
   :Default: ``0``

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
