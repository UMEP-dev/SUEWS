.. _yaml_config_reference_tabsets:

YAML Configuration Reference - Tab Sets
========================================

This documentation follows the hierarchical structure of SUEWS YAML configuration files.

.. note::
   This version uses tab sets for major sections with cards for subsections.

.. tab-set::

   .. tab-item:: Model
      :sync: model
      
      .. card:: Model Control
         :link: modelcontrol
         :link-type: doc
         :class-header: bg-light
         :class-card: sd-rounded-3 shadow-sm
         
         Core simulation parameters
         ^^^
         
         **Core fields:** ``tstep``, ``forcing_file``, ``start_time``, ``end_time``, ``ref``
         
         .. button-link:: outputconfig
            :color: primary
            :outline:
            :align: center
            
            Output Configuration →
         
         +++
         Configure time stepping, input files, and simulation period
      
      .. card:: Physics Methods
         :link: modelphysics
         :link-type: doc
         :class-header: bg-light
         :class-card: sd-rounded-3 shadow-sm
         
         Physical process calculations
         ^^^
         
         **16 physics options** for detailed urban surface modelling:
         
         .. list-table::
            :widths: 50 50
            :class: table-sm
            
            * - ``netradiationmethod``
              - Net radiation calculation
            * - ``emissionsmethod``
              - Emissions calculation
            * - ``storageheatmethod``
              - Storage heat method
            * - ``ohmincqf``
              - Include QF in OHM
            * - ``roughlenmommethod``
              - Momentum roughness
            * - ``roughlenheatmethod``
              - Heat roughness
            * - ``stabilitymethod``
              - Stability calculation
            * - ``snowuse``
              - Snow processes
         
         +++
         Configure all physics calculation methods

   .. tab-item:: Sites
      :sync: sites
      
      .. card:: Site Properties
         :link: siteproperties
         :link-type: doc
         :class-header: bg-light
         :class-card: sd-rounded-3 shadow-sm
         
         Location and physical characteristics
         ^^^
         
         **Geographic:** ``lat``, ``lng``, ``alt``, ``timezone``
         
         **Physical:** ``surfacearea``, ``z``, ``z0``, ``zd``, ``fai``, ``pai``
         
         **Population:** ``popdens``, ``popdensdaytime``, ``popdensnighttime``
         
         **Major parameter groups:**
         
         .. grid:: 2
            :gutter: 2
            
            .. grid-item::
               
               - :doc:`LUMPS Parameters <lumpsparams>`
               - :doc:`SPARTACUS <spartacusparams>`
               - :doc:`STEBBS <stebbsproperties>`
               - :doc:`Archetype Properties <archetypeproperties>`
               - :doc:`Conductance <conductance>`
            
            .. grid-item::
               
               - :doc:`Irrigation <irrigationparams>`
               - :doc:`Anthropogenic Emissions <anthropogenicemissions>`
               - :doc:`Snow <snowparams>`
               - :doc:`Land Cover <landcover>` (7 types)
               - :doc:`Vertical Layers <verticallayers>`
      
      .. card:: Initial States
         :link: initialstates
         :link-type: doc
         :class-header: bg-light
         :class-card: sd-rounded-3 shadow-sm
         
         Surface initial conditions
         ^^^
         
         **Snow:** ``snowalb`` - Initial snow albedo
         
         **Surface states by type:**
         
         .. grid:: 3
            :gutter: 2
            
            .. grid-item::
               
               **Built surfaces:**
               
               - :doc:`Paved <initialstatepaved>`
               - :doc:`Buildings <initialstatebldgs>`
               - :doc:`Roofs <surfaceinitialstate>`
               - :doc:`Walls <surfaceinitialstate>`
            
            .. grid-item::
               
               **Vegetation:**
               
               - :doc:`Evergreen <initialstateevetr>`
               - :doc:`Deciduous <initialstatedectr>`
               - :doc:`Grass <initialstategrass>`
            
            .. grid-item::
               
               **Natural:**
               
               - :doc:`Bare Soil <initialstatebsoil>`
               - :doc:`Water <initialstatewater>`

   .. tab-item:: Quick Reference
      :sync: reference
      
      .. card:: Complete Field Reference
         :class-header: bg-info text-white
         :class-card: sd-rounded-3
         
         All configuration fields at a glance
         ^^^
         
         .. accordion::
            
            .. accordion-item:: Model Control Fields
               
               ============== ==================================================
               Field          Description
               ============== ==================================================
               ``tstep``      Time step in seconds
               ``forcing_file`` Meteorological forcing data file(s)
               ``start_time`` Simulation start time
               ``end_time``   Simulation end time
               ``output_file`` Output configuration (→ OutputConfig)
               ``ref``        Reference metadata
               ============== ==================================================
            
            .. accordion-item:: Physics Methods Fields
               
               ======================== =====================================
               Field                    Description
               ======================== =====================================
               ``netradiationmethod``   Net radiation calculation method
               ``emissionsmethod``      Emissions calculation method
               ``storageheatmethod``    Storage heat calculation method
               ``ohmincqf``            Include QF in OHM calculation
               ``roughlenmommethod``    Momentum roughness length method
               ``roughlenheatmethod``   Heat roughness length method
               ``stabilitymethod``      Atmospheric stability method
               ``snowuse``             Snow processes flag
               ``netwatermethod``       Net water calculation method
               ``smdmethod``           Soil moisture deficit method
               ``laimethod``           LAI calculation method
               ``laibasedon``          LAI basis
               ``diagnosticmethod``     Diagnostic output method
               ``waterusemethod``       Water use calculation method
               ``radlongcalcmethiod``   Long-wave radiation method
               ``ref``                 Reference metadata
               ======================== =====================================
            
            .. accordion-item:: Site Properties Core Fields
               
               ===================== =========================================
               Field                 Description
               ===================== =========================================
               ``lat``               Latitude (degrees)
               ``lng``               Longitude (degrees)
               ``alt``               Altitude (m)
               ``timezone``          Time zone
               ``surfacearea``       Total surface area (m²)
               ``popdens``           Population density (people/ha)
               ``popdensdaytime``    Daytime population density
               ``popdensnighttime``  Nighttime population density
               ``areawaterbody``     Water body area (m²)
               ``areawall``          Wall area (m²)
               ``z``                 Measurement height (m)
               ``z0``                Roughness length for momentum (m)
               ``zd``                Zero-plane displacement (m)
               ``fai``               Frontal area index
               ``pai``               Plan area index
               ===================== =========================================

.. toctree::
   :hidden:
   :maxdepth: 3

   index
   index-cards-dropdowns
   index-cards-badges
   index-accordion
   index-definition
   anthropogenicemissions
   anthropogenicheat
   archetypeproperties
   bldgsproperties
   bsoilproperties
   buildinglayer
   co2params
   conductance
   dayprofile
   dectrproperties
   evetrproperties
   grassproperties
   hourlyprofile
   initialstatebldgs
   initialstatebsoil
   initialstatedectr
   initialstateevetr
   initialstategrass
   initialstatepaved
   initialstateveg
   initialstatewater
   initialstates
   irrigationparams
   laiparams
   laipowercoefficients
   lumpsparams
   landcover
   model
   modelcontrol
   modelphysics
   nonvegetatedsurfaceproperties
   ohmcoefficients
   ohm_coefficient_season_wetness
   outputconfig
   pavedproperties
   rooflayer
   spartacusparams
   suewsconfig
   site
   siteproperties
   snowparams
   stebbsproperties
   storagedrainparams
   surfaceinitialstate
   surfaceproperties
   thermallayers
   vegetatedsurfaceproperties
   vegetationparams
   verticallayers
   walllayer
   waterdistribution
   waterproperties
   weeklyprofile