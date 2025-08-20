.. _yaml_config_reference_accordion:

YAML Configuration Reference - Expandable Sections
===================================================

This documentation follows the hierarchical structure of SUEWS YAML configuration files.

.. note::
   This version uses single-level expandable sections with comprehensive field listings.

.. dropdown:: Model Configuration
   :open:
   :color: primary
   :icon: gear
   :animate: fade-in-slide-down
   
   Complete model setup including control parameters and physics methods.
   
   → :doc:`Model Configuration Documentation <model>`
   
   .. grid:: 1 1 2 2
      :gutter: 3
      :padding: 2
      
      .. grid-item::
         
         .. card:: Model Control
            :link: modelcontrol
            :link-type: doc
            :class-card: sd-rounded-2
            
            **Core Parameters**
            ^^^
            
            - ``tstep`` - Time step (seconds)
            - ``forcing_file`` - Input data file(s)
            - ``start_time`` - Simulation start
            - ``end_time`` - Simulation end
            - ``ref`` - Reference metadata
            
            **Nested Configuration:**
            
            ``output_file`` → :doc:`outputconfig`
      
      .. grid-item::
         
         .. card:: Physics Methods
            :link: modelphysics
            :link-type: doc
            :class-card: sd-rounded-2
            
            **16 Calculation Methods**
            ^^^
            
            **Radiation & Energy:**
            
            - ``netradiationmethod``
            - ``radlongcalcmethiod``
            - ``emissionsmethod``
            - ``storageheatmethod``
            - ``ohmincqf``
            
            **Surface Properties:**
            
            - ``roughlenmommethod``
            - ``roughlenheatmethod``
            - ``stabilitymethod``
            
            **Water & Vegetation:**
            
            - ``snowuse``
            - ``netwatermethod``
            - ``smdmethod``
            - ``laimethod``
            - ``laibasedon``
            - ``waterusemethod``
            
            **Other:**
            
            - ``diagnosticmethod``
            - ``ref``

.. dropdown:: Site Configuration
   :open:
   :color: success
   :icon: location
   :animate: fade-in-slide-down
   
   Site-specific parameters and properties for your urban area.
   
   → :doc:`Site Configuration Documentation <site>`
   
   .. card:: Site Properties
      :link: siteproperties
      :link-type: doc
      :class-header: bg-light
      :class-card: sd-rounded-2 shadow-sm
      
      **Core Fields** (15 parameters)
      ^^^
      
      .. grid:: 1 1 2 3
         :gutter: 2
         
         .. grid-item::
            
            **Geographic:**
            
            - ``lat`` - Latitude
            - ``lng`` - Longitude
            - ``alt`` - Altitude
            - ``timezone`` - Time zone
            - ``surfacearea`` - Area
         
         .. grid-item::
            
            **Urban Form:**
            
            - ``z`` - Measurement height
            - ``z0`` - Roughness length
            - ``zd`` - Displacement
            - ``fai`` - Frontal area index
            - ``pai`` - Plan area index
         
         .. grid-item::
            
            **Demographics:**
            
            - ``popdens`` - Population
            - ``popdensdaytime`` - Day pop.
            - ``popdensnighttime`` - Night pop.
            - ``areawaterbody`` - Water area
            - ``areawall`` - Wall area
      
      +++
      
      **Major Parameter Groups:**
      
      .. list-table::
         :widths: 30 70
         :class: table-sm
         
         * - :doc:`LUMPS <lumpsparams>`
           - LUMPS model parameters
         * - :doc:`SPARTACUS <spartacusparams>`
           - SPARTACUS radiation model
         * - :doc:`STEBBS <stebbsproperties>`
           - STEBBS parameters
         * - :doc:`Archetype <archetypeproperties>`
           - Building archetype properties
         * - :doc:`Conductance <conductance>`
           - Surface conductance parameters
         * - :doc:`Irrigation <irrigationparams>`
           - Irrigation settings
         * - :doc:`Emissions <anthropogenicemissions>`
           - Anthropogenic emissions (2 subsections)
         * - :doc:`Snow <snowparams>`
           - Snow parameters
         * - :doc:`Land Cover <landcover>`
           - 7 surface types configuration
         * - :doc:`Vertical Layers <verticallayers>`
           - Building layers (2 subsections)

.. dropdown:: Initial States
   :open:
   :color: info
   :icon: play
   :animate: fade-in-slide-down
   
   Initial conditions for all surface types at simulation start.
   
   → :doc:`Initial States Documentation <initialstates>`
   
   **Snow Parameter:** ``snowalb`` - Initial snow albedo
   
   .. grid:: 1 1 2 3
      :gutter: 2
      :padding: 2
      
      .. grid-item::
         
         .. card:: Built Surfaces
            :class-card: sd-rounded-2
            
            - :doc:`Paved <initialstatepaved>`
            - :doc:`Buildings <initialstatebldgs>`
            - :doc:`Roofs <surfaceinitialstate>`
            - :doc:`Walls <surfaceinitialstate>`
      
      .. grid-item::
         
         .. card:: Vegetation
            :class-card: sd-rounded-2
            
            - :doc:`Evergreen <initialstateevetr>`
            - :doc:`Deciduous <initialstatedectr>`
            - :doc:`Grass <initialstategrass>`
            
            *Contains LAI fields*
      
      .. grid-item::
         
         .. card:: Natural Surfaces
            :class-card: sd-rounded-2
            
            - :doc:`Bare Soil <initialstatebsoil>`
            - :doc:`Water <initialstatewater>`

.. dropdown:: Complete Field Index
   :color: warning
   :icon: list-unordered
   :animate: fade-in-slide-down
   
   Alphabetical index of all configuration fields with their locations.
   
   .. tab-set::
      
      .. tab-item:: A-E
         
         ======================= ====================================
         Field                   Location
         ======================= ====================================
         ``alt``                 Site Properties
         ``areawaterbody``       Site Properties
         ``areawall``            Site Properties
         ``diagnosticmethod``    Physics Methods
         ``emissionsmethod``     Physics Methods
         ``end_time``            Model Control
         ======================= ====================================
      
      .. tab-item:: F-L
         
         ======================= ====================================
         Field                   Location
         ======================= ====================================
         ``fai``                 Site Properties
         ``forcing_file``        Model Control
         ``laibasedon``          Physics Methods
         ``laimethod``           Physics Methods
         ``lat``                 Site Properties
         ``lng``                 Site Properties
         ======================= ====================================
      
      .. tab-item:: M-R
         
         ======================= ====================================
         Field                   Location
         ======================= ====================================
         ``netradiationmethod``  Physics Methods
         ``netwatermethod``      Physics Methods
         ``ohmincqf``            Physics Methods
         ``output_file``         Model Control → OutputConfig
         ``pai``                 Site Properties
         ``popdens``             Site Properties
         ``popdensdaytime``      Site Properties
         ``popdensnighttime``    Site Properties
         ``radlongcalcmethiod``  Physics Methods
         ``ref``                 Multiple locations
         ``roughlenheatmethod``  Physics Methods
         ``roughlenmommethod``   Physics Methods
         ======================= ====================================
      
      .. tab-item:: S-Z
         
         ======================= ====================================
         Field                   Location
         ======================= ====================================
         ``smdmethod``           Physics Methods
         ``snowalb``             Initial States
         ``snowuse``             Physics Methods
         ``stabilitymethod``     Physics Methods
         ``start_time``          Model Control
         ``storageheatmethod``   Physics Methods
         ``surfacearea``         Site Properties
         ``timezone``            Site Properties
         ``tstep``               Model Control
         ``waterusemethod``      Physics Methods
         ``z``                   Site Properties
         ``z0``                  Site Properties
         ``zd``                  Site Properties
         ======================= ====================================

---

Quick Navigation
----------------

.. grid:: 1 1 2 3
   :gutter: 2
   
   .. grid-item::
      
      **Model Setup**
      
      - :doc:`modelcontrol`
      - :doc:`modelphysics`
      - :doc:`outputconfig`
   
   .. grid-item::
      
      **Site Configuration**
      
      - :doc:`siteproperties`
      - :doc:`landcover`
      - :doc:`initialstates`
   
   .. grid-item::
      
      **Surface Types**
      
      - :doc:`pavedproperties`
      - :doc:`bldgsproperties`
      - :doc:`grassproperties`

.. toctree::
   :hidden:
   :maxdepth: 3

   index
   index-cards-dropdowns
   index-tabsets
   index-cards-badges
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