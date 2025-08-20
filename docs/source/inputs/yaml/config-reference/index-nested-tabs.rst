.. _yaml_config_reference_nested_tabs:

YAML Configuration Reference - Nested Tabs
===========================================

This documentation follows the hierarchical structure of SUEWS YAML configuration files.

.. note::
   This version uses nested tab sets for intuitive hierarchical navigation. Click tabs to explore different sections and subsections.

.. tab-set::

   .. tab-item:: Model Configuration
      :sync: model-main
      
      .. card::
         :class-header: bg-primary text-white
         
         **Model Setup & Control**
         ^^^
         
         Configure how SUEWS runs simulations, including time control and physics options.
         
         → :doc:`Complete Model Documentation <model>`
      
      .. tab-set::
         
         .. tab-item:: Control Parameters
            :sync: control
            
            .. card:: Model Control
               :link: modelcontrol
               :link-type: doc
               :class-card: sd-rounded-3
               
               **Time & File Management**
               ^^^
               
               **Core Fields:**
               
               .. list-table::
                  :widths: 30 70
                  :class: table-sm
                  
                  * - ``tstep``
                    - Time step in seconds for calculations
                  * - ``forcing_file``
                    - Path(s) to meteorological input data
                  * - ``start_time``
                    - Simulation start date/time
                  * - ``end_time``
                    - Simulation end date/time
                  * - ``ref``
                    - Reference metadata
               
               **Nested Configuration:**
               
               .. card:: Output Configuration
                  :link: outputconfig
                  :link-type: doc
                  :class-card: sd-rounded-2 sd-bg-light
                  
                  ``output_file`` → :doc:`outputconfig`
                  
                  - **Format:** ``txt`` or ``parquet``
                  - **Frequency:** Output interval (seconds)
                  - **Groups:** Data groups to save
         
         .. tab-item:: Physics Methods
            :sync: physics
            
            .. card:: Physics Calculation Options
               :link: modelphysics
               :link-type: doc
               :class-card: sd-rounded-3
               
               **16 Calculation Methods**
               ^^^
               
               .. tab-set::
                  
                  .. tab-item:: Radiation & Energy
                     
                     .. list-table::
                        :widths: 40 60
                        :class: table-sm
                        
                        * - ``netradiationmethod``
                          - Net all-wave radiation calculation
                        * - ``radlongcalcmethiod``
                          - Longwave radiation method
                        * - ``emissionsmethod``
                          - Anthropogenic emissions
                        * - ``storageheatmethod``
                          - Storage heat flux (OHM)
                        * - ``ohmincqf``
                          - Include QF in OHM calculation
                  
                  .. tab-item:: Surface Properties
                     
                     .. list-table::
                        :widths: 40 60
                        :class: table-sm
                        
                        * - ``roughlenmommethod``
                          - Momentum roughness length
                        * - ``roughlenheatmethod``
                          - Heat roughness length
                        * - ``stabilitymethod``
                          - Atmospheric stability
                        * - ``faimethod``
                          - Frontal area index method
                        * - ``rslmethod``
                          - Roughness sublayer model
                  
                  .. tab-item:: Water & Vegetation
                     
                     .. list-table::
                        :widths: 40 60
                        :class: table-sm
                        
                        * - ``snowuse``
                          - Snow processes flag
                        * - ``netwatermethod``
                          - Net water calculation
                        * - ``smdmethod``
                          - Soil moisture deficit
                        * - ``laimethod``
                          - LAI calculation method
                        * - ``laibasedon``
                          - LAI data source
                        * - ``waterusemethod``
                          - Water use calculation
                  
                  .. tab-item:: Other Options
                     
                     .. list-table::
                        :widths: 40 60
                        :class: table-sm
                        
                        * - ``diagnosticmethod``
                          - Diagnostic output level
                        * - ``gsmodel``
                          - Surface conductance model
                        * - ``stebbsmethod``
                          - STEBBS calculation flag
                        * - ``rsllevel``
                          - RSL calculation level
                        * - ``ref``
                          - Reference metadata

   .. tab-item:: Site Configuration
      :sync: site-main
      
      .. card::
         :class-header: bg-success text-white
         
         **Site Properties & Settings**
         ^^^
         
         Define site-specific parameters, surface characteristics, and initial conditions.
         
         → :doc:`Complete Site Documentation <site>`
      
      .. tab-set::
         
         .. tab-item:: Core Properties
            :sync: core-props
            
            .. card:: Site Properties
               :link: siteproperties
               :link-type: doc
               :class-card: sd-rounded-3
               
               **Essential Site Parameters**
               ^^^
               
               .. tab-set::
                  
                  .. tab-item:: Geographic
                     
                     .. list-table::
                        :widths: 30 70
                        :class: table-sm
                        
                        * - ``lat``
                          - Latitude (degrees)
                        * - ``lng``
                          - Longitude (degrees)
                        * - ``alt``
                          - Altitude (m)
                        * - ``timezone``
                          - UTC offset
                        * - ``surfacearea``
                          - Total area (m²)
                  
                  .. tab-item:: Urban Form
                     
                     .. list-table::
                        :widths: 30 70
                        :class: table-sm
                        
                        * - ``z``
                          - Measurement height (m)
                        * - ``z0m_in``
                          - Roughness length for momentum (m)
                        * - ``zdm_in``
                          - Zero-plane displacement (m)
                        * - ``h_std``
                          - Standard deviation of building height
                        * - ``lambda_c``
                          - Plan area fraction
                  
                  .. tab-item:: Infrastructure
                     
                     .. list-table::
                        :widths: 30 70
                        :class: table-sm
                        
                        * - ``pipecapacity``
                          - Drainage capacity (mm/h)
                        * - ``runofftowater``
                          - Runoff to water bodies fraction
                        * - ``narp_trans_site``
                          - NARP transmissivity
                        * - ``n_buildings``
                          - Number of buildings
                        * - ``ref``
                          - Reference metadata
         
         .. tab-item:: Parameter Groups
            :sync: param-groups
            
            .. grid:: 1 1 2 2
               :gutter: 3
               
               .. grid-item::
                  
                  .. card:: Model Parameters
                     :class-card: sd-rounded-2
                     
                     **Specialized Models**
                     
                     - :doc:`LUMPS <lumpsparams>` - LUMPS model
                     - :doc:`SPARTACUS <spartacusparams>` - 3D radiation
                     - :doc:`STEBBS <stebbsproperties>` - Energy balance
                     - :doc:`Archetype <archetypeproperties>` - Building types
                     - :doc:`Conductance <conductance>` - Surface conductance
               
               .. grid-item::
                  
                  .. card:: Process Parameters
                     :class-card: sd-rounded-2
                     
                     **Physical Processes**
                     
                     - :doc:`Irrigation <irrigationparams>` - Water application
                     - :doc:`Emissions <anthropogenicemissions>` - Heat & CO₂
                     - :doc:`Snow <snowparams>` - Snow processes
                     - :doc:`Land Cover <landcover>` - 7 surface types
                     - :doc:`Vertical Layers <verticallayers>` - Building structure
         
         .. tab-item:: Land Cover
            :sync: landcover
            
            .. card:: Surface Types Configuration
               :link: landcover
               :link-type: doc
               :class-card: sd-rounded-3
               
               **7 Surface Types**
               ^^^
               
               .. tab-set::
                  
                  .. tab-item:: Built Surfaces
                     
                     .. grid:: 1 1 2 2
                        :gutter: 2
                        
                        .. grid-item::
                           
                           **Paved** → :doc:`pavedproperties`
                           
                           - Roads, parking lots
                           - Impervious surfaces
                           - Surface fraction: ``paved``
                        
                        .. grid-item::
                           
                           **Buildings** → :doc:`bldgsproperties`
                           
                           - Building footprints
                           - Roofs and walls
                           - Surface fraction: ``bldgs``
                  
                  .. tab-item:: Vegetation
                     
                     .. grid:: 1 1 3 3
                        :gutter: 2
                        
                        .. grid-item::
                           
                           **Evergreen** → :doc:`evetrproperties`
                           
                           - Coniferous trees
                           - Year-round LAI
                           - Fraction: ``evetr``
                        
                        .. grid-item::
                           
                           **Deciduous** → :doc:`dectrproperties`
                           
                           - Broadleaf trees
                           - Seasonal LAI
                           - Fraction: ``dectr``
                        
                        .. grid-item::
                           
                           **Grass** → :doc:`grassproperties`
                           
                           - Lawns, parks
                           - Low vegetation
                           - Fraction: ``grass``
                  
                  .. tab-item:: Natural
                     
                     .. grid:: 1 1 2 2
                        :gutter: 2
                        
                        .. grid-item::
                           
                           **Bare Soil** → :doc:`bsoilproperties`
                           
                           - Exposed soil
                           - Unpaved surfaces
                           - Surface fraction: ``bsoil``
                        
                        .. grid-item::
                           
                           **Water** → :doc:`waterproperties`
                           
                           - Water bodies
                           - Rivers, lakes
                           - Surface fraction: ``water``
         
         .. tab-item:: Initial States
            :sync: initial
            
            .. card:: Initial Conditions
               :link: initialstates
               :link-type: doc
               :class-card: sd-rounded-3
               
               **Starting Conditions for All Surfaces**
               ^^^
               
               **Snow:** ``snowalb`` - Initial snow albedo
               
               .. tab-set::
                  
                  .. tab-item:: Built Surfaces
                     
                     .. list-table::
                        :widths: 40 60
                        :class: table-sm
                        
                        * - :doc:`Paved <initialstatepaved>`
                          - Roads and parking initial state
                        * - :doc:`Buildings <initialstatebldgs>`
                          - Building surface initial state
                        * - :doc:`Roofs <surfaceinitialstate>`
                          - Roof-specific initial conditions
                        * - :doc:`Walls <surfaceinitialstate>`
                          - Wall-specific initial conditions
                  
                  .. tab-item:: Vegetation
                     
                     .. list-table::
                        :widths: 40 60
                        :class: table-sm
                        
                        * - :doc:`Evergreen <initialstateevetr>`
                          - Includes LAI initialization
                        * - :doc:`Deciduous <initialstatedectr>`
                          - Includes LAI initialization
                        * - :doc:`Grass <initialstategrass>`
                          - Includes LAI initialization
                  
                  .. tab-item:: Natural
                     
                     .. list-table::
                        :widths: 40 60
                        :class: table-sm
                        
                        * - :doc:`Bare Soil <initialstatebsoil>`
                          - Soil moisture and temperature
                        * - :doc:`Water <initialstatewater>`
                          - Water body initial state

   .. tab-item:: Quick Reference
      :sync: reference
      
      .. card::
         :class-header: bg-info text-white
         
         **Field Lookup & Navigation**
         ^^^
         
         Quick access to all configuration fields and their locations.
      
      .. tab-set::
         
         .. tab-item:: Essential Fields
            
            .. card:: Most Common Parameters
               :class-card: sd-rounded-2
               
               .. list-table::
                  :header-rows: 1
                  :widths: 30 30 40
                  :class: table-sm
                  
                  * - Field
                    - Location
                    - Purpose
                  * - ``tstep``
                    - Model Control
                    - Time step (seconds)
                  * - ``forcing_file``
                    - Model Control
                    - Input meteorological data
                  * - ``lat``, ``lng``
                    - Site Properties
                    - Geographic coordinates
                  * - ``surfacearea``
                    - Site Properties
                    - Total area (m²)
                  * - ``z``
                    - Site Properties
                    - Measurement height (m)
                  * - ``paved``, ``bldgs``, etc.
                    - Land Cover
                    - Surface fractions (sum = 1.0)
         
         .. tab-item:: All Fields A-M
            
            .. list-table::
               :header-rows: 1
               :widths: 35 65
               :class: table-sm
               
               * - Field
                 - Location & Purpose
               * - ``alt``
                 - Site Properties - Altitude (m)
               * - ``diagnosticmethod``
                 - Physics Methods - Output detail level
               * - ``emissionsmethod``
                 - Physics Methods - Anthropogenic heat
               * - ``end_time``
                 - Model Control - Simulation end
               * - ``faimethod``
                 - Physics Methods - FAI calculation
               * - ``forcing_file``
                 - Model Control - Met data input
               * - ``gsmodel``
                 - Physics Methods - Conductance model
               * - ``h_std``
                 - Site Properties - Building height std dev
               * - ``laibasedon``
                 - Physics Methods - LAI data source
               * - ``laimethod``
                 - Physics Methods - LAI calculation
               * - ``lambda_c``
                 - Site Properties - Plan area fraction
               * - ``lat``
                 - Site Properties - Latitude
               * - ``lng``
                 - Site Properties - Longitude
         
         .. tab-item:: All Fields N-Z
            
            .. list-table::
               :header-rows: 1
               :widths: 35 65
               :class: table-sm
               
               * - Field
                 - Location & Purpose
               * - ``n_buildings``
                 - Site Properties - Building count
               * - ``narp_trans_site``
                 - Site Properties - NARP transmissivity
               * - ``netradiationmethod``
                 - Physics Methods - Net radiation
               * - ``netwatermethod``
                 - Physics Methods - Water balance
               * - ``ohmincqf``
                 - Physics Methods - Include QF in OHM
               * - ``output_file``
                 - Model Control → OutputConfig
               * - ``pipecapacity``
                 - Site Properties - Drainage (mm/h)
               * - ``radlongcalcmethiod``
                 - Physics Methods - Longwave radiation
               * - ``ref``
                 - Multiple - Reference metadata
               * - ``roughlenheatmethod``
                 - Physics Methods - Heat roughness
               * - ``roughlenmommethod``
                 - Physics Methods - Momentum roughness
               * - ``rslmethod``
                 - Physics Methods - RSL model
               * - ``rsllevel``
                 - Physics Methods - RSL height
               * - ``runofftowater``
                 - Site Properties - Runoff fraction
               * - ``smdmethod``
                 - Physics Methods - Soil moisture
               * - ``snowalb``
                 - Initial States - Snow albedo
               * - ``snowuse``
                 - Physics Methods - Snow flag
               * - ``stabilitymethod``
                 - Physics Methods - Stability
               * - ``start_time``
                 - Model Control - Simulation start
               * - ``stebbsmethod``
                 - Physics Methods - STEBBS flag
               * - ``storageheatmethod``
                 - Physics Methods - Storage heat
               * - ``surfacearea``
                 - Site Properties - Total area
               * - ``timezone``
                 - Site Properties - UTC offset
               * - ``tstep``
                 - Model Control - Time step
               * - ``waterusemethod``
                 - Physics Methods - Water use
               * - ``z``
                 - Site Properties - Height
               * - ``z0m_in``
                 - Site Properties - z0 momentum
               * - ``zdm_in``
                 - Site Properties - Zero-plane

   .. tab-item:: Navigation Help
      :sync: nav
      
      .. card::
         :class-header: bg-warning
         
         **How to Use This Documentation**
         ^^^
         
         .. grid:: 1 1 2 2
            :gutter: 3
            
            .. grid-item::
               
               **Navigation Tips:**
               
               1. **Main tabs** show major sections
               2. **Nested tabs** organize subsections
               3. **Links** connect to detailed pages
               4. **Tables** provide quick lookups
               5. **Cards** highlight key information
               
               **Color Coding:**
               
               - :bdg-primary:`Primary` - Model configuration
               - :bdg-success:`Success` - Site properties
               - :bdg-info:`Info` - Initial states
               - :bdg-warning:`Warning` - Important notes
            
            .. grid-item::
               
               **Quick Links:**
               
               **Most Used Pages:**
               
               - :doc:`modelcontrol` - Time & file settings
               - :doc:`modelphysics` - Physics options
               - :doc:`siteproperties` - Site parameters
               - :doc:`landcover` - Surface types
               - :doc:`initialstates` - Initial conditions
               
               **Output & Validation:**
               
               - :doc:`outputconfig` - Output settings
               - :doc:`/inputs/yaml/validation` - Config validation

---

.. admonition:: Pro Tip
   :class: tip
   
   Use the **Quick Reference** tab for fast field lookups, or navigate through the hierarchical tabs to understand the configuration structure.

.. toctree::
   :hidden:
   :maxdepth: 3

   index
   index-cards-dropdowns
   index-tabsets
   index-cards-badges
   index-accordion
   index-definition
   index-showcase
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