.. _yaml_config_reference_definition:

YAML Configuration Reference - Definition Lists
================================================

This documentation follows the hierarchical structure of SUEWS YAML configuration files.

.. note::
   This version uses clean definition lists with icons and visual hierarchy.

.. card::
   :class-card: sd-rounded-3 shadow
   :class-header: bg-dark text-white
   
   **SUEWS YAML Configuration Structure**
   ^^^
   
   :octicon:`package;1em;sd-text-primary` **SUEWS Config** → :doc:`suewsconfig`
      The root configuration object containing all model and site settings.
      
      :octicon:`settings;1em;sd-text-info` **Model Configuration** → :doc:`model`
         Controls how SUEWS runs simulations.
         
         **Model Control** (:doc:`modelcontrol`)
            :Fields: ``tstep``, ``forcing_file``, ``start_time``, ``end_time``, ``ref``
            :Nested: :bdg-link-primary:`Output Configuration <outputconfig>` - Format, frequency, groups
         
         **Physics Methods** (:doc:`modelphysics`)
            :Fields: 16 physics calculation options
            :Examples: ``netradiationmethod``, ``emissionsmethod``, ``storageheatmethod``
            :More: ``ohmincqf``, ``roughlenmommethod``, ``roughlenheatmethod``, ``stabilitymethod``
            :Water: ``snowuse``, ``netwatermethod``, ``smdmethod``, ``waterusemethod``
            :Vegetation: ``laimethod``, ``laibasedon``
            :Other: ``diagnosticmethod``, ``radlongcalcmethiod``, ``ref``
      
      :octicon:`location;1em;sd-text-success` **Site Configuration** → :doc:`site`
         Site-specific parameters and initial conditions.
         
         **Site Properties** (:doc:`siteproperties`)
            :Geographic: ``lat``, ``lng``, ``alt``, ``timezone``
            :Physical: ``surfacearea``, ``areawaterbody``, ``areawall``
            :Urban Form: ``z``, ``z0``, ``zd``, ``fai``, ``pai``
            :Population: ``popdens``, ``popdensdaytime``, ``popdensnighttime``
            
            .. card:: Parameter Groups
               :class-card: sd-rounded-2 sd-bg-light
               
               .. list-table::
                  :widths: 40 60
                  :class: table-sm
                  
                  * - :doc:`LUMPS Parameters <lumpsparams>`
                    - LUMPS model parameters
                  * - :doc:`SPARTACUS <spartacusparams>`
                    - 3D radiation model
                  * - :doc:`STEBBS <stebbsproperties>`
                    - STEBBS energy balance
                  * - :doc:`Archetype Properties <archetypeproperties>`
                    - Building characteristics
                  * - :doc:`Conductance <conductance>`
                    - Surface conductance
                  * - :doc:`Irrigation <irrigationparams>`
                    - Irrigation parameters
                  * - :doc:`Anthropogenic Emissions <anthropogenicemissions>`
                    - Heat and CO₂ emissions :bdg:`2 subsections`
                  * - :doc:`Snow <snowparams>`
                    - Snow processes
                  * - :doc:`Land Cover <landcover>`
                    - Surface types :bdg:`7 types`
                  * - :doc:`Vertical Layers <verticallayers>`
                    - Building layers :bdg:`2 subsections`
         
         **Initial States** (:doc:`initialstates`)
            :Snow: ``snowalb`` - Initial snow albedo
            :Built: | :doc:`Paved <initialstatepaved>` | :doc:`Buildings <initialstatebldgs>`
                    | :doc:`Roofs <surfaceinitialstate>` | :doc:`Walls <surfaceinitialstate>`
            :Vegetation: | :doc:`Evergreen Trees <initialstateevetr>` (with LAI)
                        | :doc:`Deciduous Trees <initialstatedectr>` (with LAI)
                        | :doc:`Grass <initialstategrass>` (with LAI)
            :Natural: | :doc:`Bare Soil <initialstatebsoil>` | :doc:`Water <initialstatewater>`

---

Field Quick Reference
---------------------

.. card:: Field Lookup by Category
   :class-card: sd-rounded-3
   
   .. tab-set::
      
      .. tab-item:: Essential Fields
         
         .. glossary::
            
            tstep
               :Location: Model Control
               :Type: Integer
               :Purpose: Time step in seconds for calculations
            
            forcing_file
               :Location: Model Control
               :Type: String or List[String]
               :Purpose: Path(s) to meteorological input data
            
            lat, lng
               :Location: Site Properties
               :Type: Float
               :Purpose: Geographic coordinates (degrees)
            
            surfacearea
               :Location: Site Properties
               :Type: Float
               :Purpose: Total surface area (m²)
      
      .. tab-item:: Physics Options
         
         .. glossary::
            
            netradiationmethod
               :Location: Physics Methods
               :Type: Integer
               :Values: Method selection for net radiation
            
            storageheatmethod
               :Location: Physics Methods
               :Type: Integer
               :Values: OHM options for storage heat
            
            stabilitymethod
               :Location: Physics Methods
               :Type: Integer
               :Values: Atmospheric stability calculation
      
      .. tab-item:: Urban Form
         
         .. glossary::
            
            z
               :Location: Site Properties
               :Type: Float
               :Purpose: Measurement height (m)
            
            z0, zd
               :Location: Site Properties
               :Type: Float
               :Purpose: Roughness length and displacement (m)
            
            fai, pai
               :Location: Site Properties
               :Type: Float
               :Purpose: Frontal and plan area indices

.. admonition:: Navigation Tips
   :class: tip
   
   - Click any :doc:`blue link <modelcontrol>` to view detailed documentation
   - Use :bdg:`badges` to see counts and subsections
   - Fields shown as ``monospace`` are parameter names
   - The :octicon:`gear` icons indicate configuration sections

.. toctree::
   :hidden:
   :maxdepth: 3

   index
   index-cards-dropdowns
   index-tabsets
   index-cards-badges
   index-accordion
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