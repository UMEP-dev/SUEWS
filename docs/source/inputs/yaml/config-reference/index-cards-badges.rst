.. _yaml_config_reference_cards_badges:

YAML Configuration Reference - Cards with Badges
=================================================

This documentation follows the hierarchical structure of SUEWS YAML configuration files.

.. note::
   This version uses clean cards with badges and tables for optimal readability.

.. grid:: 1 1 2 2
   :gutter: 3

   .. grid-item-card:: 
      :class-header: bg-primary text-white
      :class-card: sd-rounded-3 shadow
      
      **SUEWS Configuration**
      ^^^
      
      .. button-link:: suewsconfig
         :color: light
         :align: right
         :outline:
         
         Full Documentation →
      
      +++
      
      **Model Setup** :bdg-info:`2 sections`
      
      .. list-table::
         :widths: 25 75
         :class: table-sm
         
         * - **Control**
           - | ``tstep``, ``forcing_file``, ``start_time``, ``end_time``, ``ref``
             | → :doc:`Output Config <outputconfig>`
         * - **Physics**
           - | ``netradiationmethod``, ``emissionsmethod``, ``storageheatmethod``
             | :bdg:`+13 more fields` → :doc:`Details <modelphysics>`

   .. grid-item-card::
      :class-header: bg-success text-white
      :class-card: sd-rounded-3 shadow
      
      **Site Configuration**
      ^^^
      
      .. button-link:: site
         :color: light
         :align: right
         :outline:
         
         Full Documentation →
      
      +++
      
      **Properties** :bdg-success:`15 core fields` :bdg-warning:`10 subsections`
      
      .. list-table::
         :widths: 100
         :class: table-sm
         
         * - **Core:** ``lat``, ``lng``, ``alt``, ``timezone``, ``surfacearea``
         * - **Urban:** ``z``, ``z0``, ``zd``, ``fai``, ``pai``
         * - **Population:** ``popdens``, ``popdensdaytime``, ``popdensnighttime``
      
      **Parameter Groups:**
      
      .. grid:: 2
         :gutter: 1
         
         .. grid-item::
            
            :bdg-link-success:`LUMPS <lumpsparams>` :bdg-link-success:`SPARTACUS <spartacusparams>`
            
            :bdg-link-success:`STEBBS <stebbsproperties>` :bdg-link-success:`Archetype <archetypeproperties>`
            
            :bdg-link-success:`Conductance <conductance>`
         
         .. grid-item::
            
            :bdg-link-warning:`Irrigation <irrigationparams>` :bdg-link-warning:`Emissions <anthropogenicemissions>` :bdg:`2 sub`
            
            :bdg-link-warning:`Snow <snowparams>` :bdg-link-warning:`Land Cover <landcover>` :bdg:`7 types`
            
            :bdg-link-warning:`Vertical <verticallayers>` :bdg:`2 sub`

.. grid:: 1
   :gutter: 3

   .. grid-item-card::
      :class-header: bg-info text-white
      :class-card: sd-rounded-3 shadow
      
      **Initial States Configuration**
      ^^^
      
      .. button-link:: initialstates
         :color: light
         :align: right
         :outline:
         
         Full Documentation →
      
      +++
      
      **Snow Parameter:** ``snowalb`` - Initial snow albedo
      
      **Surface Initial States** :bdg-primary:`9 surface types`
      
      .. tab-set::
         
         .. tab-item:: Built Surfaces
            
            .. grid:: 2
               :gutter: 1
               
               .. grid-item::
                  
                  :bdg-link-primary:`Paved <initialstatepaved>`
                  
                  :bdg-link-primary:`Buildings <initialstatebldgs>`
               
               .. grid-item::
                  
                  :bdg-link-primary:`Roofs <surfaceinitialstate>`
                  
                  :bdg-link-primary:`Walls <surfaceinitialstate>`
         
         .. tab-item:: Vegetation
            
            :bdg-link-success:`Evergreen Trees <initialstateevetr>` - Contains LAI fields
            
            :bdg-link-success:`Deciduous Trees <initialstatedectr>` - Contains LAI fields
            
            :bdg-link-success:`Grass <initialstategrass>` - Contains LAI fields
         
         .. tab-item:: Natural
            
            :bdg-link-warning:`Bare Soil <initialstatebsoil>`
            
            :bdg-link-info:`Water <initialstatewater>`

---

Quick Field Reference
---------------------

.. card:: Most Common Configuration Fields
   :class-card: sd-rounded-3
   
   .. tab-set::
      
      .. tab-item:: Model Control
         :sync: control
         
         ================== ============================================
         Field              Purpose
         ================== ============================================
         ``tstep``          Time step (seconds)
         ``forcing_file``   Input meteorological data
         ``start_time``     Simulation start
         ``end_time``       Simulation end
         ``output_file``    → OutputConfig object
         ================== ============================================
      
      .. tab-item:: Site Properties
         :sync: site
         
         =================== ==========================================
         Field               Purpose
         =================== ==========================================
         ``lat``, ``lng``    Location coordinates
         ``alt``             Altitude (m)
         ``timezone``        Time zone
         ``surfacearea``     Total area (m²)
         ``z``               Measurement height (m)
         ``z0``, ``zd``      Roughness parameters
         =================== ==========================================
      
      .. tab-item:: Physics Methods
         :sync: physics
         
         ======================= =====================================
         Field                   Calculation Method For
         ======================= =====================================
         ``netradiationmethod``  Net radiation
         ``emissionsmethod``     Emissions
         ``storageheatmethod``   Storage heat
         ``stabilitymethod``     Atmospheric stability
         ``snowuse``             Snow processes
         ======================= =====================================

.. toctree::
   :hidden:
   :maxdepth: 3

   index
   index-cards-dropdowns
   index-tabsets
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