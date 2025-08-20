.. _yaml_config_reference_cards_dropdowns:

YAML Configuration Reference - Cards with Dropdowns
====================================================

This documentation follows the hierarchical structure of SUEWS YAML configuration files.

.. note::
   This version uses cards with single-level dropdowns for clean organization.

.. grid:: 2
   :gutter: 3

   .. grid-item-card:: Model Configuration
      :link: model
      :link-type: doc
      :class-header: bg-primary text-white
      :class-card: sd-rounded-3
      
      Core model setup and physics
      ^^^
      
      .. dropdown:: Model Control
         :icon: gear
         :color: primary
         :animate: fade-in-slide-down
         
         → :doc:`Full documentation <modelcontrol>`
         
         **Fields:** ``tstep``, ``forcing_file``, ``start_time``, ``end_time``, ``ref``
         
         **Nested configurations:**
         
         - :bdg-link-primary:`Output Configuration <outputconfig>` - File format, frequency, and groups
      
      .. dropdown:: Physics Methods
         :icon: calculator
         :color: primary
         :animate: fade-in-slide-down
         
         → :doc:`Full documentation <modelphysics>`
         
         **All 16 physics options:**
         
         ``netradiationmethod``, ``emissionsmethod``, ``storageheatmethod``, ``ohmincqf``,
         ``roughlenmommethod``, ``roughlenheatmethod``, ``stabilitymethod``, ``snowuse``,
         ``netwatermethod``, ``smdmethod``, ``laimethod``, ``laibasedon``,
         ``diagnosticmethod``, ``waterusemethod``, ``radlongcalcmethiod``, ``ref``

   .. grid-item-card:: Site Configuration
      :link: site
      :link-type: doc
      :class-header: bg-success text-white
      :class-card: sd-rounded-3
      
      Site-specific parameters
      ^^^
      
      .. dropdown:: Site Properties
         :icon: map
         :color: success
         :animate: fade-in-slide-down
         
         → :doc:`Full documentation <siteproperties>`
         
         **Core fields:** ``lat``, ``lng``, ``alt``, ``timezone``, ``surfacearea``,
         ``popdens``, ``popdensdaytime``, ``popdensnighttime``, ``areawaterbody``,
         ``areawall``, ``z``, ``z0``, ``zd``, ``fai``, ``pai``
         
         **Major subsections:**
         
         - :bdg-link-success:`LUMPS Parameters <lumpsparams>`
         - :bdg-link-success:`SPARTACUS <spartacusparams>`
         - :bdg-link-success:`STEBBS <stebbsproperties>`
         - :bdg-link-success:`Archetype Properties <archetypeproperties>`
         - :bdg-link-success:`Conductance <conductance>`
         - :bdg-link-success:`Irrigation <irrigationparams>`
         - :bdg-link-success:`Anthropogenic Emissions <anthropogenicemissions>` (2 subsections)
         - :bdg-link-success:`Snow <snowparams>`
         - :bdg-link-success:`Land Cover <landcover>` (7 surface types)
         - :bdg-link-success:`Vertical Layers <verticallayers>` (2 subsections)
      
      .. dropdown:: Initial States
         :icon: play
         :color: success
         :animate: fade-in-slide-down
         
         → :doc:`Full documentation <initialstates>`
         
         **Field:** ``snowalb``
         
         **Surface initial states:**
         
         - :bdg-link-success:`Paved <initialstatepaved>`
         - :bdg-link-success:`Buildings <initialstatebldgs>`
         - :bdg-link-success:`Evergreen Tree <initialstateevetr>`
         - :bdg-link-success:`Deciduous Tree <initialstatedectr>`
         - :bdg-link-success:`Grass <initialstategrass>`
         - :bdg-link-success:`Bare Soil <initialstatebsoil>`
         - :bdg-link-success:`Water <initialstatewater>`
         - :bdg-link-success:`Roofs <surfaceinitialstate>`
         - :bdg-link-success:`Walls <surfaceinitialstate>`

.. toctree::
   :hidden:
   :maxdepth: 3

   index
   index-tabsets
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