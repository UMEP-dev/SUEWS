.. _yaml_config_reference_simple:

YAML Configuration Reference
=============================

This documentation follows the hierarchical structure of SUEWS YAML configuration files.

**SUEWS Config** → :doc:`suewsconfig`
  
  * **Model Configuration** → :doc:`model`
    
    - **Model Control** → :doc:`modelcontrol`
      
      Simple fields: ``tstep``, ``forcing_file``, ``start_time``, ``end_time``, ``ref``
      
      Nested: :doc:`Output Configuration <outputconfig>`
    
    - **Physics Methods** → :doc:`modelphysics`
      
      Fields: ``netradiationmethod``, ``emissionsmethod``, ``storageheatmethod``, ``ohmincqf``, 
      ``roughlenmommethod``, ``roughlenheatmethod``, ``stabilitymethod``, ``snowuse``, 
      ``netwatermethod``, ``smdmethod``, ``laimethod``, ``laibasedon``, ``diagnosticmethod``, 
      ``waterusemethod``, ``radlongcalcmethiod``, ``ref``
  
  * **Sites** → :doc:`site`
    
    - **Site Properties** → :doc:`siteproperties`
      
      Core fields: ``lat``, ``lng``, ``alt``, ``timezone``, ``surfacearea``, ``z``, ``z0m_in``, 
      ``zdm_in``, ``pipecapacity``, ``runofftowater``, ``narp_trans_site``, ``n_buildings``, 
      ``h_std``, ``lambda_c``, ``ref``
      
      Parameter groups:
      
      - :doc:`LUMPS Parameters <lumpsparams>`
      - :doc:`SPARTACUS <spartacusparams>`
      - :doc:`STEBBS <stebbsproperties>`
      - :doc:`Archetype Properties <archetypeproperties>`
      - :doc:`Conductance <conductance>`
      - :doc:`Irrigation <irrigationparams>`
      - :doc:`Anthropogenic Emissions <anthropogenicemissions>`
        
        - :doc:`Anthropogenic Heat <anthropogenicheat>`
        - :doc:`CO2 Parameters <co2params>`
      
      - :doc:`Snow <snowparams>`
      - :doc:`Land Cover <landcover>`
        
        - :doc:`Paved <pavedproperties>`
        - :doc:`Buildings <bldgsproperties>`
        - :doc:`Evergreen Trees <evetrproperties>`
        - :doc:`Deciduous Trees <dectrproperties>`
        - :doc:`Grass <grassproperties>`
        - :doc:`Bare Soil <bsoilproperties>`
        - :doc:`Water <waterproperties>`
      
      - :doc:`Vertical Layers <verticallayers>`
        
        - :doc:`Roof Layer <rooflayer>`
        - :doc:`Wall Layer <walllayer>`
    
    - **Initial States** → :doc:`initialstates`
      
      Field: ``snowalb``
      
      Surface initial states:
      
      - :doc:`Paved <initialstatepaved>`
      - :doc:`Buildings <initialstatebldgs>`
      - :doc:`Evergreen Trees <initialstateevetr>` (includes LAI)
      - :doc:`Deciduous Trees <initialstatedectr>` (includes LAI)
      - :doc:`Grass <initialstategrass>` (includes LAI)
      - :doc:`Bare Soil <initialstatebsoil>`
      - :doc:`Water <initialstatewater>`
      - :doc:`Roofs <surfaceinitialstate>`
      - :doc:`Walls <surfaceinitialstate>`

---

Quick Field Reference
---------------------

Common Configuration Fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~

=============================== ================================= ==================================
Field                           Location                          Description
=============================== ================================= ==================================
``tstep``                       Model Control                     Time step (seconds)
``forcing_file``                Model Control                     Meteorological input data
``start_time``, ``end_time``    Model Control                     Simulation period
``lat``, ``lng``                Site Properties                   Geographic coordinates
``alt``                         Site Properties                   Altitude (m)
``timezone``                    Site Properties                   UTC offset
``surfacearea``                 Site Properties                   Total area (m²)
``z``                           Site Properties                   Measurement height (m)
``z0m_in``, ``zdm_in``          Site Properties                   Roughness parameters
``netradiationmethod``          Physics Methods                   Net radiation calculation
``storageheatmethod``           Physics Methods                   Storage heat method
``snowuse``                     Physics Methods                   Snow processes flag
=============================== ================================= ==================================

Surface Fractions
~~~~~~~~~~~~~~~~~

Surface fractions in Land Cover must sum to 1.0:

- ``paved`` - Roads and parking
- ``bldgs`` - Building footprint
- ``evetr`` - Evergreen trees
- ``dectr`` - Deciduous trees
- ``grass`` - Grass surfaces
- ``bsoil`` - Bare soil
- ``water`` - Water bodies

.. toctree::
   :hidden:
   :maxdepth: 3

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