.. _yaml_config_reference:

YAML Configuration Reference
============================

This documentation follows the hierarchical structure of SUEWS YAML configuration files.

* :doc:`SUEWS Config <suewsconfig>`

  - :opt:`name <inline link to name>`

  - :opt:`description <inline link to description>`

  - :opt:`schema_version <inline link to schema_version>`


  - :doc:`Model Configuration <model>`


    = :doc:`Model Control <modelcontrol>`

      + :opt:`tstep <inline link to tstep>`

      + :opt:`forcing_file <inline link to forcing_file>`

      + :opt:`start_time <inline link to start_time>`

      + :opt:`end_time <inline link to end_time>`

      .. similar fields


    = :doc:`Physics Methods <modelphysics>`

      + :opt:`netradiationmethod <inline link to netradiationmethod>`

      + :opt:`emissionsmethod <inline link to emissionsmethod>`

      + :opt:`storageheatmethod <inline link to storageheatmethod>`

      + :opt:`ohmincqf <inline link to ohmincqf>`

      + :opt:`roughlenmommethod <inline link to roughlenmommethod>`

      + :opt:`roughlenheatmethod <inline link to roughlenheatmethod>`

      + :opt:`roughlenmommethod <inline link to roughlenmommethod>`


      .. similar fields

  - :doc:`Sites <site>`

    = :opt:`name <inline link to name>`

    = :opt:`gridiv <inline link to gridiv>`

    = :doc:`Site Properties <siteproperties>`

      + :opt:`lat <inline link to lat>`

      + :opt:`lng <inline link to lng>`

      + :opt:`alt <inline link to alt>`

      + :opt:`timezone <inline link to timezone>`

      + :opt:`surfacearea <inline link to surfacearea>`

      .. similar fields

    = :doc:`Initial States <initialstates>`

      + :opt:`snowalb <inline link to snowalb>`

      .. similar fields


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