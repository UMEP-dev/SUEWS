.. meta::
   :description: SUEWS YAML configuration for suews config parameters
   :keywords: SUEWS, YAML, suewsconfig, parameters, configuration

.. _suewsconfig:

.. index::
   single: SUEWSConfig (YAML parameter)
   single: YAML; SUEWSConfig

SUEWS Config
============

Main SUEWS configuration.

**Parameters:**

.. index::
   single: name (YAML parameter)
   single: SUEWSConfig; name

.. option:: name

   Name of the SUEWS configuration

   :Default: ``'sample config'``

.. index::
   single: schema_version (YAML parameter)
   single: SUEWSConfig; schema_version

.. option:: schema_version

   Configuration schema version (e.g., '0.1', '1.0', '1.1'). Only changes when configuration structure changes.

   :Default: ``'0.1'``

.. index::
   single: description (YAML parameter)
   single: SUEWSConfig; description

.. option:: description

   Description of this SUEWS configuration

   :Default: ``'this is a sample config for testing purposes ONLY - values are not realistic'``

.. index::
   single: model (YAML parameter)
   single: SUEWSConfig; model

.. option:: model

   Model control and physics parameters

   :Default: ``PydanticUndefined``

   The ``model`` parameter group is defined by the :doc:`model` structure.

.. index::
   single: sites (YAML parameter)
   single: SUEWSConfig; sites

.. option:: sites

   List of sites to simulate

   :Default: ``PydanticUndefined``

   Each item in the ``sites`` list must conform to the :doc:`site` structure.
