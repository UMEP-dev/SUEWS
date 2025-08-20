.. meta::
   :description: SUEWS YAML configuration for site configuration parameters
   :keywords: SUEWS, YAML, site, parameters, configuration

.. _site:

.. index::
   single: Site (YAML parameter)
   single: YAML; Site

Site Configuration
==================

Site configuration for SUEWS simulations.

Represents the complete configuration for a single SUEWS simulation site,
including all physical properties, initial states, and model parameters.

**Parameters:**

.. index::
   single: name (YAML parameter)
   single: Site; name

.. option:: name

   Name of the site

   :Sample value: ``'test site'``

.. index::
   single: gridiv (YAML parameter)
   single: Site; gridiv

.. option:: gridiv

   Grid ID for identifying this site in multi-site simulations

   :Sample value: ``1``

.. index::
   single: properties (YAML parameter)
   single: Site; properties

.. option:: properties

   Physical and morphological properties of the site

   :Sample value: ``PydanticUndefined``

   The ``properties`` parameter group is defined by the :doc:`siteproperties` structure.

.. index::
   single: initial_states (YAML parameter)
   single: Site; initial_states

.. option:: initial_states

   Initial conditions for model state variables

   :Sample value: ``PydanticUndefined``

   The ``initial_states`` parameter group is defined by the :doc:`initialstates` structure.
