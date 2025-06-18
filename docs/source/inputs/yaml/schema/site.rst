Site
====

**Parameters:**

.. option:: name <str>

   Name of the site

   :Default: ``'test site'``

.. option:: gridiv <int>

   Grid ID for identifying this site in multi-site simulations

   :Default: ``1``

.. option:: properties <SiteProperties>

   Physical and morphological properties of the site

   :Default: ``PydanticUndefined``

   The ``properties`` parameter group is defined by the :doc:`siteproperties` structure.

.. option:: initial_states <InitialStates>

   Initial conditions for model state variables

   :Default: ``PydanticUndefined``

   The ``initial_states`` parameter group is defined by the :doc:`initialstates` structure.
