.. meta::
   :description: SUEWS YAML configuration for land cover parameters
   :keywords: SUEWS, YAML, landcover, parameters, configuration

.. _landcover:

.. index::
   single: LandCover (YAML parameter)
   single: YAML; LandCover

Land Cover
==========

Surface properties for the seven SUEWS land cover types.

SUEWS divides the urban surface into seven distinct surface types, each with
unique thermal, radiative, and hydrological properties. The surface fractions
(sfr) for all seven types must sum to 1.0 for each site.

**Parameters:**

.. index::
   single: paved (YAML parameter)
   single: LandCover; paved

.. option:: paved

   Properties for paved surfaces like roads and pavements

   :Sample value: ``PydanticUndefined``

   The ``paved`` parameter group is defined by the :doc:`pavedproperties` structure.

.. index::
   single: bldgs (YAML parameter)
   single: LandCover; bldgs

.. option:: bldgs

   Properties for building surfaces including roofs and walls

   :Sample value: ``PydanticUndefined``

   The ``bldgs`` parameter group is defined by the :doc:`bldgsproperties` structure.

.. index::
   single: evetr (YAML parameter)
   single: LandCover; evetr

.. option:: evetr

   Properties for evergreen trees and vegetation

   :Sample value: ``PydanticUndefined``

   The ``evetr`` parameter group is defined by the :doc:`evetrproperties` structure.

.. index::
   single: dectr (YAML parameter)
   single: LandCover; dectr

.. option:: dectr

   Properties for deciduous trees and vegetation

   :Sample value: ``PydanticUndefined``

   The ``dectr`` parameter group is defined by the :doc:`dectrproperties` structure.

.. index::
   single: grass (YAML parameter)
   single: LandCover; grass

.. option:: grass

   Properties for grass surfaces

   :Sample value: ``PydanticUndefined``

   The ``grass`` parameter group is defined by the :doc:`grassproperties` structure.

.. index::
   single: bsoil (YAML parameter)
   single: LandCover; bsoil

.. option:: bsoil

   Properties for bare soil surfaces

   :Sample value: ``PydanticUndefined``

   The ``bsoil`` parameter group is defined by the :doc:`bsoilproperties` structure.

.. index::
   single: water (YAML parameter)
   single: LandCover; water

.. option:: water

   Properties for water surfaces like lakes and ponds

   :Sample value: ``PydanticUndefined``

   The ``water`` parameter group is defined by the :doc:`waterproperties` structure.

.. index::
   single: ref (YAML parameter)
   single: LandCover; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
