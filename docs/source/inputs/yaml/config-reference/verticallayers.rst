.. meta::
   :description: SUEWS YAML configuration for vertical layers parameters
   :keywords: SUEWS, YAML, verticallayers, parameters, configuration

.. _verticallayers:

.. index::
   single: VerticalLayers (YAML parameter)
   single: YAML; VerticalLayers

Vertical Layers
===============

Vertical structure of surface layers.

**Parameters:**

.. index::
   single: nlayer (YAML parameter)
   single: VerticalLayers; nlayer

.. option:: nlayer

   Number of vertical layers in the urban canopy

   :Unit: dimensionless
   :Default: ``3``

.. index::
   single: height (YAML parameter)
   single: VerticalLayers; height

.. option:: height

   Heights of layer boundaries, length must be nlayer+1

   :Unit: m
   :Sample value: ``[0.0, 10.0, 20.0, 30.0]``

.. index::
   single: veg_frac (YAML parameter)
   single: VerticalLayers; veg_frac

.. option:: veg_frac

   Fraction of vegetation in each layer, length must be nlayer

   :Unit: dimensionless
   :Sample value: ``[0.0, 0.0, 0.0]``

.. index::
   single: veg_scale (YAML parameter)
   single: VerticalLayers; veg_scale

.. option:: veg_scale

   Scaling factor for vegetation in each layer, length must be nlayer

   :Unit: dimensionless
   :Default: ``[1.0, 1.0, 1.0]``

.. index::
   single: building_frac (YAML parameter)
   single: VerticalLayers; building_frac

.. option:: building_frac

   Fraction of buildings in each layer, must sum to 1.0, length must be nlayer

   :Unit: dimensionless
   :Sample value: ``[0.4, 0.3, 0.3]``

.. index::
   single: building_scale (YAML parameter)
   single: VerticalLayers; building_scale

.. option:: building_scale

   Scaling factor for buildings in each layer, length must be nlayer

   :Unit: dimensionless
   :Default: ``[1.0, 1.0, 1.0]``

.. index::
   single: roofs (YAML parameter)
   single: VerticalLayers; roofs

.. option:: roofs

   Properties for roof surfaces in each layer, length must be nlayer

   :Default: ``PydanticUndefined``

   Each item in the ``roofs`` list must conform to the :doc:`rooflayer` structure.

.. index::
   single: walls (YAML parameter)
   single: VerticalLayers; walls

.. option:: walls

   Properties for wall surfaces in each layer, length must be nlayer

   :Default: ``PydanticUndefined``

   Each item in the ``walls`` list must conform to the :doc:`walllayer` structure.

.. index::
   single: ref (YAML parameter)
   single: VerticalLayers; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
