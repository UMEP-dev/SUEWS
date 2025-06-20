Verticallayers
==============

**Parameters:**

.. option:: nlayer <RefValue[int]>

   Number of vertical layers in the urban canopy

   :Unit: dimensionless
   :Default: ``3``

.. option:: height <RefValue[List[float]]>

   Heights of layer boundaries, length must be nlayer+1

   :Unit: m
   :Default: ``[0.0, 10.0, 20.0, 30.0]``

.. option:: veg_frac <RefValue[List[float]]>

   Fraction of vegetation in each layer, length must be nlayer

   :Unit: dimensionless
   :Default: ``[0.0, 0.0, 0.0]``

.. option:: veg_scale <RefValue[List[float]]>

   Scaling factor for vegetation in each layer, length must be nlayer

   :Unit: dimensionless
   :Default: ``[1.0, 1.0, 1.0]``

.. option:: building_frac <RefValue[List[float]]>

   Fraction of buildings in each layer, must sum to 1.0, length must be nlayer

   :Unit: dimensionless
   :Default: ``[0.4, 0.3, 0.3]``

.. option:: building_scale <RefValue[List[float]]>

   Scaling factor for buildings in each layer, length must be nlayer

   :Unit: dimensionless
   :Default: ``[1.0, 1.0, 1.0]``

.. option:: roofs <List of RoofLayer>

   Properties for roof surfaces in each layer, length must be nlayer

   :Default: ``PydanticUndefined``

   Each item in the ``roofs`` list must conform to the :doc:`rooflayer` structure.

.. option:: walls <List of WallLayer>

   Properties for wall surfaces in each layer, length must be nlayer

   :Default: ``PydanticUndefined``

   Each item in the ``walls`` list must conform to the :doc:`walllayer` structure.

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
