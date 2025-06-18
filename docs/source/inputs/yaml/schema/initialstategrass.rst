Initialstategrass
=================

**Parameters:**

.. option:: state <RefValue[float]>

   Initial water state of the surface

   :Unit: mm
   :Default: ``0.0``

.. option:: soilstore <RefValue[float]>

   Initial soil store (essential for QE)

   :Unit: mm
   :Default: ``150.0``

.. option:: snowfrac <RefValue[float] (Optional)>

   Snow fraction

   :Unit: dimensionless
   :Default: ``0.0``

.. option:: snowpack <RefValue[float] (Optional)>

   Snow pack

   :Unit: mm
   :Default: ``0.0``

.. option:: icefrac <RefValue[float] (Optional)>

   Ice fraction

   :Unit: dimensionless
   :Default: ``0.0``

.. option:: snowwater <RefValue[float] (Optional)>

   Snow water

   :Unit: mm
   :Default: ``0.0``

.. option:: snowdens <RefValue[float] (Optional)>

   Snow density

   :Unit: kg m^-3
   :Default: ``0.0``

.. option:: temperature <RefValue[List[float]]>

   Initial temperature for each thermal layer

   :Unit: degC
   :Default: ``[15.0, 15.0, 15.0, 15.0, 15.0]``

.. option:: tsfc <RefValue[float] (Optional)>

   Initial exterior surface temperature

   :Unit: degC
   :Default: ``15.0``

.. option:: tin <RefValue[float] (Optional)>

   Initial interior surface temperature

   :Unit: degC
   :Default: ``20.0``

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.

.. option:: alb_id <RefValue[float]>

   Albedo at the start of the model run.

   :Unit: dimensionless
   :Default: ``0.25``

.. option:: lai_id <RefValue[float]>

   Leaf area index at the start of the model run.

   :Unit: m^2 m^-2
   :Default: ``1.0``

.. option:: gdd_id <RefValue[float]>

   Growing degree days at the start of the model run

   :Unit: degC d
   :Default: ``0``

.. option:: sdd_id <RefValue[float]>

   Senescence degree days at the start of the model run

   :Unit: degC d
   :Default: ``0``

.. option:: wu <WaterUse>

   :Default: ``PydanticUndefined``

   The ``wu`` parameter group is defined by the :doc:`wateruse` structure.
