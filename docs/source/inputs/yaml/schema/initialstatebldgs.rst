Initialstatebldgs
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
