.. meta::
   :description: SUEWS YAML configuration for grass initial state parameters
   :keywords: SUEWS, YAML, initialstategrass, parameters, configuration

.. _initialstategrass:

.. index::
   single: InitialStateGrass (YAML parameter)
   single: YAML; InitialStateGrass

Grass Initial State
===================

Initial state for grass surfaces with vegetation parameters.

**Parameters:**

.. index::
   single: state (YAML parameter)
   single: InitialStateGrass; state

.. option:: state

   Initial water state of the surface

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: soilstore (YAML parameter)
   single: InitialStateGrass; soilstore

.. option:: soilstore

   Initial soil store (essential for QE)

   :Unit: mm
   :Sample value: ``150.0``

.. index::
   single: snowfrac (YAML parameter)
   single: InitialStateGrass; snowfrac

.. option:: snowfrac

   Snow fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowpack (YAML parameter)
   single: InitialStateGrass; snowpack

.. option:: snowpack

   Snow pack

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: icefrac (YAML parameter)
   single: InitialStateGrass; icefrac

.. option:: icefrac

   Ice fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowwater (YAML parameter)
   single: InitialStateGrass; snowwater

.. option:: snowwater

   Snow water

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: snowdens (YAML parameter)
   single: InitialStateGrass; snowdens

.. option:: snowdens

   Snow density

   :Unit: kg |m^-3|
   :Sample value: ``0.0``

.. index::
   single: temperature (YAML parameter)
   single: InitialStateGrass; temperature

.. option:: temperature

   Initial temperature for each thermal layer

   :Unit: degC
   :Sample value: ``[15.0, 15.0, 15.0, 15.0, 15.0]``

.. index::
   single: tsfc (YAML parameter)
   single: InitialStateGrass; tsfc

.. option:: tsfc

   Initial exterior surface temperature

   :Unit: degC
   :Sample value: ``15.0``

.. index::
   single: tin (YAML parameter)
   single: InitialStateGrass; tin

.. option:: tin

   Initial interior surface temperature

   :Unit: degC
   :Sample value: ``20.0``

.. index::
   single: ref (YAML parameter)
   single: InitialStateGrass; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.

.. index::
   single: alb_id (YAML parameter)
   single: InitialStateGrass; alb_id

.. option:: alb_id

   Albedo at the start of the model run.

   :Unit: dimensionless
   :Sample value: ``0.25``

.. index::
   single: lai_id (YAML parameter)
   single: InitialStateGrass; lai_id

.. option:: lai_id

   Leaf area index at the start of the model run.

   :Unit: |m^2| |m^-2|
   :Sample value: ``1.0``

.. index::
   single: gdd_id (YAML parameter)
   single: InitialStateGrass; gdd_id

.. option:: gdd_id

   Growing degree days at the start of the model run

   :Unit: degC d
   :Sample value: ``0``

.. index::
   single: sdd_id (YAML parameter)
   single: InitialStateGrass; sdd_id

.. option:: sdd_id

   Senescence degree days at the start of the model run

   :Unit: degC d
   :Sample value: ``0``

.. index::
   single: wu (YAML parameter)
   single: InitialStateGrass; wu

.. option:: wu

   :Sample value: ``PydanticUndefined``

   The ``wu`` parameter group is defined by the :doc:`wateruse` structure.
