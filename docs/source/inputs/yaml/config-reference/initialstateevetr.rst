.. meta::
   :description: SUEWS YAML configuration for evergreen tree initial state parameters
   :keywords: SUEWS, YAML, initialstateevetr, parameters, configuration

.. _initialstateevetr:

.. index::
   single: InitialStateEvetr (YAML parameter)
   single: YAML; InitialStateEvetr

Evergreen Tree Initial State
============================

Initial state for evergreen trees with vegetation parameters.

**Parameters:**

.. index::
   single: state (YAML parameter)
   single: InitialStateEvetr; state

.. option:: state

   Initial water state of the surface

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: soilstore (YAML parameter)
   single: InitialStateEvetr; soilstore

.. option:: soilstore

   Initial soil store (essential for QE)

   :Unit: mm
   :Sample value: ``150.0``

.. index::
   single: snowfrac (YAML parameter)
   single: InitialStateEvetr; snowfrac

.. option:: snowfrac

   Snow fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowpack (YAML parameter)
   single: InitialStateEvetr; snowpack

.. option:: snowpack

   Snow pack

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: icefrac (YAML parameter)
   single: InitialStateEvetr; icefrac

.. option:: icefrac

   Ice fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowwater (YAML parameter)
   single: InitialStateEvetr; snowwater

.. option:: snowwater

   Snow water

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: snowdens (YAML parameter)
   single: InitialStateEvetr; snowdens

.. option:: snowdens

   Snow density

   :Unit: kg |m^-3|
   :Sample value: ``0.0``

.. index::
   single: temperature (YAML parameter)
   single: InitialStateEvetr; temperature

.. option:: temperature

   Initial temperature for each thermal layer

   :Unit: degC
   :Sample value: ``[15.0, 15.0, 15.0, 15.0, 15.0]``

.. index::
   single: tsfc (YAML parameter)
   single: InitialStateEvetr; tsfc

.. option:: tsfc

   Initial exterior surface temperature

   :Unit: degC
   :Sample value: ``15.0``

.. index::
   single: tin (YAML parameter)
   single: InitialStateEvetr; tin

.. option:: tin

   Initial interior surface temperature

   :Unit: degC
   :Sample value: ``20.0``

.. index::
   single: ref (YAML parameter)
   single: InitialStateEvetr; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.

.. index::
   single: alb_id (YAML parameter)
   single: InitialStateEvetr; alb_id

.. option:: alb_id

   Albedo at the start of the model run.

   :Unit: dimensionless
   :Sample value: ``0.25``

.. index::
   single: lai_id (YAML parameter)
   single: InitialStateEvetr; lai_id

.. option:: lai_id

   Leaf area index at the start of the model run.

   :Unit: |m^2| |m^-2|
   :Sample value: ``1.0``

.. index::
   single: gdd_id (YAML parameter)
   single: InitialStateEvetr; gdd_id

.. option:: gdd_id

   Growing degree days at the start of the model run

   :Unit: degC d
   :Sample value: ``0``

.. index::
   single: sdd_id (YAML parameter)
   single: InitialStateEvetr; sdd_id

.. option:: sdd_id

   Senescence degree days at the start of the model run

   :Unit: degC d
   :Sample value: ``0``

.. index::
   single: wu (YAML parameter)
   single: InitialStateEvetr; wu

.. option:: wu

   :Sample value: ``PydanticUndefined``

   The ``wu`` parameter group is defined by the :doc:`wateruse` structure.
