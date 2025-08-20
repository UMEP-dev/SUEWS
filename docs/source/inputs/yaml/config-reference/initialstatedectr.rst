.. meta::
   :description: SUEWS YAML configuration for deciduous tree initial state parameters
   :keywords: SUEWS, YAML, initialstatedectr, parameters, configuration

.. _initialstatedectr:

.. index::
   single: InitialStateDectr (YAML parameter)
   single: YAML; InitialStateDectr

Deciduous Tree Initial State
============================

Initial state for deciduous trees with vegetation parameters plus porosity.

**Parameters:**

.. index::
   single: state (YAML parameter)
   single: InitialStateDectr; state

.. option:: state

   Initial water state of the surface

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: soilstore (YAML parameter)
   single: InitialStateDectr; soilstore

.. option:: soilstore

   Initial soil store (essential for QE)

   :Unit: mm
   :Sample value: ``150.0``

.. index::
   single: snowfrac (YAML parameter)
   single: InitialStateDectr; snowfrac

.. option:: snowfrac

   Snow fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowpack (YAML parameter)
   single: InitialStateDectr; snowpack

.. option:: snowpack

   Snow pack

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: icefrac (YAML parameter)
   single: InitialStateDectr; icefrac

.. option:: icefrac

   Ice fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowwater (YAML parameter)
   single: InitialStateDectr; snowwater

.. option:: snowwater

   Snow water

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: snowdens (YAML parameter)
   single: InitialStateDectr; snowdens

.. option:: snowdens

   Snow density

   :Unit: kg |m^-3|
   :Sample value: ``0.0``

.. index::
   single: temperature (YAML parameter)
   single: InitialStateDectr; temperature

.. option:: temperature

   Initial temperature for each thermal layer

   :Unit: degC
   :Sample value: ``[15.0, 15.0, 15.0, 15.0, 15.0]``

.. index::
   single: tsfc (YAML parameter)
   single: InitialStateDectr; tsfc

.. option:: tsfc

   Initial exterior surface temperature

   :Unit: degC
   :Sample value: ``15.0``

.. index::
   single: tin (YAML parameter)
   single: InitialStateDectr; tin

.. option:: tin

   Initial interior surface temperature

   :Unit: degC
   :Sample value: ``20.0``

.. index::
   single: ref (YAML parameter)
   single: InitialStateDectr; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.

.. index::
   single: alb_id (YAML parameter)
   single: InitialStateDectr; alb_id

.. option:: alb_id

   Albedo at the start of the model run.

   :Unit: dimensionless
   :Sample value: ``0.25``

.. index::
   single: lai_id (YAML parameter)
   single: InitialStateDectr; lai_id

.. option:: lai_id

   Leaf area index at the start of the model run.

   :Unit: |m^2| |m^-2|
   :Sample value: ``1.0``

.. index::
   single: gdd_id (YAML parameter)
   single: InitialStateDectr; gdd_id

.. option:: gdd_id

   Growing degree days at the start of the model run

   :Unit: degC d
   :Sample value: ``0``

.. index::
   single: sdd_id (YAML parameter)
   single: InitialStateDectr; sdd_id

.. option:: sdd_id

   Senescence degree days at the start of the model run

   :Unit: degC d
   :Sample value: ``0``

.. index::
   single: wu (YAML parameter)
   single: InitialStateDectr; wu

.. option:: wu

   :Sample value: ``PydanticUndefined``

   The ``wu`` parameter group is defined by the :doc:`wateruse` structure.

.. index::
   single: porosity_id (YAML parameter)
   single: InitialStateDectr; porosity_id

.. option:: porosity_id

   Porosity for deciduous trees at the start of the model run

   :Unit: dimensionless
   :Sample value: ``0.2``

.. index::
   single: decidcap_id (YAML parameter)
   single: InitialStateDectr; decidcap_id

.. option:: decidcap_id

   Deciduous capacity for deciduous trees at the start of the model run

   :Unit: mm
   :Sample value: ``0.3``
