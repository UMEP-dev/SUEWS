.. meta::
   :description: SUEWS YAML configuration for water surface initial state parameters
   :keywords: SUEWS, YAML, initialstatewater, parameters, configuration

.. _initialstatewater:

.. index::
   single: InitialStateWater (YAML parameter)
   single: YAML; InitialStateWater

Water Surface Initial State
===========================

**Parameters:**

.. index::
   single: state (YAML parameter)
   single: InitialStateWater; state

.. option:: state

   Initial water state of the surface

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: soilstore (YAML parameter)
   single: InitialStateWater; soilstore

.. option:: soilstore

   Initial soil store (not applicable for water surfaces)

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: snowfrac (YAML parameter)
   single: InitialStateWater; snowfrac

.. option:: snowfrac

   Snow fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowpack (YAML parameter)
   single: InitialStateWater; snowpack

.. option:: snowpack

   Snow pack

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: icefrac (YAML parameter)
   single: InitialStateWater; icefrac

.. option:: icefrac

   Ice fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowwater (YAML parameter)
   single: InitialStateWater; snowwater

.. option:: snowwater

   Snow water

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: snowdens (YAML parameter)
   single: InitialStateWater; snowdens

.. option:: snowdens

   Snow density

   :Unit: kg |m^-3|
   :Sample value: ``0.0``

.. index::
   single: temperature (YAML parameter)
   single: InitialStateWater; temperature

.. option:: temperature

   Initial temperature for each thermal layer

   :Unit: degC
   :Sample value: ``[15.0, 15.0, 15.0, 15.0, 15.0]``

.. index::
   single: tsfc (YAML parameter)
   single: InitialStateWater; tsfc

.. option:: tsfc

   Initial exterior surface temperature

   :Unit: degC
   :Sample value: ``15.0``

.. index::
   single: tin (YAML parameter)
   single: InitialStateWater; tin

.. option:: tin

   Initial interior surface temperature

   :Unit: degC
   :Sample value: ``20.0``

.. index::
   single: ref (YAML parameter)
   single: InitialStateWater; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
