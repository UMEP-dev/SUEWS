.. meta::
   :description: SUEWS YAML configuration for generic surface initial state parameters
   :keywords: SUEWS, YAML, surfaceinitialstate, parameters, configuration

.. _surfaceinitialstate:

.. index::
   single: SurfaceInitialState (YAML parameter)
   single: YAML; SurfaceInitialState

Generic Surface Initial State
=============================

Generic initial state parameters for all surface types

**Parameters:**

.. index::
   single: state (YAML parameter)
   single: SurfaceInitialState; state

.. option:: state

   Initial water state of the surface

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: soilstore (YAML parameter)
   single: SurfaceInitialState; soilstore

.. option:: soilstore

   Initial soil store (essential for QE)

   :Unit: mm
   :Sample value: ``150.0``

.. index::
   single: snowfrac (YAML parameter)
   single: SurfaceInitialState; snowfrac

.. option:: snowfrac

   Snow fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowpack (YAML parameter)
   single: SurfaceInitialState; snowpack

.. option:: snowpack

   Snow pack

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: icefrac (YAML parameter)
   single: SurfaceInitialState; icefrac

.. option:: icefrac

   Ice fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowwater (YAML parameter)
   single: SurfaceInitialState; snowwater

.. option:: snowwater

   Snow water

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: snowdens (YAML parameter)
   single: SurfaceInitialState; snowdens

.. option:: snowdens

   Snow density

   :Unit: kg |m^-3|
   :Sample value: ``0.0``

.. index::
   single: temperature (YAML parameter)
   single: SurfaceInitialState; temperature

.. option:: temperature

   Initial temperature for each thermal layer

   :Unit: degC
   :Sample value: ``[15.0, 15.0, 15.0, 15.0, 15.0]``

.. index::
   single: tsfc (YAML parameter)
   single: SurfaceInitialState; tsfc

.. option:: tsfc

   Initial exterior surface temperature

   :Unit: degC
   :Sample value: ``15.0``

.. index::
   single: tin (YAML parameter)
   single: SurfaceInitialState; tin

.. option:: tin

   Initial interior surface temperature

   :Unit: degC
   :Sample value: ``20.0``

.. index::
   single: ref (YAML parameter)
   single: SurfaceInitialState; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
