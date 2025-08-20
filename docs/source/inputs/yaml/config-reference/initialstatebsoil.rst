.. meta::
   :description: SUEWS YAML configuration for bare soil surface initial state parameters
   :keywords: SUEWS, YAML, initialstatebsoil, parameters, configuration

.. _initialstatebsoil:

.. index::
   single: InitialStateBsoil (YAML parameter)
   single: YAML; InitialStateBsoil

Bare Soil Surface Initial State
===============================

**Parameters:**

.. index::
   single: state (YAML parameter)
   single: InitialStateBsoil; state

.. option:: state

   Initial water state of the surface

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: soilstore (YAML parameter)
   single: InitialStateBsoil; soilstore

.. option:: soilstore

   Initial soil store (essential for QE)

   :Unit: mm
   :Sample value: ``150.0``

.. index::
   single: snowfrac (YAML parameter)
   single: InitialStateBsoil; snowfrac

.. option:: snowfrac

   Snow fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowpack (YAML parameter)
   single: InitialStateBsoil; snowpack

.. option:: snowpack

   Snow pack

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: icefrac (YAML parameter)
   single: InitialStateBsoil; icefrac

.. option:: icefrac

   Ice fraction

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: snowwater (YAML parameter)
   single: InitialStateBsoil; snowwater

.. option:: snowwater

   Snow water

   :Unit: mm
   :Sample value: ``0.0``

.. index::
   single: snowdens (YAML parameter)
   single: InitialStateBsoil; snowdens

.. option:: snowdens

   Snow density

   :Unit: kg |m^-3|
   :Sample value: ``0.0``

.. index::
   single: temperature (YAML parameter)
   single: InitialStateBsoil; temperature

.. option:: temperature

   Initial temperature for each thermal layer

   :Unit: degC
   :Sample value: ``[15.0, 15.0, 15.0, 15.0, 15.0]``

.. index::
   single: tsfc (YAML parameter)
   single: InitialStateBsoil; tsfc

.. option:: tsfc

   Initial exterior surface temperature

   :Unit: degC
   :Sample value: ``15.0``

.. index::
   single: tin (YAML parameter)
   single: InitialStateBsoil; tin

.. option:: tin

   Initial interior surface temperature

   :Unit: degC
   :Sample value: ``20.0``

.. index::
   single: ref (YAML parameter)
   single: InitialStateBsoil; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
