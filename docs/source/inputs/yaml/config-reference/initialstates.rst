.. meta::
   :description: SUEWS YAML configuration for initial states parameters
   :keywords: SUEWS, YAML, initialstates, parameters, configuration

.. _initialstates:

.. index::
   single: InitialStates (YAML parameter)
   single: YAML; InitialStates

Initial States
==============

Initial conditions for the SUEWS model

**Parameters:**

.. index::
   single: snowalb (YAML parameter)
   single: InitialStates; snowalb

.. option:: snowalb

   Snow albedo at the start of the model run

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: paved (YAML parameter)
   single: InitialStates; paved

.. option:: paved

   Initial states for paved surfaces

   :Sample value: ``PydanticUndefined``

   For ``paved``, one generic SurfaceInitialState object is used to specify initial conditions - see :doc:`surfaceinitialstate` for details.

.. index::
   single: bldgs (YAML parameter)
   single: InitialStates; bldgs

.. option:: bldgs

   Initial states for building surfaces

   :Sample value: ``PydanticUndefined``

   For ``bldgs``, one generic SurfaceInitialState object is used to specify initial conditions - see :doc:`surfaceinitialstate` for details.

.. index::
   single: evetr (YAML parameter)
   single: InitialStates; evetr

.. option:: evetr

   Initial states for evergreen tree surfaces

   :Sample value: ``PydanticUndefined``

   For ``evetr``, one vegetation-specific initial state with additional parameters is used - see :doc:`initialstateevetr` for details.

.. index::
   single: dectr (YAML parameter)
   single: InitialStates; dectr

.. option:: dectr

   Initial states for deciduous tree surfaces

   :Sample value: ``PydanticUndefined``

   For ``dectr``, one vegetation-specific initial state with additional parameters is used - see :doc:`initialstatedectr` for details.

.. index::
   single: grass (YAML parameter)
   single: InitialStates; grass

.. option:: grass

   Initial states for grass surfaces

   :Sample value: ``PydanticUndefined``

   For ``grass``, one vegetation-specific initial state with additional parameters is used - see :doc:`initialstategrass` for details.

.. index::
   single: bsoil (YAML parameter)
   single: InitialStates; bsoil

.. option:: bsoil

   Initial states for bare soil surfaces

   :Sample value: ``PydanticUndefined``

   For ``bsoil``, one generic SurfaceInitialState object is used to specify initial conditions - see :doc:`surfaceinitialstate` for details.

.. index::
   single: water (YAML parameter)
   single: InitialStates; water

.. option:: water

   Initial states for water surfaces

   :Sample value: ``PydanticUndefined``

   For ``water``, one generic SurfaceInitialState object is used to specify initial conditions - see :doc:`surfaceinitialstate` for details.

.. index::
   single: roofs (YAML parameter)
   single: InitialStates; roofs

.. option:: roofs

   Initial states for roof layers

   :Sample value: List of 3 SurfaceInitialState objects

   The ``roofs`` parameter group is defined by the :doc:`surfaceinitialstate` structure.

.. index::
   single: walls (YAML parameter)
   single: InitialStates; walls

.. option:: walls

   Initial states for wall layers

   :Sample value: List of 3 SurfaceInitialState objects

   The ``walls`` parameter group is defined by the :doc:`surfaceinitialstate` structure.
