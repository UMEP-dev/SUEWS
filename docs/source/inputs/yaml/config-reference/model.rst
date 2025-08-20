.. meta::
   :description: SUEWS YAML configuration for model configuration parameters
   :keywords: SUEWS, YAML, model, parameters, configuration

.. _model:

.. index::
   single: Model (YAML parameter)
   single: YAML; Model

Model Configuration
===================

**Parameters:**

.. index::
   single: control (YAML parameter)
   single: Model; control

.. option:: control

   Model control parameters including timestep, output options, etc.

   :Default: ``PydanticUndefined``

   The ``control`` parameter group is defined by the :doc:`modelcontrol` structure.

.. index::
   single: physics (YAML parameter)
   single: Model; physics

.. option:: physics

   Model physics parameters including surface properties, coefficients, etc.

   :Default: ``PydanticUndefined``

   The ``physics`` parameter group is defined by the :doc:`modelphysics` structure.
