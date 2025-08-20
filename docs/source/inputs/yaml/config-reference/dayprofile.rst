.. meta::
   :description: SUEWS YAML configuration for daily profile parameters
   :keywords: SUEWS, YAML, dayprofile, parameters, configuration

.. _dayprofile:

.. index::
   single: DayProfile (YAML parameter)
   single: YAML; DayProfile

Daily Profile
=============

Daily values that can vary by day of year.

**Parameters:**

.. index::
   single: working_day (YAML parameter)
   single: DayProfile; working_day

.. option:: working_day

   :Default: ``1.0``

.. index::
   single: holiday (YAML parameter)
   single: DayProfile; holiday

.. option:: holiday

   :Default: ``0.0``

.. index::
   single: ref (YAML parameter)
   single: DayProfile; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
