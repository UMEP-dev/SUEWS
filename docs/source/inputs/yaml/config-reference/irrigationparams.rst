.. meta::
   :description: SUEWS YAML configuration for irrigation parameters
   :keywords: SUEWS, YAML, irrigationparams, parameters, configuration

.. _irrigationparams:

.. index::
   single: IrrigationParams (YAML parameter)
   single: YAML; IrrigationParams

Irrigation
==========

Parameters for irrigation and water use management.

**Parameters:**

.. index::
   single: h_maintain (YAML parameter)
   single: IrrigationParams; h_maintain

.. option:: h_maintain

   Water depth to maintain through irrigation

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: faut (YAML parameter)
   single: IrrigationParams; faut

.. option:: faut

   Fraction of automatic irrigation

   :Unit: dimensionless
   :Default: ``0.0``

.. index::
   single: ie_start (YAML parameter)
   single: IrrigationParams; ie_start

.. option:: ie_start

   Start time of irrigation

   :Unit: hour
   :Default: Required - must be specified

.. index::
   single: ie_end (YAML parameter)
   single: IrrigationParams; ie_end

.. option:: ie_end

   End time of irrigation

   :Unit: hour
   :Default: Required - must be specified

.. index::
   single: internalwateruse_h (YAML parameter)
   single: IrrigationParams; internalwateruse_h

.. option:: internalwateruse_h

   Internal water use rate

   :Unit: mm |h^-1|
   :Default: Required - must be specified

.. index::
   single: daywatper (YAML parameter)
   single: IrrigationParams; daywatper

.. option:: daywatper

   :Default: ``PydanticUndefined``

   The ``daywatper`` parameter group is defined by the :doc:`weeklyprofile` structure.

.. index::
   single: daywat (YAML parameter)
   single: IrrigationParams; daywat

.. option:: daywat

   :Default: ``PydanticUndefined``

   The ``daywat`` parameter group is defined by the :doc:`weeklyprofile` structure.

.. index::
   single: wuprofa_24hr (YAML parameter)
   single: IrrigationParams; wuprofa_24hr

.. option:: wuprofa_24hr

   :Default: ``PydanticUndefined``

   The ``wuprofa_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. index::
   single: wuprofm_24hr (YAML parameter)
   single: IrrigationParams; wuprofm_24hr

.. option:: wuprofm_24hr

   :Default: ``PydanticUndefined``

   The ``wuprofm_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. index::
   single: ref (YAML parameter)
   single: IrrigationParams; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
