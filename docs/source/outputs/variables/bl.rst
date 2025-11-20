.. meta::
   :description: SUEWS BL output variables
   :keywords: SUEWS, output, bl, variables

.. _bl_output:

.. index::
   single: BL (output group)
   single: Output; BL

BL Output Variables
===================

Boundary Layer model outputs.

This group contains 17 output variables.

.. index::
   single: Press_hPa (output variable)
   single: BL; Press_hPa

.. yaml:option:: Press_hPa

   Pressure

   :Unit: hPa
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: QE_use (output variable)
   single: BL; QE_use

.. yaml:option:: QE_use

   Latent heat flux used

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: QH_use (output variable)
   single: BL; QH_use

.. yaml:option:: QH_use

   Sensible heat flux used

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: Temp_C (output variable)
   single: BL; Temp_C

.. yaml:option:: Temp_C

   Temperature in Celsius

   :Unit: degC
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: UStar (output variable)
   single: BL; UStar

.. yaml:option:: UStar

   Friction velocity

   :Unit: m |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: avcp (output variable)
   single: BL; avcp

.. yaml:option:: avcp

   Average specific heat capacity

   :Unit: J |kg^-1| |K^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: avdens (output variable)
   single: BL; avdens

.. yaml:option:: avdens

   Average density

   :Unit: kg |m^-3|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: avu1 (output variable)
   single: BL; avu1

.. yaml:option:: avu1

   Average wind speed

   :Unit: m |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: gamq (output variable)
   single: BL; gamq

.. yaml:option:: gamq

   Humidity lapse rate

   :Unit: kg |kg^-1| |m^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: gamt (output variable)
   single: BL; gamt

.. yaml:option:: gamt

   Temperature lapse rate

   :Unit: K |m^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: lv_J_kg (output variable)
   single: BL; lv_J_kg

.. yaml:option:: lv_J_kg

   Latent heat of vaporisation

   :Unit: J |kg^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f146``

.. index::
   single: q (output variable)
   single: BL; q

.. yaml:option:: q

   Specific humidity

   :Unit: g |kg^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: q+ (output variable)
   single: BL; q+

.. yaml:option:: q+

   Humidity excess

   :Unit: g |kg^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: rh (output variable)
   single: BL; rh

.. yaml:option:: rh

   Relative humidity

   :Unit: %
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: theta (output variable)
   single: BL; theta

.. yaml:option:: theta

   Potential temperature

   :Unit: K
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: theta+ (output variable)
   single: BL; theta+

.. yaml:option:: theta+

   Temperature excess

   :Unit: K
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: z (output variable)
   single: BL; z

.. yaml:option:: z

   Height

   :Unit: m
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
