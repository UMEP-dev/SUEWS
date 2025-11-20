.. meta::
   :description: SUEWS datetime output variables
   :keywords: SUEWS, output, datetime, variables

.. _datetime_output:

.. index::
   single: datetime (output group)
   single: Output; datetime

datetime Output Variables
=========================

Date and time information for output records.

This group contains 5 output variables.

.. index::
   single: DOY (output variable)
   single: datetime; DOY

.. yaml:option:: DOY

   Day of year (1-366)

   :Aggregation: Time (timestamp only, no aggregation)

.. index::
   single: Dectime (output variable)
   single: datetime; Dectime

.. yaml:option:: Dectime

   Decimal time (fractional day of year)

   :Aggregation: Time (timestamp only, no aggregation)

.. index::
   single: Hour (output variable)
   single: datetime; Hour

.. yaml:option:: Hour

   Hour of day (0-23)

   :Aggregation: Time (timestamp only, no aggregation)

.. index::
   single: Min (output variable)
   single: datetime; Min

.. yaml:option:: Min

   Minute of hour (0-59)

   :Aggregation: Time (timestamp only, no aggregation)

.. index::
   single: Year (output variable)
   single: datetime; Year

.. yaml:option:: Year

   Year (4-digit integer)

   :Aggregation: Time (timestamp only, no aggregation)
