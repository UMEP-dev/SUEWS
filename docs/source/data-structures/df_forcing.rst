:orphan:

.. _df_forcing_var:

Forcing variable reference
==========================

This reference is generated from forcing contract ``1.2.0``.
It describes the ``df_forcing`` columns supplied in an external forcing file.
For file layout and preparation, see :doc:`/inputs/forcing-data`; for the loaded
Python object, see :doc:`/api/io-data-structures`.

How to read this page
---------------------

All units and valid ranges describe values in the external forcing file.
Use each canonical option name as the file header.

File timestamps identify the end of each forcing interval. The default
reference is the site's fixed-offset local standard time; UTC is accepted
when ``model.control.forcing.timestamp_reference`` is ``utc``. Model output
follows the declared forcing clock. Daylight-saving civil time is unsupported.

The timestamp labels the interval; it is not an instantaneous sampling time.
Weather, radiation, and energy-flux values are means over the interval
ending at that timestamp. Rainfall and external water use are totals
accumulated over the same interval. State inputs such as LAI, snow cover,
and soil moisture apply at the interval end.

Every row needs valid values for always-required columns and for any
physics-conditional columns selected by the table below. Use ``-999`` only
for optional or inactive columns. Land-cover-specific columns use their named
bulk fallback only when that land-cover column is absent, not when it contains
``-999``.

.. _df_forcing_requirements:

Physics-conditional requirements
--------------------------------

Each row below is active only when all listed selector conditions match. Any
one complete alternative satisfies the rule; columns joined by ``+`` are
jointly required. Selector definitions are in :ref:`modelphysics`.

.. list-table::
   :header-rows: 1
   :widths: 45 55

   * - Active when
     - Valid forcing alternative
   * - ``net_radiation`` in [0]
     - ``qn``
   * - ``net_radiation`` in [1, 11, 100, 1001]
     - ``ldown``
   * - ``net_radiation`` in [2, 12, 200, 1002]
     - (``kdown`` + ``fcld``)
   * - ``net_radiation`` in [3, 13, 300, 1003]
     - ``kdown``
   * - ``storage_heat`` in [0]
     - ``qs``
   * - ``emissions`` in [0]
     - ``qf``
   * - ``soil_moisture_deficit`` in [1, 2]
     - ``xsmd``
   * - ``laimethod`` in [0]
     - ``lai`` OR (``lai_evetr`` + ``lai_dectr`` + ``lai_grass``)
   * - ``water_use`` in [1]
     - ``Wuh`` OR (``wuh_paved`` + ``wuh_bldgs`` + ``wuh_evetr`` + ``wuh_dectr`` + ``wuh_grass`` + ``wuh_bsoil`` + ``wuh_water``)
   * - ``snow_use`` in [1] and ``net_radiation`` in [0]
     - ``snow``

Timestamp columns
-----------------

Every forcing row must include these coordinates for the declared timestamp reference.

.. option:: iy

   :Description: Calendar year of the interval-end timestamp
   :Input unit: dimensionless
   :Interval basis: component of the interval-end timestamp
   :Required: always
   :Missing values: not allowed
   :Valid input range: integer calendar coordinate; calendar validity checked on load

.. option:: id

   :Description: Day of year of the interval-end timestamp
   :Input unit: dimensionless
   :Interval basis: component of the interval-end timestamp
   :Required: always
   :Missing values: not allowed
   :Valid input range: integer calendar coordinate; calendar validity checked on load

.. option:: it

   :Description: Hour component of the interval-end timestamp
   :Input unit: h
   :Interval basis: component of the interval-end timestamp
   :Required: always
   :Missing values: not allowed
   :Valid input range: integer calendar coordinate; calendar validity checked on load

.. option:: imin

   :Description: Minute component of the interval-end timestamp
   :Input unit: min
   :Interval basis: component of the interval-end timestamp
   :Required: always
   :Missing values: not allowed
   :Valid input range: integer calendar coordinate; calendar validity checked on load

Always-required weather inputs
------------------------------

Every forcing row must contain valid values for these variables.

.. option:: U

   :Description: Wind speed at the forcing measurement height
   :Input unit: m |s^-1|
   :Interval basis: mean over the forcing interval
   :Required: always
   :Missing values: not allowed
   :Valid input range: 0.01 to 60 (inclusive)

.. option:: RH

   :Description: Relative humidity
   :Input unit: %
   :Interval basis: mean over the forcing interval
   :Required: always
   :Missing values: not allowed
   :Valid input range: 0.0001 to 105 (inclusive)

.. option:: Tair

   :Description: Air temperature
   :Input unit: :math:`{}^{\circ}\mathrm{C}`
   :Interval basis: mean over the forcing interval
   :Required: always
   :Missing values: not allowed
   :Valid input range: -60 to 90 (inclusive)

.. option:: pres

   :Description: Surface air pressure
   :Input unit: kPa
   :Interval basis: mean over the forcing interval
   :Required: always
   :Missing values: not allowed
   :Valid input range: 68 to 130 (inclusive)

.. option:: rain

   :Description: Precipitation accumulated over the forcing interval
   :Input unit: mm
   :Interval basis: total accumulated over the forcing interval
   :Required: always
   :Missing values: not allowed
   :Valid input range: :math:`\geq 0`

.. option:: kdown

   :Description: Incoming short-wave radiation averaged over the forcing interval
   :Input unit: W |m^-2|
   :Interval basis: mean over the forcing interval
   :Required: always
   :Missing values: not allowed
   :Valid input range: 0 to 1400 (inclusive)

Physics-conditional inputs
--------------------------

These are required only when selected by the requirements table above.

.. option:: qn

   :Description: Observed net all-wave radiation
   :Input unit: W |m^-2|
   :Interval basis: mean over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: -500 to 1300 (inclusive)

.. option:: qs

   :Description: Observed net storage heat flux
   :Input unit: W |m^-2|
   :Interval basis: mean over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: -100 to 650 (inclusive)

.. option:: qf

   :Description: Observed anthropogenic heat flux
   :Input unit: W |m^-2|
   :Interval basis: mean over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: -500 to 1300 (inclusive)

.. option:: snow

   :Description: Observed surface snow-cover fraction
   :Input unit: dimensionless
   :Interval basis: state at the interval-end timestamp
   :Required: only for selected physics; see the requirements above
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: 0 to 1 (inclusive)

.. option:: ldown

   :Description: Incoming long-wave radiation averaged over the forcing interval
   :Input unit: W |m^-2|
   :Interval basis: mean over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: 100 to 600 (inclusive)

.. option:: fcld

   :Description: Cloud fraction
   :Input unit: dimensionless
   :Interval basis: mean over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: 0 to 1 (inclusive)

.. option:: Wuh

   :Description: Bulk external water use accumulated over the forcing interval
   :Input unit: mm
   :Interval basis: total accumulated over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: :math:`\geq 0`

.. option:: xsmd

   :Description: Observed soil-moisture deficit input
   :Input unit: ``soil_moisture_deficit=1``: |m^3| |m^-3|; ``soil_moisture_deficit=2``: kg |kg^-1|
   :Interval basis: state at the interval-end timestamp
   :Required: only for selected physics; see the requirements above
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: :math:`\geq 0`

.. option:: lai

   :Description: Bulk observed leaf area index used as a surface fallback
   :Input unit: |m^2| |m^-2|
   :Interval basis: state at the interval-end timestamp
   :Required: only for selected physics; see the requirements above
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: :math:`\geq 0`

Optional accepted columns
-------------------------

These columns are accepted but are never required by SUEWS.

.. option:: qh

   :Description: Reserved observed sensible heat-flux column; not currently consumed
   :Input unit: W |m^-2|
   :Interval basis: mean over the forcing interval
   :Required: no
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: -500 to 1300 (inclusive)

.. option:: qe

   :Description: Reserved observed latent heat-flux column; not currently consumed
   :Input unit: W |m^-2|
   :Interval basis: mean over the forcing interval
   :Required: no
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: -500 to 1300 (inclusive)

.. option:: kdiff

   :Description: Diffuse component of incoming short-wave radiation
   :Input unit: W |m^-2|
   :Interval basis: mean over the forcing interval
   :Required: no
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: 0 to 1000 (inclusive)

.. option:: kdir

   :Description: Direct component of incoming short-wave radiation
   :Input unit: W |m^-2|
   :Interval basis: mean over the forcing interval
   :Required: no
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: 0 to 1400 (inclusive)

.. option:: wdir

   :Description: Accepted wind-direction column; not currently consumed
   :Input unit: :math:`{}^{\circ}`
   :Interval basis: state at the interval-end timestamp
   :Required: no
   :Missing values: ``-999`` only while this column is optional or inactive
   :Valid input range: 0 to 360 (inclusive)

Land-cover-specific alternatives
--------------------------------

These may replace the corresponding bulk LAI or water-use column.

.. option:: lai_evetr

   :Description: Observed leaf area index for evergreen trees
   :Input unit: |m^2| |m^-2|
   :Interval basis: state at the interval-end timestamp
   :Required: only for selected physics; see the requirements above
   :Missing values: use ``lai`` only when this column is absent; an explicit ``-999`` remains missing
   :Valid input range: :math:`\geq 0`

.. option:: lai_dectr

   :Description: Observed leaf area index for deciduous trees
   :Input unit: |m^2| |m^-2|
   :Interval basis: state at the interval-end timestamp
   :Required: only for selected physics; see the requirements above
   :Missing values: use ``lai`` only when this column is absent; an explicit ``-999`` remains missing
   :Valid input range: :math:`\geq 0`

.. option:: lai_grass

   :Description: Observed leaf area index for grass
   :Input unit: |m^2| |m^-2|
   :Interval basis: state at the interval-end timestamp
   :Required: only for selected physics; see the requirements above
   :Missing values: use ``lai`` only when this column is absent; an explicit ``-999`` remains missing
   :Valid input range: :math:`\geq 0`

.. option:: wuh_paved

   :Description: External water-use depth for the paved surface over the forcing interval
   :Input unit: mm
   :Interval basis: total accumulated over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: use ``Wuh`` only when this column is absent; an explicit ``-999`` remains missing
   :Valid input range: :math:`\geq 0`

.. option:: wuh_bldgs

   :Description: External water-use depth for the building surface over the forcing interval
   :Input unit: mm
   :Interval basis: total accumulated over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: use ``Wuh`` only when this column is absent; an explicit ``-999`` remains missing
   :Valid input range: :math:`\geq 0`

.. option:: wuh_evetr

   :Description: External water-use depth for evergreen trees over the forcing interval
   :Input unit: mm
   :Interval basis: total accumulated over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: use ``Wuh`` only when this column is absent; an explicit ``-999`` remains missing
   :Valid input range: :math:`\geq 0`

.. option:: wuh_dectr

   :Description: External water-use depth for deciduous trees over the forcing interval
   :Input unit: mm
   :Interval basis: total accumulated over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: use ``Wuh`` only when this column is absent; an explicit ``-999`` remains missing
   :Valid input range: :math:`\geq 0`

.. option:: wuh_grass

   :Description: External water-use depth for grass over the forcing interval
   :Input unit: mm
   :Interval basis: total accumulated over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: use ``Wuh`` only when this column is absent; an explicit ``-999`` remains missing
   :Valid input range: :math:`\geq 0`

.. option:: wuh_bsoil

   :Description: External water-use depth for bare soil over the forcing interval
   :Input unit: mm
   :Interval basis: total accumulated over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: use ``Wuh`` only when this column is absent; an explicit ``-999`` remains missing
   :Valid input range: :math:`\geq 0`

.. option:: wuh_water

   :Description: External water-use depth for open water over the forcing interval
   :Input unit: mm
   :Interval basis: total accumulated over the forcing interval
   :Required: only for selected physics; see the requirements above
   :Missing values: use ``Wuh`` only when this column is absent; an explicit ``-999`` remains missing
   :Valid input range: :math:`\geq 0`
