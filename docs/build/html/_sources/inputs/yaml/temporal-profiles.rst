:doc-source: hand-written

.. _temporal_profiles:

Temporal Profiles
=================

SUEWS (Surface Urban Energy and Water Balance Scheme) uses the following
profile containers in its YAML configuration.
The container determines the keys; the consuming calculation determines
whether values are weights, physical quantities or switches.

Keys and timing
---------------

* :doc:`config-reference/dayprofile` has two scalar keys, ``working_day``
  and ``holiday``; it is not a day-of-year series.
* :doc:`config-reference/weeklyprofile` has seven scalar keys: ``monday``,
  ``tuesday``, ``wednesday``, ``thursday``, ``friday``, ``saturday``, ``sunday``.
  supy maps them Sunday first (``sunday`` at index 0 through ``saturday`` at
  index 6) when converting to or from the legacy ``df_state`` array and
  ``SUEWS_Irrigation.txt`` columns ``DayWat(1)``--``DayWat(7)`` /
  ``DayWatPer(1)``--``DayWatPer(7)``.
  The irrigation reader applies each named weekday on its own calendar day
  (``sunday`` on Sunday through ``saturday`` on Saturday).
* :doc:`config-reference/hourlyprofile` has ``working_day`` and ``holiday``
  mappings, each with all 24 keys ``"1"`` through ``"24"``.
  Key ``"1"`` starts at 00:00 and key ``"24"`` at 23:00; the corresponding
  kernel indices are 0--23.
* :doc:`config-reference/tenminuteprofile`,
  :doc:`config-reference/heatingsetpointprofile` and
  :doc:`config-reference/coolingsetpointprofile` have the same two mappings,
  each with all 144 keys ``"1"`` through ``"144"``.
  Key ``"1"`` covers 00:00--00:10 and key ``"144"`` covers 23:50--24:00
  (start included, end excluded); kernel indices are 0--143.

For the two-day-type containers, ``working_day`` is selected Monday to Friday
and ``holiday`` on Saturday and Sunday.
This selection uses the weekday only, without a public-holiday calendar.

The hourly anthropogenic profiles interpolate linearly from the current
hour's value towards the next hour's value, wrapping from key ``"24"`` to
key ``"1"`` of the same day type.
``get_Prof_SpecTime_inst`` uses raw values;
``get_Prof_SpecTime_mean`` first divides by the mean of the 24 values.
Neither computes a timestep average.
The irrigation reader divides by the daily sum, interpolates in the same way,
then multiplies by the timestep in seconds divided by 3600.
A zero daily sum gives zero in the normalising readers.
Anthropogenic and irrigation lookups subtract the configured daylight-saving
offset from the hour, wrapping a negative hour to 23.
Snow uses the hour directly, without interpolation; STEBBS selects the
10-minute slot directly from the hour and minute, without interpolation or
normalisation.

Fields and value treatment
--------------------------

Paths below are relative to ``sites[].properties``.
``heat`` and ``co2`` abbreviate ``anthropogenic_emissions.heat`` and
``anthropogenic_emissions.co2``.
Defaults are the data-model values when the containing block is present and
the field is omitted, not recommended site parameters.
``1 / 0`` means ``working_day: 1.0`` and ``holiday: 0.0``;
other defaults apply to every entry in both day types, or all seven weekdays.
Former names are shown in brackets once.

.. list-table:: Profile-typed fields
   :header-rows: 1
   :widths: 36 17 37 10

   * - Field
     - Class
     - Use and value treatment
     - Default
   * - ``heat.qf0_beu``
     - DayProfile
     - Raw fraction allocating base anthropogenic heat to buildings.
     - 1 / 0
   * - ``heat.qf_a``
     - DayProfile
     - Raw base coefficient in the degree-day heat calculation.
     - 1 / 0
   * - ``heat.qf_b``
     - DayProfile
     - Raw cooling-degree-day coefficient.
     - 1 / 0
   * - ``heat.qf_c``
     - DayProfile
     - Raw heating-degree-day coefficient.
     - 1 / 0
   * - ``heat.baset_cooling``
     - DayProfile
     - Raw cooling base temperature (degrees Celsius).
     - 1 / 0
   * - ``heat.baset_heating``
     - DayProfile
     - Raw heating base temperature (degrees Celsius).
     - 1 / 0
   * - ``heat.ah_min``
     - DayProfile
     - Raw temperature-independent heat flux (W |m^-2|).
     - 1 / 0
   * - ``heat.ah_slope_cooling``
     - DayProfile
     - Raw heat-flux slope per degree of cooling demand.
     - 1 / 0
   * - ``heat.ah_slope_heating``
     - DayProfile
     - Raw heat-flux slope per degree of heating demand.
     - 1 / 0
   * - ``heat.popdensdaytime``
     - DayProfile
     - Raw daytime population density (people per hectare).
     - 1 / 0
   * - ``co2.emission_factor_co2_vehicle`` (``fcef_v_kgkm``)
     - DayProfile
     - Raw vehicle carbon dioxide emission factor (kg per vehicle km).
     - 1 / 0
   * - ``co2.traffic_rate`` (``trafficrate``)
     - DayProfile
     - Raw daily traffic activity; per-area or per-capita basis selected by
       ``type_traffic_rate``.
     - 1 / 0
   * - ``irrigation.daywat``
     - WeeklyProfile
     - Watering permission switch: exactly 1 enables the daily calculation.
     - 0
   * - ``irrigation.daywatper``
     - WeeklyProfile
     - Raw fraction multiplying automatic and manual daily irrigation demand.
     - 0
   * - ``heat.ahprof_24hr``
     - HourlyProfile
     - Anthropogenic heat multiplier; normalised to daily mean 1.
     - 1/24
   * - ``co2.profile_traffic_24hr`` (``traffprof_24hr``)
     - HourlyProfile
     - Traffic heat and carbon dioxide multiplier; normalised to daily mean 1.
     - 1/24
   * - ``heat.popprof_24hr``
     - HourlyProfile
     - Raw population transition in metabolic emissions and, for the
       detailed emission methods, building heat: 1=night, 2=day.
     - 1/24
   * - ``co2.profile_human_activity_24hr`` (``humactivity_24hr``)
     - HourlyProfile
     - Raw activity transition in metabolic emissions: 1=night, 2=day.
     - 1/24
   * - ``irrigation.wuprofa_24hr``
     - HourlyProfile
     - Automatic irrigation weights; normalised to daily sum 1.
     - 1/24
   * - ``irrigation.wuprofm_24hr``
     - HourlyProfile
     - Manual irrigation weights; normalised to daily sum 1.
     - 1/24
   * - ``snow.snow_profile_24hr`` (``snowprof_24hr``)
     - HourlyProfile
     - Snow-clearing switch: exactly 1 permits removal on paved/building
       surfaces at the reader's end-of-hour timestep.
     - 1/24
   * - ``building_archetype.profile_metabolism`` (``metabolism_profile``)
     - TenMinuteProfile
     - Raw metabolic power (W), also used to determine occupant activity.
     - 1/144
   * - ``building_archetype.profile_setpoint_temperature_heating_air``
       (``profile_temperature_air_heating_setpoint``)
     - HeatingSetpointProfile
     - Raw heating setpoint (degrees Celsius) for scheduled setpoints
       (``model.physics.stebbs.setpoint: 2``).
     - 0
   * - ``building_archetype.profile_setpoint_temperature_cooling_air``
       (``profile_temperature_air_cooling_setpoint``)
     - CoolingSetpointProfile
     - Raw cooling setpoint (degrees Celsius) for scheduled setpoints
       (``model.physics.stebbs.setpoint: 2``).
     - 100
   * - ``stebbs.profile_flow_hot_water`` (``profile_hot_water_flow``)
     - TenMinuteProfile
     - Raw multiplier of the hot-water supply/drain flow rate.
     - 1/144
   * - ``stebbs.profile_appliance`` (``appliance_profile``)
     - TenMinuteProfile
     - Raw appliance power (W).
     - 1/144

Defaults
--------

The asymmetric ``DayProfile`` default is ``working_day: 1.0`` and
``holiday: 0.0``; similarly, ``heat.popdensnighttime`` defaults to ``10.0``
people per hectare.
These are backward-compatibility placeholders rather than calibrated site
parameters: omitting an active ``AnthropogenicHeat`` field (or omitting
``holiday`` inside a ``DayProfile``) applies those placeholder values and
emits a ``UserWarning`` when loaded via ``SUEWSConfig.from_yaml`` or
``SUEWSConfig.from_dict`` under a non-zero ``model.physics.emissions`` method.
Specify site-specific working-day, holiday, and night-time population density
values explicitly for active emissions runs.

The uniform ``HourlyProfile`` default is 1/24 in both day types.
It becomes a constant multiplier of 1 for mean-normalised profiles and equal
hourly weights for irrigation.
It does not suit the 1-to-2 population/activity scale and never enables the
exactly-1 snow-clearing switch.
The shipped population/activity values also have a known scale mismatch;
see the `population and activity profile report
<https://github.com/UMEP-dev/SUEWS/issues/2043>`_.
Choose values for the consuming calculation: the generic 1/144 ten-minute
default is read raw, while heating and cooling setpoint defaults are 0 and
100 degrees Celsius respectively.
