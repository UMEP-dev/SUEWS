:orphan:

.. _df_forcing_var:

``df_forcing`` variables
========================

This reference is generated from the published forcing registry.
File timestamps use the site's fixed-offset local standard time and
identify the end of each forcing interval. Daylight-saving transitions
are not part of this timestamp convention.

File-header aliases and programmatic accessor aliases are separate
namespaces. The latter are not accepted as forcing-file headers.

.. option:: iy

   :Description: Calendar year of the interval-end timestamp
   :Input unit: 1
   :Role: coordinate
   :Temporal semantics: time
   :Requiredness: baseline
   :Missing-value policy: forbidden
   :Enforced input range: not range-checked
   :Legacy position: 1

.. option:: id

   :Description: Day of year of the interval-end timestamp
   :Input unit: 1
   :Role: coordinate
   :Temporal semantics: time
   :Requiredness: baseline
   :Missing-value policy: forbidden
   :Enforced input range: not range-checked
   :Legacy position: 2

.. option:: it

   :Description: Hour component of the interval-end timestamp
   :Input unit: h
   :Role: coordinate
   :Temporal semantics: time
   :Requiredness: baseline
   :Missing-value policy: forbidden
   :Enforced input range: not range-checked
   :Legacy position: 3

.. option:: imin

   :Description: Minute component of the interval-end timestamp
   :Input unit: min
   :Role: coordinate
   :Temporal semantics: time
   :Requiredness: baseline
   :Missing-value policy: forbidden
   :Enforced input range: not range-checked
   :Legacy position: 4

.. option:: qn

   :Description: Observed net all-wave radiation
   :Input unit: W m-2
   :Role: observation
   :Temporal semantics: avg
   :Requiredness: conditional
   :Missing-value policy: sentinel
   :Enforced input range: -500.0 to 1300.0
   :Legacy position: 5
   :File aliases: qn1_obs
   :Accessor aliases: net_radiation, qstar, q_star
   :Active requirement: net_radiation in [0]

.. option:: qh

   :Description: Reserved observed sensible heat-flux column; not currently consumed
   :Input unit: W m-2
   :Role: reserved
   :Temporal semantics: avg
   :Requiredness: optional
   :Missing-value policy: sentinel
   :Enforced input range: -500.0 to 1300.0
   :Legacy position: 6
   :Accessor aliases: sensible_heat, h

.. option:: qe

   :Description: Reserved observed latent heat-flux column; not currently consumed
   :Input unit: W m-2
   :Role: reserved
   :Temporal semantics: avg
   :Requiredness: optional
   :Missing-value policy: sentinel
   :Enforced input range: -500.0 to 1300.0
   :Legacy position: 7
   :Accessor aliases: latent_heat, le

.. option:: qs

   :Description: Observed net storage heat flux
   :Input unit: W m-2
   :Role: observation
   :Temporal semantics: avg
   :Requiredness: conditional
   :Missing-value policy: sentinel
   :Enforced input range: -100.0 to 650.0
   :Legacy position: 8
   :File aliases: qs_obs
   :Accessor aliases: storage_heat
   :Active requirement: storage_heat in [0]

.. option:: qf

   :Description: Observed anthropogenic heat flux
   :Input unit: W m-2
   :Role: observation
   :Temporal semantics: avg
   :Requiredness: conditional
   :Missing-value policy: sentinel
   :Enforced input range: -500.0 to 1300.0
   :Legacy position: 9
   :File aliases: qf_obs
   :Accessor aliases: anthropogenic_heat
   :Active requirement: emissions in [0]

.. option:: U

   :Description: Wind speed at the forcing measurement height
   :Input unit: m s-1
   :Role: driver
   :Temporal semantics: inst
   :Requiredness: baseline
   :Missing-value policy: forbidden
   :Enforced input range: 0.01 to 60.0
   :Legacy position: 10
   :Accessor aliases: wind_speed, wind, u

.. option:: RH

   :Description: Relative humidity
   :Input unit: %
   :Role: driver
   :Temporal semantics: inst
   :Requiredness: baseline
   :Missing-value policy: forbidden
   :Enforced input range: 0.0001 to 105.0
   :Legacy position: 11
   :Accessor aliases: relative_humidity, humidity, rh

.. option:: Tair

   :Description: Air temperature
   :Input unit: degC
   :Role: driver
   :Temporal semantics: inst
   :Requiredness: baseline
   :Missing-value policy: forbidden
   :Enforced input range: -60.0 to 90.0
   :Legacy position: 12
   :File aliases: temp_c
   :Accessor aliases: temperature, air_temperature, temp, t_air, ta

.. option:: pres

   :Description: Surface air pressure
   :Input unit: kPa
   :Role: driver
   :Temporal semantics: inst
   :Requiredness: baseline
   :Missing-value policy: forbidden
   :Enforced input range: 68.0 to 130.0
   :Legacy position: 13
   :Accessor aliases: pressure, air_pressure, p

.. option:: rain

   :Description: Precipitation accumulated over the forcing interval
   :Input unit: mm
   :Role: driver
   :Temporal semantics: sum
   :Requiredness: baseline
   :Missing-value policy: forbidden
   :Enforced input range: >= 0.0
   :Legacy position: 14
   :Accessor aliases: precipitation, rainfall, precip

.. option:: kdown

   :Description: Incoming short-wave radiation averaged over the forcing interval
   :Input unit: W m-2
   :Role: driver
   :Temporal semantics: avg
   :Requiredness: baseline
   :Missing-value policy: forbidden
   :Enforced input range: 0.0 to 1400.0
   :Legacy position: 15
   :Accessor aliases: shortwave_down, solar_radiation, sw_down, k_down
   :Active requirement: net_radiation in [2, 12, 200, 1002]; or net_radiation in [3, 13, 300, 1003]

.. option:: snow

   :Description: Observed surface snow-cover fraction
   :Input unit: 1
   :Role: driver
   :Temporal semantics: inst
   :Requiredness: optional
   :Missing-value policy: sentinel
   :Enforced input range: 0.0 to 1.0
   :Legacy position: 16
   :File aliases: snowfrac
   :Accessor aliases: snowfall
   :Active requirement: snow_use in [1] and net_radiation in [0]

.. option:: ldown

   :Description: Incoming long-wave radiation averaged over the forcing interval
   :Input unit: W m-2
   :Role: driver
   :Temporal semantics: avg
   :Requiredness: conditional
   :Missing-value policy: sentinel
   :Enforced input range: 100.0 to 600.0
   :Legacy position: 17
   :Accessor aliases: longwave_down, lw_down, l_down
   :Active requirement: net_radiation in [1, 11, 100, 1001]

.. option:: fcld

   :Description: Cloud fraction
   :Input unit: 1
   :Role: driver
   :Temporal semantics: inst
   :Requiredness: conditional
   :Missing-value policy: sentinel
   :Enforced input range: 0.0 to 1.0
   :Legacy position: 18
   :Accessor aliases: cloud_fraction, cloud_cover, clouds
   :Active requirement: net_radiation in [2, 12, 200, 1002]

.. option:: Wuh

   :Description: Bulk external water use accumulated over the forcing interval
   :Input unit: mm
   :Role: driver
   :Temporal semantics: sum
   :Requiredness: conditional
   :Missing-value policy: sentinel
   :Enforced input range: >= 0.0
   :Legacy position: 19
   :File aliases: wu_mm
   :Accessor aliases: water_use, external_water, wu_mm
   :Active requirement: water_use in [1]

.. option:: xsmd

   :Description: Observed soil-moisture deficit input
   :Input unit: soil_moisture_deficit=1: m3 m-3; soil_moisture_deficit=2: kg kg-1
   :Role: observation
   :Temporal semantics: inst
   :Requiredness: conditional
   :Missing-value policy: sentinel
   :Enforced input range: >= 0.0
   :Legacy position: 20
   :Accessor aliases: soil_moisture, smd
   :Active requirement: soil_moisture_deficit in [1, 2]

.. option:: lai

   :Description: Bulk observed leaf area index used as a surface fallback
   :Input unit: m2 m-2
   :Role: observation
   :Temporal semantics: inst
   :Requiredness: conditional
   :Missing-value policy: sentinel
   :Enforced input range: >= 0.0
   :Legacy position: 21
   :Accessor aliases: leaf_area_index
   :Active requirement: laimethod in [0]

.. option:: kdiff

   :Description: Diffuse component of incoming short-wave radiation
   :Input unit: W m-2
   :Role: driver
   :Temporal semantics: avg
   :Requiredness: optional
   :Missing-value policy: sentinel
   :Enforced input range: 0.0 to 1000.0
   :Legacy position: 22
   :Accessor aliases: diffuse_radiation

.. option:: kdir

   :Description: Direct component of incoming short-wave radiation
   :Input unit: W m-2
   :Role: driver
   :Temporal semantics: avg
   :Requiredness: optional
   :Missing-value policy: sentinel
   :Enforced input range: 0.0 to 1400.0
   :Legacy position: 23
   :Accessor aliases: direct_radiation

.. option:: wdir

   :Description: Accepted wind-direction column; not currently consumed
   :Input unit: degree
   :Role: reserved
   :Temporal semantics: inst
   :Requiredness: optional
   :Missing-value policy: sentinel
   :Enforced input range: 0.0 to 360.0
   :Legacy position: 24
   :Accessor aliases: wind_direction, wd

.. option:: lai_evetr

   :Description: Observed leaf area index for evergreen trees
   :Input unit: m2 m-2
   :Role: observation
   :Temporal semantics: inst
   :Requiredness: conditional
   :Missing-value policy: fallback
   :Enforced input range: >= 0.0
   :Accessor aliases: leaf_area_index_evetr
   :Fallback column: lai
   :Active requirement: laimethod in [0]

.. option:: lai_dectr

   :Description: Observed leaf area index for deciduous trees
   :Input unit: m2 m-2
   :Role: observation
   :Temporal semantics: inst
   :Requiredness: conditional
   :Missing-value policy: fallback
   :Enforced input range: >= 0.0
   :Accessor aliases: leaf_area_index_dectr
   :Fallback column: lai
   :Active requirement: laimethod in [0]

.. option:: lai_grass

   :Description: Observed leaf area index for grass
   :Input unit: m2 m-2
   :Role: observation
   :Temporal semantics: inst
   :Requiredness: conditional
   :Missing-value policy: fallback
   :Enforced input range: >= 0.0
   :Accessor aliases: leaf_area_index_grass
   :Fallback column: lai
   :Active requirement: laimethod in [0]

.. option:: wuh_paved

   :Description: External water-use depth for the paved surface over the forcing interval
   :Input unit: mm
   :Role: driver
   :Temporal semantics: sum
   :Requiredness: conditional
   :Missing-value policy: fallback
   :Enforced input range: >= 0.0
   :File aliases: wu_mm_paved
   :Fallback column: Wuh
   :Active requirement: water_use in [1]

.. option:: wuh_bldgs

   :Description: External water-use depth for the building surface over the forcing interval
   :Input unit: mm
   :Role: driver
   :Temporal semantics: sum
   :Requiredness: conditional
   :Missing-value policy: fallback
   :Enforced input range: >= 0.0
   :File aliases: wu_mm_bldgs
   :Fallback column: Wuh
   :Active requirement: water_use in [1]

.. option:: wuh_evetr

   :Description: External water-use depth for evergreen trees over the forcing interval
   :Input unit: mm
   :Role: driver
   :Temporal semantics: sum
   :Requiredness: conditional
   :Missing-value policy: fallback
   :Enforced input range: >= 0.0
   :File aliases: wu_mm_evetr
   :Fallback column: Wuh
   :Active requirement: water_use in [1]

.. option:: wuh_dectr

   :Description: External water-use depth for deciduous trees over the forcing interval
   :Input unit: mm
   :Role: driver
   :Temporal semantics: sum
   :Requiredness: conditional
   :Missing-value policy: fallback
   :Enforced input range: >= 0.0
   :File aliases: wu_mm_dectr
   :Fallback column: Wuh
   :Active requirement: water_use in [1]

.. option:: wuh_grass

   :Description: External water-use depth for grass over the forcing interval
   :Input unit: mm
   :Role: driver
   :Temporal semantics: sum
   :Requiredness: conditional
   :Missing-value policy: fallback
   :Enforced input range: >= 0.0
   :File aliases: wu_mm_grass
   :Fallback column: Wuh
   :Active requirement: water_use in [1]

.. option:: wuh_bsoil

   :Description: External water-use depth for bare soil over the forcing interval
   :Input unit: mm
   :Role: driver
   :Temporal semantics: sum
   :Requiredness: conditional
   :Missing-value policy: fallback
   :Enforced input range: >= 0.0
   :File aliases: wu_mm_bsoil
   :Fallback column: Wuh
   :Active requirement: water_use in [1]

.. option:: wuh_water

   :Description: External water-use depth for open water over the forcing interval
   :Input unit: mm
   :Role: driver
   :Temporal semantics: sum
   :Requiredness: conditional
   :Missing-value policy: fallback
   :Enforced input range: >= 0.0
   :File aliases: wu_mm_water
   :Fallback column: Wuh
   :Active requirement: water_use in [1]
