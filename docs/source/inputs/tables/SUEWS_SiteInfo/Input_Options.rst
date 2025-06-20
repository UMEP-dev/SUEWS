
.. _Input_Options:

Input Options
~~~~~~~~~~~~~

.. NB: follow the rules to write items here:
.. Description: concise information to describe the meaning of the option, always include unit if applicable
.. this `Description` are synced by multiple places in the doc as the source info.

.. Configuration: detail configuration info should be included in the corresponding csv files that table-specific settings are provided there.

.. option:: a1

	:Description:
		Coefficient for Q* term [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/a1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: a2

	:Description:
		Coefficient for ``dQ*/dt`` term [h]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/a2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: a3

	:Description:
		Constant term [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/a3.csv
			:header-rows: 1
			:widths: 44 18 38




.. option:: ActivityProfWD

	:Description:
		Code linking to `ActivityProfWD` in `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ActivityProfWD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ActivityProfWE

	:Description:
		Code linking to `ActivityProfWE` in `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ActivityProfWE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AHMin_WD

	:Description:
		Minimum QF on weekdays [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AHMin_WD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AHMin_WE

	:Description:
		Minimum QF on weekends [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AHMin_WE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AHSlope_Heating_WD

	:Description:
		Heating slope of QF on weekdays [W |m^-2| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AHSlope_Heating_WD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AHSlope_Heating_WE

	:Description:
		Heating slope of QF on weekends [W |m^-2| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AHSlope_Heating_WE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AHSlope_Cooling_WD

	:Description:
		Cooling slope of QF on weekdays [W |m^-2| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AHSlope_Cooling_WD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AHSlope_Cooling_WE

	:Description:
		Cooling slope of QF on weekends [W |m^-2| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AHSlope_Cooling_WE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AlbedoMax

	:Description:
		Effective surface albedo (middle of the day value) for summertime.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AlbedoMax.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AlbedoMin

	:Description:
		Effective surface albedo (middle of the day value) for wintertime (not including snow).

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AlbedoMin.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: alpha

	:Description:
		The mean apparent ecosystem quantum. Represents the initial slope of the light-response curve. Not in use
		[umol CO2 umol photons^-1].


	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/alpha.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: Alt

	:Description:
		Altitude of grids [m].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Alt.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AnOHM_Ch

	:Description:
		Bulk transfer coefficient for this surface to use in AnOHM [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AnOHM_Ch.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AnOHM_Cp

	:Description:
		Volumetric heat capacity for this surface to use in AnOHM [J |m^-3|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AnOHM_Cp.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AnOHM_Kk

	:Description:
		Thermal conductivity for this surface to use in AnOHM [W m |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AnOHM_Kk.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AnthropogenicCode

	:Description:
		Code for modelling anthropogenic heat flux linking to `Code` of `SUEWS_AnthropogenicEmission.txt`, which contains the model coefficients for estimation of the anthropogenic heat flux (used if `EmissionsMethod` = 1, 2 in `RunControl.nml`).

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AnthropogenicCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: AreaWall

	:Description:
		Area of wall within grid (needed for ESTM calculation) [ |m^2| ].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/AreaWall.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: BaseT

	:Description:

		Base Temperature for initiating growing degree days (GDD) for leaf growth. [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/BaseT.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: BaseTe

	:Description:

		Base temperature for initiating sensesance degree days (SDD) for leaf off. [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/BaseTe.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: BaseT_HC

	:Description:
		Base temperature for heating degree days [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/BaseT_HC.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: beta

	:Description:

		The light-saturated gross photosynthesis of the canopy describing the maximum photosynthesis the certain vegetation can have. [umol |m^-2| |s^-1| ]


	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/beta.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: theta

	:Description:

		The convexity of the curve at light saturation. Not in use.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/theta.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: alpha_enh

	:Description:

		Part of the `alpha` coefficient related to the fraction of vegetation.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/alpha_enh.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: beta_enh

	:Description:

		Part of the `beta` coefficient related to the fraction of vegetation.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/beta_enh.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: resp_a

	:Description:

		Soil and vegetation respiration coefficient a.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/resp_a.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: resp_b

	:Description:

		Soil and vegetation respiration coefficient b - related to air temperature dependency.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/resp_b.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: min_respi

	:Description:

		Minimum soil and vegetation respiration rate (for cold-temperature limit) [umol |m^-2| |s^-1|].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/min_respi.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: BiogenCO2Code

	:Description:
		Code linking to the `Code` column in `SUEWS_BiogenCO2.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/BiogenCO2Code.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: QF0_BEU_WD

	:Description:
		Building energy use [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/QF0_BEU_WD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: QF0_BEU_WE

	:Description:
		Building energy use [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/QF0_BEU_WE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: CO2PointSource

	:Description:
		CO2 emission point source within the grid [kgC |day^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/CO2PointSource.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: Code

	:Description:
		Code linking to a corresponding look-up table.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_Bldgs

	:Description:
		Code for `Bldgs` surface characteristics linking to `Code` of `SUEWS_NonVeg.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_Bldgs.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_BSoil

	:Description:
		Code for `BSoil` surface characteristics linking to `Code` of `SUEWS_NonVeg.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_BSoil.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_DecTr

	:Description:
		Code for `DecTr` surface characteristics linking to `Code` of `SUEWS_Veg.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_DecTr.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_ESTMClass_Bldgs1

	:Description:
		Code linking to `SUEWS_ESTMCoefficients.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_ESTMClass_Bldgs1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_ESTMClass_Bldgs2

	:Description:
		Code linking to `SUEWS_ESTMCoefficients.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_ESTMClass_Bldgs2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_ESTMClass_Bldgs3

	:Description:
		Code linking to `SUEWS_ESTMCoefficients.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_ESTMClass_Bldgs3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_ESTMClass_Bldgs4

	:Description:
		Code linking to `SUEWS_ESTMCoefficients.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_ESTMClass_Bldgs4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_ESTMClass_Bldgs5

	:Description:
		Code linking to `SUEWS_ESTMCoefficients.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_ESTMClass_Bldgs5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_ESTMClass_Paved1

	:Description:
		Code linking to `SUEWS_ESTMCoefficients.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_ESTMClass_Paved1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_ESTMClass_Paved2

	:Description:
		Code linking to `SUEWS_ESTMCoefficients.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_ESTMClass_Paved2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_ESTMClass_Paved3

	:Description:
		Code linking to `SUEWS_ESTMCoefficients.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_ESTMClass_Paved3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_EveTr

	:Description:
		Code for `EveTr` surface characteristics linking to `Code` of `SUEWS_Veg.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_EveTr.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_Grass

	:Description:
		Code for `Grass` surface characteristics linking to `Code` of `SUEWS_Veg.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_Grass.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_Paved

	:Description:
		Code for `Paved` surface characteristics linking to `Code` of `SUEWS_NonVeg.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_Paved.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Code_Water

	:Description:
		Code for `Water` surface characteristics linking to `Code` of `SUEWS_Water.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Code_Water.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: CondCode

	:Description:
		Code for surface conductance parameters linking to `Code` of `SUEWS_Conductance.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/CondCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: CRWMax

	:Description:
		Maximum water holding capacity of snow [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/CRWMax.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: CRWMin

	:Description:
		Minimum water holding capacity of snow [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/CRWMin.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWat(1)

	:Description:
		Irrigation allowed on Sundays [1], if not [0]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWat(1).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWat(2)

	:Description:
		Irrigation allowed on Mondays [1], if not [0]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWat(2).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWat(3)

	:Description:
		Irrigation allowed on Tuesdays [1], if not [0]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWat(3).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWat(4)

	:Description:
		Irrigation allowed on Wednesdays [1], if not [0]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWat(4).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWat(5)

	:Description:
		Irrigation allowed on Thursdays [1], if not [0]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWat(5).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWat(6)

	:Description:
		Irrigation allowed on Fridays [1], if not [0]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWat(6).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWat(7)

	:Description:
		Irrigation allowed on Saturdays [1], if not [0]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWat(7).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWatPer(1)

	:Description:
		Fraction of properties using irrigation on Sundays [0-1]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWatPer(1).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWatPer(2)

	:Description:
		Fraction of properties using irrigation on Mondays [0-1]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWatPer(2).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWatPer(3)

	:Description:
		Fraction of properties using irrigation on Tuesdays [0-1]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWatPer(3).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWatPer(4)

	:Description:
		Fraction of properties using irrigation on Wednesdays [0-1]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWatPer(4).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWatPer(5)

	:Description:
		Fraction of properties using irrigation on Thursdays [0-1]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWatPer(5).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWatPer(6)

	:Description:
		Fraction of properties using irrigation on Fridays [0-1]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWatPer(6).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DayWatPer(7)

	:Description:
		Fraction of properties using irrigation on Saturdays [0-1]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DayWatPer(7).csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DrainageCoef1

	:Description:
		Coefficient D0 [mm |h^-1|] used in :option:`DrainageEq`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DrainageCoef1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DrainageCoef2

	:Description:
		Coefficient b [-] used in :option:`DrainageEq`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DrainageCoef2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: DrainageEq

	:Description:
		Calculation choice for Drainage equation

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/DrainageEq.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: EF_umolCO2perJ

	:Description:
		Emission factor for fuels used for building heating. [umol CO2 |J^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/EF_umolCO2perJ.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: Emissivity

	:Description:
		Effective surface emissivity.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Emissivity.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: EndDLS

	:Description:
		End of the day light savings [DOY]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/EndDLS.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: EnEF_v_Jkm

	:Description:
		Emission factor for heat from traffic [J k |m^-1| ].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/EnEF_v_Jkm.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: EnergyUseProfWD

	:Description:
		Code linking to `EnergyUseProfWD` in `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/EnergyUseProfWD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: EnergyUseProfWE

	:Description:
		Code linking to `EnergyUseProfWE` in `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/EnergyUseProfWE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ESTMCode

	:Description:
		 Code for ESTM coefficients linking to `SUEWS_ESTMCoefficients.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ESTMCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: FAI_Bldgs

	:Description:
		Frontal area index for buildings [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/FAI_Bldgs.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: FAI_DecTr

	:Description:
		Frontal area index for deciduous trees [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/FAI_DecTr.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: FAI_EveTr

	:Description:
		Frontal area index for evergreen trees [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/FAI_EveTr.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Faut

	:Description:
		Fraction of irrigated area that is irrigated using automated systems

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Faut.csv
			:header-rows: 1
			:widths: 44 18 38



.. option:: FcEF_v_kgkmWD

	:Description:
		CO2 emission factor for traffic on weekdays [kg |km^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/FcEF_v_kgkmWD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: FcEF_v_kgkmWE

	:Description:
		CO2 emission factor for traffic on weekends [kg |km^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/FcEF_v_kgkmWD.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: FcEF_v_Jkm

	:Description:
		Traffic emission factor for CO2.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/FcEF_v_Jkm.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: fcld

	:Description:
		Cloud fraction [tenths]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/fcld.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: FlowChange

	:Description:
		Difference in input and output flows for water surface [mm |h^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/FlowChange.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fraction1of8

	:Description:
		Fraction of water that can flow to `GridConnection1of8` [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fraction1of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fraction2of8

	:Description:
		Fraction of water that can flow to `GridConnection2of8` [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fraction2of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fraction3of8

	:Description:
		Fraction of water that can flow to `GridConnection3of8` [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fraction3of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fraction4of8

	:Description:
		Fraction of water that can flow to `GridConnection4of8` [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fraction4of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fraction5of8

	:Description:
		Fraction of water that can flow to `GridConnection5of8` [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fraction5of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fraction6of8

	:Description:
		Fraction of water that can flow to `GridConnection6of8` [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fraction6of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fraction7of8

	:Description:
		Fraction of water that can flow to `GridConnection7of8` [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fraction7of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fraction8of8

	:Description:
		Fraction of water that can flow to `GridConnection8of8` [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fraction8of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_Bldgs

	:Description:
		Surface cover fraction of buildings [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_Bldgs.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_Bsoil

	:Description:
		Surface cover fraction of bare soil or unmanaged land [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_Bsoil.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_DecTr

	:Description:
		Surface cover fraction of deciduous trees and shrubs [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_DecTr.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_ESTMClass_Bldgs1

	:Description:
		Surface cover fraction of building class 1 used in ESTM calculations

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_ESTMClass_Bldgs1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_ESTMClass_Bldgs2

	:Description:
		Surface cover fraction of building class 2 used in ESTM calculations

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_ESTMClass_Bldgs2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_ESTMClass_Bldgs3

	:Description:
		Surface cover fraction of building class 3 used in ESTM calculations

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_ESTMClass_Bldgs3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_ESTMClass_Bldgs4

	:Description:
		Surface cover fraction of building class 4 used in ESTM calculations

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_ESTMClass_Bldgs4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_ESTMClass_Bldgs5

	:Description:
		Surface cover fraction of building class 5 used in ESTM calculations

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_ESTMClass_Bldgs5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_ESTMClass_Paved1

	:Description:
		Surface cover fraction of `Paved` surface class 1 used in ESTM calculations

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_ESTMClass_Paved1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_ESTMClass_Paved2

	:Description:
		Surface cover fraction of `Paved` surface class 2 used in ESTM calculations

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_ESTMClass_Paved2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_ESTMClass_Paved3

	:Description:
		Surface cover fraction of `Paved` surface class 3 used in ESTM calculations

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_ESTMClass_Paved3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_EveTr

	:Description:
		Surface cover fraction of `EveTr`: evergreen trees and shrubs [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_EveTr.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_Grass

	:Description:
		Surface cover fraction of `Grass` [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_Grass.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_Paved

	:Description:
		Surface cover fraction of `Paved` surfaces [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_Paved.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Fr_Water

	:Description:
		Surface cover fraction of open water [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Fr_Water.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: FrFossilFuel_Heat

	:Description:
		Fraction of fossil fuels used for building heating [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/FrFossilFuel_Heat.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: FrFossilFuel_NonHeat

	:Description:
		Fraction of fossil fuels used for building energy use [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/FrFossilFuel_NonHeat.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: FrPDDwe

	:Description:
		Fraction of weekend population to weekday population. [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/FrPDDwe.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: G1

	:Description:
		Related to maximum surface conductance [mm |s^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/G1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: G2

	:Description:
		Related to Kdown dependence [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/G2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: G3

	:Description:
		Related to VPD dependence [units depend on `gsModel`]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/G3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: G4

	:Description:
		Related to VPD dependence [units depend on `gsModel`]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/G4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: G5

	:Description:
		Related to temperature dependence [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/G5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: G6

	:Description:
		Related to soil moisture dependence [|mm^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/G6.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: gamq_gkgm

	:Description:
		vertical gradient of specific humidity [g |kg^-1| |m^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/gamq_gkgm.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: gamt_Km

	:Description:
		vertical gradient of potential temperature [K |m^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/gamt_Km.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: GDDFull

	:Description:

		The growing degree days (GDD) needed for full capacity of the leaf area index (LAI) [°C].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/GDDFull.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Grid

	:Description:
		a unique number to represent grid

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Grid.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: GridConnection1of8

	:Description:
		Number of the 1st grid where water can flow to
		The next 8 pairs of columns specify the water flow between grids. The first column of each pair specifies the grid that the water flows to (from the current grid, column 1); the second column of each pair specifies the fraction of water that flow to that grid. The fraction (i.e. amount) of water transferred may be estimated based on elevation, the length of connecting surface between grids, presence of walls, etc. Water cannot flow from the current grid to the same grid, so the grid number here must be different to the grid number in column 1. Water can flow to a maximum of 8 other grids. If there is no water flow between grids, or a single grid is run, set to 0. See section on Grid Connections

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/GridConnection1of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: GridConnection2of8

	:Description:
		Number of the 2nd grid where water can flow to

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/GridConnection2of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: GridConnection3of8

	:Description:
		Number of the 3rd grid where water can flow to

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/GridConnection3of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: GridConnection4of8

	:Description:
		Number of the 4th grid where water can flow to

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/GridConnection4of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: GridConnection5of8

	:Description:
		Number of the 5th grid where water can flow to

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/GridConnection5of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: GridConnection6of8

	:Description:
		Number of the 6th grid where water can flow to

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/GridConnection6of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: GridConnection7of8

	:Description:
		Number of the 7th grid where water can flow to

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/GridConnection7of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: GridConnection8of8

	:Description:
		Number of the 8th grid where water can flow to

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/GridConnection8of8.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: gsModel

	:Description:
		Formulation choice for conductance calculation.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/gsModel.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: H_Bldgs

	:Description:
		Mean building height [m]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/H_Bldgs.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: H_DecTr

	:Description:
		Mean height of deciduous trees [m]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/H_DecTr.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: H_EveTr

	:Description:
		Mean height of evergreen trees [m]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/H_EveTr.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: H_maintain

	:Description:
		water depth to maintain used in automatic irrigation (e.g., ponding water due to flooding irrigation in rice crop-field) [mm].

	.. note::
		1. `H_maintain` can be positive (e.g., ponding water due to flooding irrigation in rice crop-field) or negative (e.g., soil water store level to maintain: `SoilStoreCap` + `H_maintain` ) or zero (e.g., to maintain a maximum soil store level, i.e., `SoilStoreCap`).
		2. Disable this feature by setting this parameter to ``-999``: then no restrictions will be applied to maintain available water level.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/H_maintain.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: id

	:Description:
		Day of year [DOY]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/id.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Ie_a1

	:Description:
		Coefficient for automatic irrigation model [mm |d^-1| ]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Ie_a1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Ie_a2

	:Description:
		Coefficient for automatic irrigation model [mm |d^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Ie_a2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Ie_a3

	:Description:
		Coefficient for automatic irrigation model [mm |d^-2| ]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Ie_a3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Ie_end

	:Description:
		Day when irrigation ends [DOY]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Ie_end.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Ie_m1

	:Description:
		Coefficient for manual irrigation model [mm |d^-1| ]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Ie_m1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Ie_m2

	:Description:
		Coefficient for manual irrigation model [mm |d^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Ie_m2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Ie_m3

	:Description:
		Coefficient for manual irrigation model [mm |d^-2| ]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Ie_m3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Ie_start

	:Description:
		Day when irrigation starts [DOY]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Ie_start.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ih

	:Description:
		Hour [H]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ih.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: imin

	:Description:
		Minute [M]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/imin.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: InfiltrationRate

	:Description:
		Infiltration rate.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/InfiltrationRate.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_albedo

	:Description:
		Albedo of all internal elements for building surfaces only

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_albedo.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_CHbld

	:Description:
		Bulk transfer coefficient of internal building elements [W |m^-2| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_CHbld.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_CHroof

	:Description:
		Bulk transfer coefficient of internal roof [W |m^-2| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_CHroof.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_CHwall

	:Description:
		Bulk transfer coefficient of internal wall [W |m^-2| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_CHwall.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_emissivity

	:Description:
		Emissivity of all internal elements for building surfaces only

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_emissivity.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_k1

	:Description:
		Thermal conductivity of the first layer [W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_k1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_k2

	:Description:
		Thermal conductivity of the second layer [W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_k2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_k3

	:Description:
		Thermal conductivity of the third layer [W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_k3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_k4

	:Description:
		Thermal conductivity of the fourth layer [W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_k4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_k5

	:Description:
		Thermal conductivity of the fifth layer [W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_k5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_rhoCp1

	:Description:
		Volumetric heat capacity of the first layer[J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_rhoCp1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_rhoCp2

	:Description:
		Volumetric heat capacity of the second layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_rhoCp2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_rhoCp3

	:Description:
		Volumetric heat capacity of the third layer[J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_rhoCp3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_rhoCp4

	:Description:
		Volumetric heat capacity of the fourth layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_rhoCp4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_rhoCp5

	:Description:
		Volumetric heat capacity of the fifth layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_rhoCp5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_thick1

	:Description:
		Thickness of the first layer [m] for building surfaces only

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_thick1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_thick2

	:Description:
		Thickness of the second layer [m]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_thick2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_thick3

	:Description:
		Thickness of the third layer [m]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_thick3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_thick4

	:Description:
		Thickness of the fourth layer [m]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_thick4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Internal_thick5

	:Description:
		Thickness of the fifth layer [m]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Internal_thick5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: InternalWaterUse

	:Description:
		Internal water use [mm |h^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/InternalWaterUse.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: IrrFr_Paved

	:Description:
		Fraction of `Paved` that is irrigated [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/IrrFr_Paved.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: IrrFr_Bldgs

	:Description:
		Fraction of `Bldgs` that is irrigated [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/IrrFr_Bldgs.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: IrrFr_DecTr

	:Description:
		Fraction of `DecTr` that is irrigated [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/IrrFr_DecTr.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: IrrFr_EveTr

	:Description:
		Fraction of `EveTr` that is irrigated [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/IrrFr_EveTr.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: IrrFr_Grass

	:Description:
		Fraction of `Grass` that is irrigated [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/IrrFr_Grass.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: IrrFr_BSoil

	:Description:
		Fraction of `BSoil` that is irrigated [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/IrrFr_BSoil.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: IrrFr_Water

	:Description:
		Fraction of `Water` that is irrigated [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/IrrFr_Water.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: IrrigationCode

	:Description:
		Code for modelling irrigation linking to `Code` of `SUEWS_Irrigation.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/IrrigationCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: it

	:Description:
		Hour [H]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/it.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: iy

	:Description:
		Year [YYYY]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/iy.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: kdiff

	:Description:
		Diffuse radiation [W |m^-2|].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/kdiff.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: kdir

	:Description:
		Direct radiation [W |m^-2|].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/kdir.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: kdown

	:Description:
		Incoming shortwave radiation [W |m^-2|].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/kdown.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Kmax

	:Description:
		Maximum incoming shortwave radiation [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Kmax.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: lai

	:Description:
		Observed leaf area index [|m^-2| |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/lai.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: LAIEq

	:Description:
		LAI calculation choice.

		.. note::

			North and South hemispheres are treated slightly differently.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/LAIEq.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: LAIMax

	:Description:
		full leaf-on summertime value

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/LAIMax.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: LAIMin

	:Description:
		leaf-off wintertime value

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/LAIMin.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: lat

	:Description:
		Latitude [deg].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/lat.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ldown

	:Description:
		Incoming longwave radiation [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ldown.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: LeafGrowthPower1

	:Description:
		a parameter required by LAI calculation in `LAIEq`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/LeafGrowthPower1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: LeafGrowthPower2

	:Description:
		a parameter required by LAI calculation [|K^-1|] in `LAIEq`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/LeafGrowthPower2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: LeafOffPower1

	:Description:
		a parameter required by LAI calculation [|K^-1|] in `LAIEq`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/LeafOffPower1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: LeafOffPower2

	:Description:
		a parameter required by LAI calculation [|K^-1|] in `LAIEq`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/LeafOffPower2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: lng

	:Description:
		longitude [deg]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/lng.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: LUMPS_Cover

	:Description:
		Limit when surface totally covered with water for LUMPS [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/LUMPS_Cover.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: LUMPS_DrRate

	:Description:
		Drainage rate of bucket for LUMPS [mm |h^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/LUMPS_DrRate.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: LUMPS_MaxRes

	:Description:
		Maximum water bucket reservoir [mm] Used for LUMPS surface wetness control.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/LUMPS_MaxRes.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: MaxQFMetab

	:Description:

		Maximum value for human heat emission. [W |m^-2|]

		Example values: 175 Sailor and Lu (2004) :cite:`SL04`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/MaxQFMetab.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: MaxFCMetab

	:Description:

		Maximum (day) CO2 from human metabolism. [umol |s^-1| |cap^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/MaxFCMetab.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: MaxConductance

	:Description:

		The maximum conductance of each vegetation or surface type. [mm |s^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/MaxConductance.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: MinQFMetab

	:Description:

		Minimum value for human heat emission. [W |m^-2|]

		Example values: 75 Sailor and Lu (2004) :cite:`SL04`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/MinQFMetab.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: MinFCMetab

	:Description:

		Minimum (night) CO2 from human metabolism. [umol |s^-1| |cap^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/MinFCMetab.csv
			:header-rows: 1
			:widths: 44 18 38




.. option:: NARP_Trans

	:Description:
		Atmospheric transmissivity for NARP [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/NARP_Trans.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: nroom

	:Description:
		Number of rooms per floor for building surfaces only [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/nroom.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: OBS_SMCap

	:Description:

		The maximum observed soil moisture. [|m^3| |m^-3| or kg |kg^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/OBS_SMCap.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: OBS_SMDepth

	:Description:

		The depth of soil moisture measurements. [mm]


	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/OBS_SMDepth.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: OBS_SoilNotRocks

	:Description:

		Fraction of soil without rocks. [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/OBS_SoilNotRocks.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: OHMCode_SummerDry

	:Description:
		Code for OHM coefficients to use for this surface during dry conditions in summer, linking to `SUEWS_OHMCoefficients.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/OHMCode_SummerDry.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: OHMCode_SummerWet

	:Description:
		Code for OHM coefficients to use for this surface during wet conditions in summer, linking to `SUEWS_OHMCoefficients.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/OHMCode_SummerWet.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: OHMCode_WinterDry

	:Description:
		Code for OHM coefficients to use for this surface during dry conditions in winter, linking to `SUEWS_OHMCoefficients.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/OHMCode_WinterDry.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: OHMCode_WinterWet

	:Description:
		Code for OHM coefficients to use for this surface during wet conditions in winter, linking to `SUEWS_OHMCoefficients.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/OHMCode_WinterWet.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: OHMThresh_SW

	:Description:
		Temperature threshold determining whether summer/winter OHM coefficients are applied [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/OHMThresh_SW.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: OHMThresh_WD

	:Description:
		Soil moisture threshold determining whether wet/dry OHM coefficients are applied [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/OHMThresh_WD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: PipeCapacity

	:Description:
		Storage capacity of pipes [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/PipeCapacity.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: PopDensDay

	:Description:
		Daytime population density (i.e. workers, tourists) [people |ha^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/PopDensDay.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: PopDensNight

	:Description:
		Night-time population density (i.e. residents) [people |ha^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/PopDensNight.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: PopProfWD

	:Description:
		Code for population density profile (weekdays) linking to `Code` of `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/PopProfWD.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: PopProfWE

	:Description:
		Code for population density profile (weekends) linking to `Code` of `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/PopProfWE.csv
			:header-rows: 1
			:widths: 44 18 38



.. option:: PorosityMax

	:Description:
		full leaf-on summertime value Used only for `DecTr` (can affect roughness calculation)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/PorosityMax.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: PorosityMin

	:Description:
		leaf-off wintertime value Used only for `DecTr` (can affect roughness calculation)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/PorosityMin.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: PrecipLimAlb

	:Description:
		Limit for hourly precipitation when the ground is fully covered with snow [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/PrecipiLimAlb.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: PrecipLimSnow

	:Description:
		Temperature limit when precipitation falls as snow [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/PrecipLimSnow.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: pres

	:Description:
		Barometric pressure [kPa]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/pres.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: qe

	:Description:
		Latent heat flux [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/qe.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: qf

	:Description:
		Anthropogenic heat flux [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/qf.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: QF_A_WD

	:Description:
		Base value for QF on weekdays [W |m^-2| (Cap |ha^-1| |)^-1| ]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/QF_A_WD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: QF_A_WE

	:Description:
		Base value for QF on weekends [W |m^-2| (Cap |ha^-1| |)^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/QF_A_WE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: QF_B_WD

	:Description:
		Parameter related to cooling degree days on weekdays [W |m^-2| |K^-1| (Cap |ha^-1| |)^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/QF_B_WD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: QF_B_WE

	:Description:
		Parameter related to cooling degree days on weekends [W |m^-2| |K^-1| (Cap |ha^-1| |)^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/QF_B_WE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: QF_C_WD

	:Description:
		Parameter related to heating degree days on weekdays [W |m^-2| |K^-1| (Cap |ha^-1| |)^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/QF_C_WD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: QF_C_WE

	:Description:
		Parameter related to heating degree days on weekends [W |m^-2| |K^-1| (Cap |ha^-1| |)^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/QF_C_WE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: q+_gkg

	:Description:
		specific humidity at the top of CBL [g |kg^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/q+_gkg.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: q_gkg

	:Description:
		specific humidiy in CBL [g |kg^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/q_gkg.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: qh

	:Description:
		Sensible heat flux [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/qh.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: qn

	:Description:
		Net all-wave radiation [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/qn.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: qs

	:Description:
		Storage heat flux [W |m^-2|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/qs.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: RadMeltFactor

	:Description:
		Hourly radiation melt factor of snow [mm |w^-1| |h^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/RadMeltFactor.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: rain

	:Description:
		Rainfall [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/rain.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: RH

	:Description:
		Relative Humidity [%]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/RH.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: RunoffToWater

	:Description:
		Fraction of above-ground runoff flowing to water surface during flooding [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/RunoffToWater.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: S1

	:Description:
		A parameter related to soil moisture dependence [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/S1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: S2

	:Description:
		A parameter related to soil moisture dependence [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/S2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SatHydraulicCond

	:Description:
		Hydraulic conductivity for saturated soil [mm |s^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SatHydraulicCond.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SDDFull

	:Description:

		The sensesence degree days (SDD) needed to initiate leaf off. [°C]


	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SDDFull.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: snow

	:Description:
		Snowfall [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/snow.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SnowClearingProfWD

	:Description:
		Code for snow clearing profile (weekdays) linking to `Code` of `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SnowClearingProfWD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SnowClearingProfWE

	:Description:
		Code for snow clearing profile (weekends) linking to `Code` of `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SnowClearingProfWE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SnowCode

	:Description:
		Code for snow surface characteristics linking to `Code` of SUEWS_Snow.txt

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SnowCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SnowDensMax

	:Description:
		Maximum snow density [kg |m^-3|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/snowDensMax.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SnowDensMin

	:Description:
		Fresh snow density [kg |m^-3|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/snowDensMin.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SnowLimPatch

	:Description:
		Limit for the snow water equivalent when snow cover starts to be patchy [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SnowLimPatch.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SnowLimRemove

	:Description:
		Limit of the snow water equivalent for snow removal from roads and roofs [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SnowLimRemove.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SoilDensity

	:Description:
		Soil density [kg |m^-3|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SoilDensity.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SoilDepth

	:Description:
		Depth of soil beneath the surface [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SoilDepth.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SoilStoreCap

	:Description:
		Limit value for `SoilDepth` [mm]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SoilStoreCap.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SoilTypeCode

	:Description:
		Code for soil characteristics below this surface linking to `Code` of `SUEWS_Soil.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SoilTypeCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: StartDLS

	:Description:
		Start of the day light savings [DOY]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/StartDLS.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: StateLimit

	:Description:

		Upper limit to the surface state. [mm]

		Currently only used for the water surface. Set to a large value (e.g. 20000 mm = 20 m) if the water body is substantial (lake, river, etc) or a small value (e.g. 10 mm) if water bodies are very shallow (e.g. fountains). WaterDepth (column 9) must not exceed this value.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/StateLimit.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: StorageMax

	:Description:
		Maximum water storage capacity for upper surfaces (i.e. canopy)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/StorageMax.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: StorageMin

	:Description:
		Minimum water storage capacity for upper surfaces (i.e. canopy).

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/StorageMin.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: SurfaceArea

	:Description:
		Area of the grid [ha].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/SurfaceArea.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_k1

	:Description:
		Thermal conductivity of the first layer [W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_k1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_k2

	:Description:
		Thermal conductivity of the second layer [W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_k2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_k3

	:Description:
		Thermal conductivity of the third layer[W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_k3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_k4

	:Description:
		Thermal conductivity of the fourth layer[W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_k4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_k5

	:Description:
		Thermal conductivity of the fifth layer [W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_k5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_rhoCp1

	:Description:
		Volumetric heat capacity of the first layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_rhoCp1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_rhoCp2

	:Description:
		Volumetric heat capacity of the second layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_rhoCp2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_rhoCp3

	:Description:
		Volumetric heat capacity of the third layer[J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_rhoCp3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_rhoCp4

	:Description:
		Volumetric heat capacity of the fourth layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_rhoCp4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_rhoCp5

	:Description:
		Volumetric heat capacity of the fifth layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_rhoCp5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_thick1

	:Description:
		Thickness of the first layer [m] for roofs (building surfaces) and ground (all other surfaces)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_thick1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_thick2

	:Description:
		Thickness of the second layer [m] (if no second layer, set to -999.)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_thick2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_thick3

	:Description:
		Thickness of the third layer [m] (if no third layer, set to -999.)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_thick3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_thick4

	:Description:
		Thickness of the fourth layer [m] (if no fourth layer, set to -999.)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_thick4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Surf_thick5

	:Description:
		Thickness of the fifth layer [m] (if no fifth layer, set to -999.)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Surf_thick5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Tair

	:Description:
		Air temperature [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Tair.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: tau_a

	:Description:
		Time constant for snow albedo aging in cold snow [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/tau_a.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: tau_f

	:Description:
		Time constant for snow albedo aging in melting snow [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/tau_f.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: tau_r

	:Description:
		Time constant for snow density ageing [-]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/tau_r.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: TCritic_Heating_WD

	:Description:
		Critical heating temperature on weekdays [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TCritic_Heating_WD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: TCritic_Heating_WE

	:Description:
		Critical heating temperature on weekends [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TCritic_Heating_WE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: TCritic_Cooling_WD

	:Description:
		Critical cooling temperature on weekdays [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TCritic_Cooling_WD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: TCritic_Cooling_WE

	:Description:
		Critical cooling temperature on weekends [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TCritic_Cooling_WE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: TempMeltFactor

	:Description:
		Hourly temperature melt factor of snow [mm |K^-1| |h^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TempMeltFactor.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: TH

	:Description:
		Upper air temperature limit [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TH.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Theta+_K

	:Description:
		potential temperature at the top of CBL [K]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Theta+_K.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Theta_K

	:Description:
		potential temperature in CBL [K]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Theta_K.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Tiair

	:Description:
		Indoor air temperature [˚C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Tiair.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Timezone

	:Description:
		Time zone [h] for site relative to UTC (east is positive). This should be set according to the times given in the meteorological forcing file(s).

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Timezone.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: TL

	:Description:
		Lower air temperature limit [°C]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TL.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ToBldgs

	:Description:
		Fraction of water going to ``Bldgs``

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ToBldgs.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ToBSoil

	:Description:
		Fraction of water going to ``BSoil``

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ToBSoil.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ToDecTr

	:Description:
		Fraction of water going to ``DecTr``

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ToDecTr.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ToEveTr

	:Description:
		Fraction of water going to `EveTr`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ToEveTr.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ToGrass

	:Description:
		Fraction of water going to `Grass`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ToGrass.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ToPaved

	:Description:
		Fraction of water going to `Paved`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ToPaved.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ToRunoff

	:Description:
		Fraction of water going to `Runoff`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ToRunoff.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ToSoilStore

	:Description:
		Fraction of water going to `SoilStore`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ToSoilStore.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: ToWater

	:Description:
		Fraction of water going to `Water`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/ToWater.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: TraffProfWD

	:Description:
		Code for traffic activity profile (weekdays) linking to `Code` of `SUEWS_Profiles.txt`. Not used in v2018a.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TraffProfWD.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: TraffProfWE

	:Description:
		Code for traffic activity profile (weekends) linking to `Code` of `SUEWS_Profiles.txt`. Not used in v2018a.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TraffProfWE.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: TrafficUnits

	:Description:
		Units for the traffic rate for the study area. 1 = [veh km |m^-2| |day^-1|] 2 = [veh km |cap^-1| |day^-1|]). Not used in v2018a.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TrafficUnits.csv
			:header-rows: 1
			:widths: 44 18 38



.. option:: TrafficRate_WD

	:Description:
		Weekday traffic rate [veh km |m^-2| s-1] Can be used for CO2 flux calculation - not used in v2018a.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TrafficRate_WD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: TrafficRate_WE

	:Description:
		Weekend traffic rate [veh km |m^-2| s-1] Can be used for CO2 flux calculation - not used in v2018a.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/TrafficRate_WE.csv
			:header-rows: 1
			:widths: 44 18 38

.. option:: Troad

	:Description:
		Ground surface temperature [˚C] (used when `TsurfChoice` = 1 or 2)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Troad.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Troof

	:Description:
		Roof surface temperature [˚C] (used when `TsurfChoice` = 1 or 2)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Troof.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Tsurf

	:Description:
		Bulk surface temperature [˚C] (used when `TsurfChoice` = 0)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Tsurf.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Twall

	:Description:
		Wall surface temperature [˚C] (used when `TsurfChoice` = 1)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Twall.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Twall_e

	:Description:
		East-facing wall surface temperature [˚C] (used when `TsurfChoice` = 2)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Twall_e.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Twall_n

	:Description:
		North-facing wall surface temperature [˚C] (used when `TsurfChoice` = 2)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Twall_n.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Twall_s

	:Description:
		South-facing wall surface temperature [˚C] (used when `TsurfChoice` = 2)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Twall_s.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Twall_w

	:Description:
		West-facing wall surface temperature [˚C] (used when `TsurfChoice` = 2)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Twall_w.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: U

	:Description:
		Wind speed. [m |s^-1|. ]Height of the wind speed measurement (`z`) is needed in `SUEWS_SiteSelect.txt` .

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/U.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_k1

	:Description:
		Thermal conductivity of the first layer [W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_k1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_k2

	:Description:
		Thermal conductivity of the second layer [W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_k2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_k3

	:Description:
		Thermal conductivity of the third layer [W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_k3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_k4

	:Description:
		Thermal conductivity of the fourth layer[W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_k4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_k5

	:Description:
		Thermal conductivity of the fifth layer[W |m^-1| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_k5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_rhoCp1

	:Description:
		Volumetric heat capacity of the first layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_rhoCp1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_rhoCp2

	:Description:
		Volumetric heat capacity of the second layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_rhoCp2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_rhoCp3

	:Description:
		Volumetric heat capacity of the third layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_rhoCp3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_rhoCp4

	:Description:
		Volumetric heat capacity of the fourth layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_rhoCp4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_rhoCp5

	:Description:
		Volumetric heat capacity of the fifth layer [J |m^-3| |K^-1|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_rhoCp5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_thick1

	:Description:
		Thickness of the first layer [m] for building surfaces only; set to -999 for all other surfaces

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_thick1.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_thick2

	:Description:
		Thickness of the second layer [m] (if no second layer, set to -999.)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_thick2.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_thick3

	:Description:
		Thickness of the third layer [m] (if no third layer, set to -999.)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_thick3.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_thick4

	:Description:
		Thickness of the fourth layer [m] (if no fourth layer, set to -999.)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_thick4.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wall_thick5

	:Description:
		Thickness of the fifth layer [m] (if no fifth layer, set to -999.)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wall_thick5.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WaterDepth

	:Description:
		Water depth [mm].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WaterDepth.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WaterUseProfAutoWD

	:Description:
		Code for water use profile (automatic irrigation, weekdays) linking to `Code` of `SUEWS_Profiles.txt`. Value of integer is arbitrary but must match code specified in `Code` of `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WaterUseProfAutoWD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WaterUseProfAutoWE

	:Description:
		Code for water use profile (automatic irrigation, weekends) linking to `Code` of `SUEWS_Profiles.txt`. Value of integer is arbitrary but must match code specified in `Code` of `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WaterUseProfAutoWE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WaterUseProfManuWD

	:Description:
		Code for water use profile (manual irrigation, weekdays) linking to `Code` of `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WaterUseProfManuWD.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WaterUseProfManuWE

	:Description:
		Code for water use profile (manual irrigation, weekends) linking to `Code` of `SUEWS_Profiles.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WaterUseProfManuWE.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: wdir

	:Description:
		Wind direction [deg].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/wdir.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WetThreshold

	:Description:
		Depth of water which determines whether evaporation occurs from a partially wet or completely wet surface [mm].

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WetThreshold.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WithinGridBldgsCode

	:Description:
		Code that links to the fraction of water that flows from `Bldgs` surfaces to surfaces in columns 2-10 of `SUEWS_WithinGridWaterDist.txt`

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WithinGridBldgsCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WithinGridBSoilCode

	:Description:
		Code that links to the fraction of water that flows from `BSoil` surfaces to surfaces in columns 2-10 of `SUEWS_WithinGridWaterDist.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WithinGridBSoilCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WithinGridDecTrCode

	:Description:
		Code that links to the fraction of water that flows from `DecTr` surfaces to surfaces in columns 2-10 of `SUEWS_WithinGridWaterDist.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WithinGridDecTrCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WithinGridEveTrCode

	:Description:
		Code that links to the fraction of water that flows from `EveTr` surfaces to surfaces in columns 2-10 of `SUEWS_WithinGridWaterDist.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WithinGridEveTrCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WithinGridGrassCode

	:Description:
		Code that links to the fraction of water that flows from `Grass` surfaces to surfaces in columns 2-10 of `SUEWS_WithinGridWaterDist.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WithinGridGrassCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WithinGridPavedCode

	:Description:
		Code that links to the fraction of water that flows from `Paved` surfaces to surfaces in columns 2-10 of `SUEWS_WithinGridWaterDist.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WithinGridPavedCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: WithinGridWaterCode

	:Description:
		Code that links to the fraction of water that flows from Water surfaces to surfaces in columns 2-10 of `SUEWS_WithinGridWaterDist.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/WithinGridWaterCode.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Wuh

	:Description:
		External water use [|m^3|]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Wuh.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: xsmd

	:Description:
		Observed soil moisture;
		can be provided either as volumetric ([|m^3| |m^-3|] when `SMDMethod` = 1) or gravimetric quantity ([kg |kg^-1|] when `SMDMethod` = 2).
		This should be used in conjunction with other soil properties in `SUEWS_Soil.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/xsmd.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: Year

	:Description:
		Year [YYYY]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/Year.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: z

	:Description:
		Measurement height [m] for all atmospheric forcing variables set in `SSss_YYYY_data_tt.txt`.

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/z.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: z0

	:Description:
		Roughness length for momentum [m]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/z0.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: zd

	:Description:
		Zero-plane displacement [m]

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/zd.csv
			:header-rows: 1
			:widths: 44 18 38


.. option:: zi0

	:Description:
		initial convective boundary layer height (m)

	:Configuration:
		.. csv-table::
			:class: longtable
			:file: csv-table/zi0.csv
			:header-rows: 1
			:widths: 44 18 38
