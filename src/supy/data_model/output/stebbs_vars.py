"""STEBBS output variables matching Fortran suews_phys_stebbs.f95 output.

These variables are from the STEBBS (Simple ThermoEnergy Building Budget Scheme).
Names match Fortran output exactly (including truncation from fixed-width character arrays).
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)

# Variable definitions matching Fortran dataOutLineSTEBBS assignment
# Names are exactly as they appear in Fortran output (truncated at 16 chars)
STEBBS_VARIABLE_DEFS = [
    # Forcing variables (6)
    ("ws_10", "m s-1", "Wind speed at 10m", 0),
    ("ws_bh", "m s-1", "Wind speed at building height", 0),
    ("ws_hbh", "m s-1", "Wind speed at half building height", 0),
    ("Tair_2", "degC", "Air temperature at 2m", 0),
    ("Tair_bh", "degC", "Air temperature at building height", 0),
    ("Tair_hbh", "degC", "Air temperature at half building height", 0),
    # Radiation forcing (4)
    ("Kroof_in", "W m-2", "Incoming shortwave radiation on roof per surface area", 0),
    ("Lroof_in", "W m-2", "Incoming longwave radiation on roof per surface area", 0),
    ("Kwall_in", "W m-2", "Incoming shortwave radiation on wall per surface area", 0),
    ("Lwall_in", "W m-2", "Incoming longwave radiation on wall per surface area", 0),
    #Energy balance fluxes (6)
    ("QN_bldg_FA", "W m-2", "Net all-wave radiation per building footprint area", 0),
    ("QEC_bldg_FA", "W m-2", "Total energy consumption and metabolism per building footprint area", 0),
    ("QS_total_FA", "W m-2", "Total Storage heat flux per building footprint area", 0),
    ("QH_bldg_FA", "W m-2", "Total convective heat transfer at external building surface per building footprint area", 0),
    ("QBAE_bldg_FA", "W m-2", "Total heat transfer by air exchange per building footprint area", 0),
    ("QWaste_bldg_FA", "W m-2", "Total waste heat from HVAC system to outdoor air per building footprint area", 0),
    # components of storage heat flux and energy consumption (6)
    ("QS_bldg_FA", "W m-2", "Storage heat flux for building components", 0),
    ("QS_dhw_FA", "W m-2", "Storage heat flux in hot water going to drainage system", 0),
    ("QS_ground_FA", "W m-2", "Storage heat flux in the soil beneath building", 0),
    ("QEC_heating_FA", "W m-2", "Energy consumption by space heating", 0),
    ("QEC_cooling_FA", "W m-2", "Energy consumption by space cooling", 0),
    ("QEC_dhw_FA", "W m-2", "Energy consumption by dhw system", 0),
    # Indoor air and surface temperatures (10)
    ("Tair_ind", "K", "Indoor air temperature", 0),
    ("Tindoormass", "K", "Indoor mass temperature", 0),
    ("Tintwall", "K", "Internal surface temperature of wall", 0),
    ("Tintroof", "K", "Internal surface temperature of roof", 0),
    ("Textwall", "K", "External surface temperature of wall", 0),
    ("Textroof", "K", "External surface temperature of roof", 0),
    ("Tintwindow", "K", "Internal surface temperature of window", 0),
    ("Textwindow", "K", "External surface temperature of window", 0),
    ("Tintgrndflr", "K", "Internal surface temperature of ground floor", 0),
    ("Textgrndflr", "K", "External surface temperature of ground floor", 0),
    
    # Detailed heat transfer flux (2)
    # Shortwave window/wall/roof absorption (4)
    ("Qsw_trans_win_FA", "W m-2", "Shortwave radiation transmitted through window", 0),
    ("Qsw_abs_win_FA", "W m-2", "Shortwave radiation absorbed by window", 0),
    ("Qsw_abs_wall_FA", "W m-2", "Shortwave radiation absorbed by wall", 0),
    ("Qsw_abs_roof_FA", "W m-2", "Shortwave radiation absorbed by roof", 0),
    # Net longwave radiation (5)
    ("Qlw_net_wall_FA", "W m-2", "Net longwave radiation from external surface of wall", 0),
    ("Qlw_net_roof_FA", "W m-2", "Net longwave radiation from external surface of roof", 0),
    ("Qlw_net_window_FA", "W m-2", "Net longwave radiation from external surface of window", 0),
    ("Qlw_net_tank_FA", "W m-2", "Net longwave radiation from external surface of tank to internal mass", 0),
    ("Qlw_net_vessel_FA", "W m-2", "Net longwave radiation from external surface of vessel to internal mass", 0),    
    # Convection (12)
    ("QHconv_extwall_FA", "W m-2", "Convective heat between external wall and outdoor air", 0),
    ("QHconv_extroof_FA", "W m-2", "Convective heat between external roof and outdoor air", 0),
    ("QHconv_extwin_FA", "W m-2", "Convective heat between external window and outdoor air", 0),
    ("QHconv_intwall_FA", "W m-2", "Convective heat between internal surface of wall and indoor air", 0),
    ("QHconv_introof_FA", "W m-2", "Convective heat between internal surface of roof and indoor air", 0),
    ("QHconv_intwindow_FA", "W m-2", "Convective heat between internal surface of window and indoor air", 0),
    ("QHconv_intgroundflr_FA", "W m-2", "Convective heat between internal surface of floor and indoor air", 0),
    ("QHconv_indoormass_FA", "W m-2", "Convective heat between internal mass and indoor air", 0),
    ("QHconv_exttankwall_FA", "W m-2", "Convective heat between external surface of tank and indoor air", 0),
    ("QHconv_inttankwall_FA", "W m-2", "Convective heat between internal surface of tank and indoor water", 0),
    ("QHconv_intvesselwall_FA", "W m-2", "Convective heat between internal surface of vessel and water", 0),
    ("QHconv_extvesselwall_FA", "W m-2", "Convective heat between external surface of vessel and indoor air", 0),
    # Conduction (7)
    ("QHcond_wall_FA", "W m-2", "Conductive heat through wall", 0),
    ("QHcond_roof_FA", "W m-2", "Conductive heat through roof",0),
    ("QHcond_window_FA", "W m-2", "Conductive heat through window",0),
    ("QHcond_groundflr_FA", "W m-2", "Conductive heat through floor",0),
    ("QHcond_ground_FA", "W m-2", "Conductive heat through ground beneath building",0),
    ("QHcond_tank_FA", "W m-2", "Conductive heat through water tank",0),
    ("QHcond_vessel_FA", "W m-2", "Conductive heat through water vessel",0),    
    # Other heat gain/loss to indoor air (5)
    ("QH_appliance_FA", "W m-2", "Sensible appliance heat gain",0),
    ("QH_metabolism_FA", "W m-2", "Sensible metabolic heat gain", 0),
    ("QHwaste_heating_FA", "W m-2", "Sensible heat gain from waste heat due to space heating", 0),
    ("QHwaste_dhw_FA", "W m-2", "Sensible heat gain from waste heat due to hot water", 0),
    ("QH_ventilation_FA", "W m-2", "Heat gain/loss through ventilation", 0),
    # Heating/cooling Load (3)
    ("QHload_heating_FA", "W m-2", "Sensible heating load", 0),
    ("QHload_cooling_FA", "W m-2", "Sensible cooling load", 0),
    ("QHload_dhw_FA", "W m-2", "Sensible heat supply to hot water tank", 0),
    # Other heat emission pathway (2)
    ("QHwaste_cooling_FA", "W m-2", "Waste heat rejection from cooling system to outdoor air", 0),
    ("Qloss_drain_FA", "W m-2", "Heat loss from hot water to drainage system", 0),
    # components of building storage heat flux (6)
    ("QS_wall_FA", "W m-2", "Storage heat flux in wall", 0),
    ("QS_roof_FA", "W m-2", "Storage heat flux in roof", 0),
    ("QS_groundfloor_FA", "W m-2", "Storage heat flux ground floor", 0),
    ("QS_window_FA", "W m-2", "Storage heat flux in window", 0),
    ("QS_indoormass_FA", "W m-2", "Storage heat flux in indoormass", 0),
    ("QS_air_FA", "W m-2", "Storage heat flux in air", 0),
    #Water tank/vessel (4)
    ("Twater_tank", "K", "Water temperature in tank", 0),
    ("Twater_vessel", "K", "Water temperature in vessel", 0),
    ("Vwater_tank", "m3", "Volume of water in tank", 0),
    ("Vwater_vessel", "m3", "Volume of water in vessel", 0),
]

STEBBS_VARIABLES = [
    OutputVariable(
        name=name,
        unit=unit,
        description=description,
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.STEBBS,
        level=OutputLevel.DEFAULT if level == 0 else OutputLevel(level),
    )
    for name, unit, description, level in STEBBS_VARIABLE_DEFS
]
