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
    # Forcing variables (7)
    ("ws_10", "m s-1", "Wind speed at 10 m", 0),
    ("ws_bh", "m s-1", "Wind speed at building height", 0),
    ("ws_hbh", "m s-1", "Wind speed at half building height", 0),
    ("Tair_2", "degC", "Air temperature at 2 m", 0),
    ("Tair_bh", "degC", "Air temperature at building height", 0),
    ("Tair_hbh", "degC", "Air temperature at half building height", 0),
    ("Twater_mains", "degC", "Cold mains water temperature", 0),
    # Radiation forcing (4)
    ("Kroof_in", "W m-2", "Incoming shortwave radiation on the roof per unit roof area", 0),
    ("Lroof_in", "W m-2", "Incoming longwave radiation on the roof per unit roof area", 0),
    ("Kwall_in", "W m-2", "Incoming shortwave radiation on the walls per unit wall area", 0),
    ("Lwall_in", "W m-2", "Incoming longwave radiation on the walls per unit wall area", 0),
    #Energy balance fluxes (6)
    ("QN_bldg_FA", "W m-2", "Net all-wave radiation per building footprint area", 0),
    ("QEC_bldg_FA", "W m-2", "Total energy consumption and metabolism per building footprint area", 0),
    ("QS_total_FA", "W m-2", "Total storage heat flux per building footprint area", 0),
    ("QH_bldg_FA", "W m-2", "Total convective heat transfer at external building surface per building footprint area", 0),
    ("QBAE_bldg_FA", "W m-2", "Total heat transfer by air exchange per building footprint area", 0),
    ("QWaste_bldg_FA", "W m-2", "Total waste heat from HVAC system to outdoor air per building footprint area", 0),
    # components of storage heat flux and energy consumption (6)
    ("QS_bldg_FA", "W m-2", "Storage heat flux in the building fabric, indoor mass and indoor air", 0),
    ("QS_dhw_FA", "W m-2", "Heat in hot water going to the drainage system (equal to Qloss_drain_FA)", 0),
    ("QS_ground_FA", "W m-2", "Heat conducted into the ground beneath the building (equal to QHcond_ground_FA)", 0),
    ("QEC_heating_FA", "W m-2", "Energy consumption by space heating", 0),
    ("QEC_cooling_FA", "W m-2", "Energy consumption by space cooling", 0),
    ("QEC_dhw_FA", "W m-2", "Energy consumption by the domestic hot water system", 0),
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
    ("Qlw_net_tank_FA", "W m-2", "Net longwave radiation from the outer surface of the hot water tank to the indoor mass", 0),
    ("Qlw_net_vessel_FA", "W m-2", "Net longwave radiation from the outer surface of the vessel (hot water in use) to the indoor mass", 0),    
    # Convection (12)
    ("QHconv_extwall_FA", "W m-2", "Convective heat flux between external wall and outdoor air", 0),
    ("QHconv_extroof_FA", "W m-2", "Convective heat flux between external roof and outdoor air", 0),
    ("QHconv_extwin_FA", "W m-2", "Convective heat flux between external window and outdoor air", 0),
    ("QHconv_intwall_FA", "W m-2", "Convective heat flux between internal surface of wall and indoor air", 0),
    ("QHconv_introof_FA", "W m-2", "Convective heat flux between internal surface of roof and indoor air", 0),
    ("QHconv_intwindow_FA", "W m-2", "Convective heat flux between internal surface of window and indoor air", 0),
    ("QHconv_intgroundflr_FA", "W m-2", "Convective heat flux between internal surface of floor and indoor air", 0),
    ("QHconv_indoormass_FA", "W m-2", "Convective heat flux between indoor mass and indoor air", 0),
    ("QHconv_exttankwall_FA", "W m-2", "Convective heat flux between the outer surface of the hot water tank and indoor air", 0),
    ("QHconv_inttankwall_FA", "W m-2", "Convective heat flux between the inner surface of the hot water tank and the water in it", 0),
    ("QHconv_intvesselwall_FA", "W m-2", "Convective heat flux between the inner surface of the vessel (hot water in use) and the water in it", 0),
    ("QHconv_extvesselwall_FA", "W m-2", "Convective heat flux between the outer surface of the vessel (hot water in use) and indoor air", 0),
    # Conduction (7)
    ("QHcond_wall_FA", "W m-2", "Conductive heat flux through wall", 0),
    ("QHcond_roof_FA", "W m-2", "Conductive heat flux through roof",0),
    ("QHcond_window_FA", "W m-2", "Conductive heat flux through window",0),
    ("QHcond_groundflr_FA", "W m-2", "Conductive heat flux through floor",0),
    ("QHcond_ground_FA", "W m-2", "Conductive heat flux through ground beneath building",0),
    ("QHcond_tank_FA", "W m-2", "Conductive heat flux through the wall of the hot water tank",0),
    ("QHcond_vessel_FA", "W m-2", "Conductive heat flux through the wall of the vessel (hot water in use)",0),    
    # Other heat gain/loss to indoor air (6)
    ("QH_appliance_FA", "W m-2", "Sensible appliance heat gain",0),
    ("QH_lighting_FA", "W m-2", "Sensible lighting heat gain",0),
    ("QH_metabolism_FA", "W m-2", "Sensible metabolic heat gain", 0),
    ("QHwaste_heating_FA", "W m-2", "Sensible heat gain from waste heat due to space heating", 0),
    ("QHwaste_dhw_FA", "W m-2", "Sensible heat gain to indoor air from losses of the water heater", 0),
    ("QH_ventilation_FA", "W m-2", "Heat gain or loss through ventilation", 0),
    # Heating/cooling Load (3)
    ("QHload_heating_FA", "W m-2", "Sensible heating load", 0),
    ("QHload_cooling_FA", "W m-2", "Sensible cooling load", 0),
    ("QHload_dhw_FA", "W m-2", "Heat supplied by the water heater to the hot water tank", 0),
    # Other heat emission pathway (2)
    ("QHwaste_cooling_FA", "W m-2", "Waste heat rejection from cooling system to outdoor air", 0),
    ("Qloss_drain_FA", "W m-2", "Heat carried away by hot water going to the drainage system, relative to the mains water temperature", 0),
    # components of building storage heat flux (6)
    ("QS_wall_FA", "W m-2", "Storage heat flux in wall", 0),
    ("QS_roof_FA", "W m-2", "Storage heat flux in roof", 0),
    ("QS_groundfloor_FA", "W m-2", "Storage heat flux in ground floor", 0),
    ("QS_window_FA", "W m-2", "Storage heat flux in window", 0),
    ("QS_indoormass_FA", "W m-2", "Storage heat flux in indoor mass", 0),
    ("QS_air_FA", "W m-2", "Storage heat flux in indoor air", 0),
    #Water tank/vessel (4)
    ("Twater_tank", "K", "Water temperature in the hot water tank", 0),
    ("Twater_vessel", "K", "Temperature of the hot water in use (vessel)", 0),
    ("Vwater_tank", "m3", "Volume of water in the hot water tank", 0),
    ("Vwater_vessel", "m3", "Volume of hot water in use (vessel)", 0),
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
