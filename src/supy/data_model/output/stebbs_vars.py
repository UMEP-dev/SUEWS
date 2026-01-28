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
    ("ws_10", "m s-1", "Wind speed at 10m", 0),
    ("ws_bh", "m s-1", "Wind speed at building height", 0),
    ("ws_hbh", "m s-1", "Wind speed at half building height", 0),
    ("Tair_2", "degC", "Air temperature at 2m", 0),
    ("Tair_bh", "degC", "Air temperature at building height", 0),
    ("Tair_hbh", "degC", "Air temperature at half building height", 0),
    ("Tsurf_sout", "degC", "Surface temperature", 0),
    # Radiation forcing (4)
    ("Kroof_sout", "W m-2", "Shortwave radiation on roof", 0),
    ("Lroof_sout", "W m-2", "Longwave radiation on roof", 0),
    ("Kwall_sout", "W m-2", "Shortwave radiation on wall", 0),
    ("Lwall_sout", "W m-2", "Longwave radiation on wall", 0),
    # Indoor temperatures (10)
    ("Tair_ind", "degC", "Indoor air temperature", 0),
    ("Tindoormass", "degC", "Indoor mass temperature", 0),
    ("Tintwall", "degC", "Internal wall temperature", 0),
    ("Tintroof", "degC", "Internal roof temperature", 0),
    ("Textwall", "degC", "External wall temperature", 0),
    ("Textroof", "degC", "External roof temperature", 0),
    ("Tintwindow", "degC", "Internal window temperature", 0),
    ("Textwindow", "degC", "External window temperature", 0),
    ("Tintgrndflr", "degC", "Internal ground floor temperature", 0),
    ("Textgrndflr", "degC", "External ground floor temperature", 0),
    # Heating/cooling loads (2)
    ("Qload_heating_F", "W", "Heating load (FA)", 0),
    ("Qload_cooling_F", "W", "Cooling load (FA)", 0),
    # Shortwave window/wall/roof absorption (4)
    ("Qsw_trans_win_F", "W", "Shortwave transmitted through window (FA)", 0),
    ("Qsw_abs_win_FA", "W", "Shortwave absorbed by window (FA)", 0),
    ("Qsw_abs_wall_FA", "W", "Shortwave absorbed by wall (FA)", 0),
    ("Qsw_abs_roof_FA", "W", "Shortwave absorbed by roof (FA)", 0),
    # Convective and longwave indoor (6)
    ("Qconv_indair_FA", "W", "Convective heat from indoor air to indoor mass (FA)", 0),
    ("Qlw_net_intwall", "W", "Net longwave from internal wall", 0),
    ("Qlw_net_introof", "W", "Net longwave from internal roof", 0),
    ("Qlw_net_intwin_", "W", "Net longwave from internal window", 0),
    ("Qlw_net_intgrnd", "W", "Net longwave from internal ground floor", 0),
    ("QH_appliance_FA", "W", "Appliance heat (FA)", 0),
    # Ventilation and convection (6)
    ("QH_ventilation_", "W", "Ventilation heat", 0),
    ("QHconv_indwall_", "W", "Convective heat to internal wall", 0),
    ("QHconv_indroof_", "W", "Convective heat to internal roof", 0),
    ("QHconv_indwin_F", "W", "Convective heat to internal window", 0),
    ("QHconv_indgrnd_", "W", "Convective heat to internal ground floor", 0),
    ("QHrejection_hea", "W", "Heat rejection from heating system", 0),
    # Conduction through surfaces (5)
    ("QHcond_wall_FA", "W", "Conductive heat through wall (FA)", 0),
    ("QHcond_roof_FA", "W", "Conductive heat through roof (FA)", 0),
    ("QHcond_window_F", "W", "Conductive heat through window", 0),
    ("QHcond_grndflr_", "W", "Conductive heat through ground floor", 0),
    ("QHcond_ground_F", "W", "Conductive heat through ground", 0),
    # Longwave net external (3)
    ("Qlw_net_wall_FA", "W", "Net longwave from wall (FA)", 0),
    ("Qlw_net_roof_FA", "W", "Net longwave from roof (FA)", 0),
    ("Qlw_net_window_", "W", "Net longwave from window", 0),
    # External convection and cooling (4)
    ("QHconv_extwall_", "W", "Convective heat from external wall", 0),
    ("QHconv_extroof_", "W", "Convective heat from external roof", 0),
    ("QHconv_extwin_F", "W", "Convective heat from external window", 0),
    ("QHrejection_coo", "W", "Heat rejection from cooling system", 0),
    # Water tank/vessel (12)
    ("Qtotal_water", "W", "Total water heat", 0),
    ("Qloss_drain", "W", "Drain heat loss", 0),
    ("Twater_tank", "degC", "Water tank temperature", 0),
    ("Tintwall_tank", "degC", "Internal wall temperature of tank", 0),
    ("Textwall_tank", "degC", "External wall temperature of tank", 0),
    ("Twater_vessel", "degC", "Water vessel temperature", 0),
    ("Tintwall_vessel", "degC", "Internal wall temperature of vessel", 0),
    ("Textwall_vessel", "degC", "External wall temperature of vessel", 0),
    ("Vwater_vessel", "m3", "Volume of water in vessel", 0),
    ("Awater_vessel", "m2", "Area of water in vessel", 0),
    ("Vwall_vessel", "m3", "Volume of wall in vessel", 0),
    ("QH_metabolism_F", "W", "Metabolic sensible heat", 0),
    # Latent and storage heat (5)
    ("QE_metabolism_F", "W", "Metabolic latent heat", 0),
    ("QS_total_FA", "W", "Total storage heat flux (FA)", 0),
    ("QS_wall_FA", "W", "Storage heat flux in wall (FA)", 0),
    ("QS_roof_FA", "W", "Storage heat flux in roof (FA)", 0),
    ("QS_air_FA", "W", "Storage heat flux in air (FA)", 0),
    # Tank wall and water volume (2)
    ("Vwall_tank", "m3", "Volume of wall in tank", 0),
    ("Vwater_tank", "m3", "Volume of water in tank", 0),
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
