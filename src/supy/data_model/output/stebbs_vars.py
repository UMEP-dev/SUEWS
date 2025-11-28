"""STEBBS output variables matching Fortran suews_phys_stebbs.f95 output.

These variables are from the STEBBS (Simple ThermoEnergy Building Budget Scheme).
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)

# Variable definitions matching Fortran dataOutLineSTEBBS assignment (lines 855-892)
# Order must match Fortran output exactly
STEBBS_VARIABLE_DEFS = [
    # Forcing variables (7)
    ("ws", "m s-1", "f104", "Wind speed at standard height", 0),
    ("ws_bh", "m s-1", "f104", "Wind speed at building height", 0),
    ("ws_hbh", "m s-1", "f104", "Wind speed at half building height", 0),
    ("Tair_sout", "degC", "f104", "Air temperature at standard height", 0),
    ("Tair_bh", "degC", "f104", "Air temperature at building height", 0),
    ("Tair_hbh", "degC", "f104", "Air temperature at half building height", 0),
    ("Tsurf_sout", "degC", "f104", "Surface temperature", 0),
    # Radiation forcing (4)
    ("Kroof_sout", "W m-2", "f104", "Shortwave radiation on roof", 0),
    ("Lroof_sout", "W m-2", "f104", "Longwave radiation on roof", 0),
    ("Kwall_sout", "W m-2", "f104", "Shortwave radiation on wall", 0),
    ("Lwall_sout", "W m-2", "f104", "Longwave radiation on wall", 0),
    # Indoor temperatures (9)
    ("Tair_ind", "degC", "f104", "Indoor air temperature", 0),
    ("Tindoormass", "degC", "f104", "Indoor mass temperature", 0),
    ("Tintwall", "degC", "f104", "Internal wall temperature", 0),
    ("Tintroof", "degC", "f104", "Internal roof temperature", 0),
    ("Textwall", "degC", "f104", "External wall temperature", 0),
    ("Textroof", "degC", "f104", "External roof temperature", 0),
    ("Tintwindow", "degC", "f104", "Internal window temperature", 0),
    ("Textwindow", "degC", "f104", "External window temperature", 0),
    ("Tintgroundfloor", "degC", "f104", "Internal ground floor temperature", 0),
    # Ground floor and loads (3)
    ("Textgroundfloor", "degC", "f104", "External ground floor temperature", 0),
    ("QHload_heating", "W", "f104", "Heating load", 0),
    ("QHload_cooling", "W", "f104", "Cooling load", 0),
    # Shortwave window/wall/roof absorption (4)
    ("Qsw_transmitted_window", "W", "f104", "Shortwave transmitted through window", 0),
    ("Qsw_absorbed_window", "W", "f104", "Shortwave absorbed by window", 0),
    ("Qsw_absorbed_wall", "W", "f104", "Shortwave absorbed by wall", 0),
    ("Qsw_absorbed_roof", "W", "f104", "Shortwave absorbed by roof", 0),
    # Convective and longwave indoor (6)
    ("QHconv_indair_indoormass", "W", "f104", "Convective heat from indoor air to indoor mass", 0),
    ("Qlw_net_intwall", "W", "f104", "Net longwave from internal wall to other indoor surfaces", 0),
    ("Qlw_net_introof", "W", "f104", "Net longwave from internal roof to other indoor surfaces", 0),
    ("Qlw_net_intwindow", "W", "f104", "Net longwave from internal window to other indoor surfaces", 0),
    ("Qlw_net_intgroundfloor", "W", "f104", "Net longwave from internal ground floor to other indoor surfaces", 0),
    ("QH_appliance", "W", "f104", "Appliance heat", 0),
    # Ventilation and convection to surfaces (6)
    ("QH_ventilation", "W", "f104", "Ventilation heat", 0),
    ("QHconv_indair_intwall", "W", "f104", "Convective heat from indoor air to internal wall", 0),
    ("QHconv_indair_introof", "W", "f104", "Convective heat from indoor air to internal roof", 0),
    ("QHconv_indair_intwindow", "W", "f104", "Convective heat from indoor air to internal window", 0),
    ("QHconv_indair_intgroundfloor", "W", "f104", "Convective heat from indoor air to internal ground floor", 0),
    ("QHrejection_heating", "W", "f104", "Heat rejection from heating system", 0),
    # Conduction through surfaces (5)
    ("QHcond_wall", "W", "f104", "Conductive heat through wall", 0),
    ("QHcond_roof", "W", "f104", "Conductive heat through roof", 0),
    ("QHcond_window", "W", "f104", "Conductive heat through window", 0),
    ("QHcond_groundfloor", "W", "f104", "Conductive heat through ground floor", 0),
    ("QHcond_ground", "W", "f104", "Conductive heat through ground", 0),
    # Longwave net external (3)
    ("Qlw_net_wall", "W", "f104", "Net longwave from wall to outdoor", 0),
    ("Qlw_net_roof", "W", "f104", "Net longwave from roof to outdoor", 0),
    ("Qlw_net_window", "W", "f104", "Net longwave from window to outdoor", 0),
    # External convection and cooling (4)
    ("QHconv_extwall_outair", "W", "f104", "Convective heat from external wall to outdoor air", 0),
    ("QHconv_extroof_outair", "W", "f104", "Convective heat from external roof to outdoor air", 0),
    ("QHconv_extwindow_outair", "W", "f104", "Convective heat from external window to outdoor air", 0),
    ("QHrejection_cooling", "W", "f104", "Heat rejection from cooling system", 0),
    # Water tank/vessel (12)
    ("Qtotal_water_tank", "W", "f104", "Total water tank heat", 0),
    ("Qloss_drain", "W", "f104", "Drain heat loss", 0),
    ("Twater_tank", "degC", "f104", "Water tank temperature", 0),
    ("Tintwall_tank", "degC", "f104", "Internal wall temperature of tank", 0),
    ("Textwall_tank", "degC", "f104", "External wall temperature of tank", 0),
    ("Twater_vessel", "degC", "f104", "Water vessel temperature", 0),
    ("Tintwall_vessel", "degC", "f104", "Internal wall temperature of vessel", 0),
    ("Textwall_vessel", "degC", "f104", "External wall temperature of vessel", 0),
    ("Vwater_vessel", "m3", "f104", "Volume of water in vessel", 0),
    ("Awater_vessel", "m2", "f104", "Area of water in vessel", 0),
    ("Vwall_vessel", "m3", "f104", "Volume of wall in vessel", 0),
    ("QH_metabolism", "W", "f104", "Metabolic sensible heat", 0),
    # Latent and storage heat (5)
    ("QE_metabolism", "W", "f104", "Metabolic latent heat", 0),
    ("QS_total", "W", "f104", "Total storage heat flux", 0),
    ("QS_wall", "W", "f104", "Storage heat flux in wall", 0),
    ("QS_roof", "W", "f104", "Storage heat flux in roof", 0),
    ("QS_air", "W", "f104", "Storage heat flux in air", 0),
    # SPARTACUS coupling outputs (8)
    ("Qsw_absorbed_wall_SA", "W", "f104", "Shortwave absorbed by wall (SPARTACUS)", 0),
    ("Qsw_absorbed_roof_SA", "W", "f104", "Shortwave absorbed by roof (SPARTACUS)", 0),
    ("Qsw_reflected_wall_SA", "W", "f104", "Shortwave reflected by wall (SPARTACUS)", 0),
    ("Qsw_reflected_roof_SA", "W", "f104", "Shortwave reflected by roof (SPARTACUS)", 0),
    ("Qlw_net_wall_SA", "W", "f104", "Net longwave wall (SPARTACUS)", 0),
    ("Qlw_net_roof_SA", "W", "f104", "Net longwave roof (SPARTACUS)", 0),
    ("Qlw_up_wall_SA", "W", "f104", "Upward longwave wall (SPARTACUS)", 0),
    ("Qlw_up_roof_SA", "W", "f104", "Upward longwave roof (SPARTACUS)", 0),
    # Tank wall and water volume (2)
    ("Vwall_tank", "m3", "f104", "Volume of wall in tank", 0),
    ("Vwater_tank", "m3", "f104", "Volume of water in tank", 0),
]

STEBBS_VARIABLES = [
    OutputVariable(
        name=name,
        unit=unit,
        description=description,
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.STEBBS,
        level=OutputLevel.DEFAULT if level == 0 else OutputLevel(level),
        format=fmt,
    )
    for name, unit, fmt, description, level in STEBBS_VARIABLE_DEFS
]
