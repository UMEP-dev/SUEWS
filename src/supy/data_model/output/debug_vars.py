"""Debug output variables.

These variables provide detailed diagnostic information broken down by surface type.
Useful for debugging and detailed model analysis.
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)


# Surface types for debug variables
SURFACE_TYPES = ["Paved", "Bldgs", "EveTr", "DecTr", "Grass", "BSoil", "Water"]


def make_debug_surface_vars(base_name, unit, description_template, aggregation=AggregationMethod.AVERAGE, format_spec="f104"):
    """Helper to create debug variables for all surface types.

    Args:
        base_name: Base variable name (e.g., "Ts", "QN")
        unit: Physical unit
        description_template: Description with {} placeholder for surface type
        aggregation: Aggregation method
        format_spec: Fortran format specifier

    Returns:
        List of OutputVariable instances for all surface types
    """
    surface_names = {
        "Paved": "paved surface",
        "Bldgs": "building surface",
        "EveTr": "evergreen tree surface",
        "DecTr": "deciduous tree surface",
        "Grass": "grass surface",
        "BSoil": "bare soil surface",
        "Water": "water surface",
    }

    return [
        OutputVariable(
            name=f"{base_name}_{surf}",
            unit=unit,
            description=description_template.format(surface_names[surf]),
            aggregation=aggregation,
            group=OutputGroup.DEBUG,
            level=OutputLevel.DEFAULT,
            format=format_spec,
        )
        for surf in SURFACE_TYPES
    ]


# Test flag
FLAG_VAR = [
    OutputVariable(
        name="flag_test",
        unit="-",
        description="Flag for testing",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.DEBUG,
        level=OutputLevel.DEFAULT,
        format="f104",
    )
]

# Surface temperature by surface type
TS_VARS = make_debug_surface_vars(
    "Ts", "degC", "Surface temperature for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Net radiation by surface type
QN_VARS = make_debug_surface_vars(
    "QN", "W m-2", "Net all-wave radiation for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Storage heat flux by surface type
QS_VARS = make_debug_surface_vars(
    "QS", "W m-2", "Storage heat flux for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Latent heat flux from PM by surface type
QE0_VARS = make_debug_surface_vars(
    "QE0", "W m-2", "Latent heat flux from PM for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Latent heat flux by surface type
QE_VARS = make_debug_surface_vars(
    "QE", "W m-2", "Latent heat flux for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Sensible heat flux by surface type
QH_VARS = make_debug_surface_vars(
    "QH", "W m-2", "Sensible heat flux for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Water use by surface type
WU_VARS = make_debug_surface_vars(
    "wu", "mm", "Water use for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Evapotranspiration from PM by surface type
EV0_VARS = make_debug_surface_vars(
    "ev0", "mm", "Evapotranspiration from PM for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Evapotranspiration by surface type
EV_VARS = make_debug_surface_vars(
    "ev", "mm", "Evapotranspiration for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Drainage by surface type
DRAIN_VARS = make_debug_surface_vars(
    "drain", "mm", "Drainage for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Surface wetness (previous timestep) by surface type
ST_PREV_VARS = make_debug_surface_vars(
    "st", "mm", "Surface wetness (previous timestep) for {}",
    AggregationMethod.AVERAGE, "f104"
)
# Rename to add _prev suffix
for var in ST_PREV_VARS:
    var.name = var.name.replace("_", "_", 1) + "_prev"
    # Fix naming: should be st_Paved_prev not st_Paved_prev (already correct)
    # Actually need to insert _prev before the last part
    parts = var.name.split("_")
    var.name = f"{parts[0]}_{parts[1]}_prev"

# Surface wetness (next timestep) by surface type
ST_NEXT_VARS = make_debug_surface_vars(
    "st", "mm", "Surface wetness (next timestep) for {}",
    AggregationMethod.AVERAGE, "f104"
)
# Rename to add _next suffix
for var in ST_NEXT_VARS:
    parts = var.name.split("_")
    var.name = f"{parts[0]}_{parts[1]}_next"


# Soil moisture store (previous timestep) by surface type
SS_PREV_VARS = make_debug_surface_vars(
    "ss", "mm", "Soil moisture store (previous timestep) for {}",
    AggregationMethod.AVERAGE, "f104"
)
# Rename to add _prev suffix
for var in SS_PREV_VARS:
    parts = var.name.split("_")
    var.name = f"{parts[0]}_{parts[1]}_prev"

# Soil moisture store (next timestep) by surface type
SS_NEXT_VARS = make_debug_surface_vars(
    "ss", "mm", "Soil moisture store (next timestep) for {}",
    AggregationMethod.AVERAGE, "f104"
)
# Rename to add _next suffix
for var in SS_NEXT_VARS:
    parts = var.name.split("_")
    var.name = f"{parts[0]}_{parts[1]}_next"

# Resistance and atmospheric variables (scalars)
RESISTANCE_VARS = [
    OutputVariable(name="RS", unit="s m-1", description="Surface resistance",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="RA_h", unit="s m-1", description="Aerodynamic resistance for heat",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="RB", unit="s m-1", description="Boundary layer resistance",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="RAsnow", unit="s m-1", description="Aerodynamic resistance for snow",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
]

# Surface resistance by surface type
RSS_VARS = make_debug_surface_vars(
    "rss", "s m-1", "Surface resistance for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Volumetric soil moisture deficit by surface type
VSMD_VARS = make_debug_surface_vars(
    "vsmd", "m3 m-3", "Volumetric soil moisture deficit for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Conductance parameters by surface type
COND_S1S2_VARS = make_debug_surface_vars(
    "cond_s1s2", "-", "Conductance parameter S1/G_sm + S2 for {}",
    AggregationMethod.AVERAGE, "f104"
)

COND_GSM_VARS = make_debug_surface_vars(
    "cond_gsm", "-", "Conductance parameter G_sm for {}",
    AggregationMethod.AVERAGE, "f104"
)

COND_CALC_VARS = make_debug_surface_vars(
    "cond_calc", "-", "Conductance calculation term for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Stomatal conductance modifiers by surface type
G_KDOWN_VARS = make_debug_surface_vars(
    "g_kdown", "-", "Kdown modifier for stomatal conductance for {}",
    AggregationMethod.AVERAGE, "f104"
)

G_DQ_VARS = make_debug_surface_vars(
    "g_dq", "-", "Specific humidity deficit modifier for stomatal conductance for {}",
    AggregationMethod.AVERAGE, "f104"
)

G_TA_VARS = make_debug_surface_vars(
    "g_ta", "-", "Air temperature modifier for stomatal conductance for {}",
    AggregationMethod.AVERAGE, "f104"
)

G_SMD_VARS = make_debug_surface_vars(
    "g_smd", "-", "Soil moisture deficit modifier for stomatal conductance for {}",
    AggregationMethod.AVERAGE, "f104"
)

G_LAI_VARS = make_debug_surface_vars(
    "g_lai", "-", "LAI modifier for stomatal conductance for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Additional atmospheric scalars
ATMOS_VARS = [
    OutputVariable(name="vpd_hPa", unit="hPa", description="Vapour pressure deficit",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="lv_J_kg", unit="J kg-1", description="Latent heat of vaporisation",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="avdens", unit="kg m-3", description="Air density",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="avcp", unit="J kg-1 K-1", description="Air specific heat capacity",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="s_hPa", unit="hPa K-1", description="Slope of saturation vapour pressure curve",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="psyc_hPa", unit="hPa K-1", description="Psychrometric constant",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
]

# Iteration and FAI variables
MISC_VARS = [
    OutputVariable(name="i_iter", unit="-", description="Number of iterations in convergence loop",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="FAIBldg_use", unit="-", description="Building frontal area index used",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="FAIEveTree_use", unit="-", description="Evergreen tree frontal area index used",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="FAIDecTree_use", unit="-", description="Deciduous tree frontal area index used",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="FAI", unit="-", description="Total frontal area index",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
    OutputVariable(name="dqndt", unit="W m-2", description="Rate of change of net radiation",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.DEBUG,
                  level=OutputLevel.DEFAULT, format="f104"),
]

# Combine all debug variables
DEBUG_VARIABLES = (
    FLAG_VAR +
    TS_VARS +
    QN_VARS +
    QS_VARS +
    QE0_VARS +
    QE_VARS +
    QH_VARS +
    WU_VARS +
    EV0_VARS +
    EV_VARS +
    DRAIN_VARS +
    ST_PREV_VARS +
    ST_NEXT_VARS +
    SS_PREV_VARS +
    SS_NEXT_VARS +
    RESISTANCE_VARS +
    RSS_VARS +
    VSMD_VARS +
    COND_S1S2_VARS +
    COND_GSM_VARS +
    COND_CALC_VARS +
    G_KDOWN_VARS +
    G_DQ_VARS +
    G_TA_VARS +
    G_SMD_VARS +
    G_LAI_VARS +
    ATMOS_VARS +
    MISC_VARS
)
