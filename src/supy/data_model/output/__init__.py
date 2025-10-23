"""Output variable definitions for SUEWS.

This module provides Python/Pydantic-first definitions of all SUEWS output variables,
replacing the previous Fortran-first runtime extraction approach.

The registry pattern provides:
- Type-safe variable definitions
- Self-documenting metadata
- Easy extensibility
- Better IDE support

Example usage:
    >>> from supy.data_model.output import OUTPUT_REGISTRY
    >>> # Get all SUEWS core variables
    >>> suews_vars = OUTPUT_REGISTRY.by_group(OutputGroup.SUEWS)
    >>> # Get default output level variables
    >>> default_vars = OUTPUT_REGISTRY.by_level(OutputLevel.DEFAULT)
    >>> # Get aggregation rules for resampling
    >>> agg_rules = OUTPUT_REGISTRY.get_aggregation_rules()
"""

from .variables import (
    OutputVariable,
    OutputVariableRegistry,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)
from .datetime_vars import DATETIME_VARIABLES
from .suews_vars import SUEWS_VARIABLES
from .snow_vars import SNOW_VARIABLES
from .estm_vars import ESTM_VARIABLES
from .rsl_vars import RSL_VARIABLES
from .dailystate_vars import DAILYSTATE_VARIABLES
from .bl_vars import BL_VARIABLES
from .beers_vars import BEERS_VARIABLES
from .debug_vars import DEBUG_VARIABLES

# EHC variables defined inline (import from ehc_vars fails due to Conductor workspace issue)
# TODO: Move back to ehc_vars.py when import issue is resolved
def _make_ehc_layer_vars(base_name, unit, description_template, group=OutputGroup.EHC):
    """Helper to create EHC variables for 15 layers."""
    return [
        OutputVariable(
            name=f"{base_name}{i}",
            unit=unit,
            description=description_template.format(i),
            aggregation=AggregationMethod.AVERAGE,
            group=group,
            level=OutputLevel.DEFAULT,
            format="f104",
        )
        for i in range(1, 16)
    ]

EHC_VARIABLES = (
    # Surface variables
    [
        OutputVariable(name="Tsfc_surf", unit="degK", description="Surface temperature (aggregated)",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.EHC, level=OutputLevel.DEFAULT, format="f104"),
        OutputVariable(name="QS_surf", unit="W m-2", description="Storage heat flux at surface (aggregated)",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.EHC, level=OutputLevel.DEFAULT, format="f104"),
    ] +
    # Roof variables (15 layers each)
    _make_ehc_layer_vars("Tsfc_roof", "degK", "Roof surface temperature layer {}") +
    _make_ehc_layer_vars("Qn_roof", "W m-2", "Roof net radiation layer {}") +
    _make_ehc_layer_vars("QS_roof", "W m-2", "Roof storage heat flux layer {}") +
    _make_ehc_layer_vars("QE_roof", "W m-2", "Roof latent heat flux layer {}") +
    _make_ehc_layer_vars("QH_roof", "W m-2", "Roof sensible heat flux layer {}") +
    _make_ehc_layer_vars("state_roof", "mm", "Roof water state layer {}") +
    _make_ehc_layer_vars("soilstore_roof", "mm", "Roof soil water storage layer {}") +
    # Wall variables (15 layers each)
    _make_ehc_layer_vars("Tsfc_wall", "degK", "Wall surface temperature layer {}") +
    _make_ehc_layer_vars("Qn_wall", "W m-2", "Wall net radiation layer {}") +
    _make_ehc_layer_vars("QS_wall", "W m-2", "Wall storage heat flux layer {}") +
    _make_ehc_layer_vars("QE_wall", "W m-2", "Wall latent heat flux layer {}") +
    _make_ehc_layer_vars("QH_wall", "W m-2", "Wall sensible heat flux layer {}") +
    _make_ehc_layer_vars("state_wall", "mm", "Wall water state layer {}") +
    _make_ehc_layer_vars("soilstore_wall", "mm", "Wall soil water storage layer {}")
)

# SPARTACUS radiation model variables (194 total)
SPARTACUS_VARIABLES = (
    [
        OutputVariable(name="alb", unit="-", description="Albedo",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
        OutputVariable(name="emis", unit="-", description="Emissivity",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
    ] +
    # Spectral bands (4 bands)
    [OutputVariable(name=f"top_dn_dir_sw{i}", unit="W m-2", description=f"Top-of-canopy downward direct SW band {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 5)] +
    [
        OutputVariable(name="sw_up", unit="W m-2", description="Upward SW at top of canopy",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
        OutputVariable(name="top_dn_lw", unit="W m-2", description="Top-of-canopy downward LW",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
        OutputVariable(name="lw_up", unit="W m-2", description="Upward LW at top of canopy",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
        OutputVariable(name="qn", unit="W m-2", description="Net radiation",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
        OutputVariable(name="top_net_sw", unit="W m-2", description="Net SW at top of canopy",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
        OutputVariable(name="top_net_lw", unit="W m-2", description="Net LW at top of canopy",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
        OutputVariable(name="lw_emission", unit="W m-2", description="LW emission",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
    ] +
    [OutputVariable(name=f"grnd_dn_dir_sw{i}", unit="W m-2", description=f"Ground downward direct SW band {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 5)] +
    [
        OutputVariable(name="grnd_vertical_diff", unit="W m-2", description="Ground vertical diffuse",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
        OutputVariable(name="grnd_net_sw", unit="W m-2", description="Ground net SW",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
        OutputVariable(name="grnd_net_lw", unit="W m-2", description="Ground net LW",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
    ] +
    # 15-layer variables for roofs and walls
    [OutputVariable(name=f"roof_in_sw{i}", unit="W m-2", description=f"Roof incoming SW layer {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 16)] +
    [OutputVariable(name=f"roof_net_sw{i}", unit="W m-2", description=f"Roof net SW layer {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 16)] +
    [OutputVariable(name=f"wall_in_sw{i}", unit="W m-2", description=f"Wall incoming SW layer {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 16)] +
    [OutputVariable(name=f"wall_net_sw{i}", unit="W m-2", description=f"Wall net SW layer {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 16)] +
    [
        OutputVariable(name="clear_air_abs_sw", unit="W m-2", description="Clear air absorbed SW",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
    ] +
    [OutputVariable(name=f"roof_in_lw{i}", unit="W m-2", description=f"Roof incoming LW layer {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 16)] +
    [OutputVariable(name=f"roof_net_lw{i}", unit="W m-2", description=f"Roof net LW layer {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 16)] +
    [OutputVariable(name=f"wall_in_lw{i}", unit="W m-2", description=f"Wall incoming LW layer {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 16)] +
    [OutputVariable(name=f"wall_net_lw{i}", unit="W m-2", description=f"Wall net LW layer {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 16)] +
    [OutputVariable(name=f"sfr_roof{i}", unit="-", description=f"Surface fraction roof layer {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 16)] +
    [OutputVariable(name=f"sfr_wall{i}", unit="-", description=f"Surface fraction wall layer {i}",
                   aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                   level=OutputLevel.DEFAULT, format="f104") for i in range(1, 16)] +
    [
        OutputVariable(name="clear_air_abs_lw", unit="W m-2", description="Clear air absorbed LW",
                      aggregation=AggregationMethod.AVERAGE, group=OutputGroup.SPARTACUS,
                      level=OutputLevel.DEFAULT, format="f104"),
    ]
)

# STEBBS variables (57 total - using generic names pending detailed Fortran analysis)
STEBBS_VARIABLES = [
    OutputVariable(name=f"stebbs_var{i}", unit="-", description=f"STEBBS variable {i}",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.STEBBS,
                  level=OutputLevel.DEFAULT, format="f104")
    for i in range(1, 58)
]

# NHOOD variables (1 total)
NHOOD_VARIABLES = [
    OutputVariable(name="nhood_var1", unit="-", description="Neighbourhood variable",
                  aggregation=AggregationMethod.AVERAGE, group=OutputGroup.NHOOD,
                  level=OutputLevel.DEFAULT, format="f104"),
]


# Assemble the global registry from all variable modules
OUTPUT_REGISTRY = OutputVariableRegistry(
    variables=(
        DATETIME_VARIABLES +
        SUEWS_VARIABLES +
        SNOW_VARIABLES +
        ESTM_VARIABLES +
        RSL_VARIABLES +
        DAILYSTATE_VARIABLES +
        BL_VARIABLES +
        BEERS_VARIABLES +
        DEBUG_VARIABLES +
        EHC_VARIABLES +
        SPARTACUS_VARIABLES +
        STEBBS_VARIABLES +
        NHOOD_VARIABLES
    )
)


__all__ = [
    "OUTPUT_REGISTRY",
    "OutputVariable",
    "OutputVariableRegistry",
    "AggregationMethod",
    "OutputGroup",
    "OutputLevel",
]
