"""User-facing configuration for the coupled single-column model (SCM).

Research preview. This block configures the coupled runner
(:func:`supy.scm.run_scm`); it has no representation in the DataFrame
state and does not affect standard (offline) SUEWS runs. Field names
follow the SUEWS naming convention (physical quantity first; category
prefixes for non-physical quantities; string-valued options, never
numeric codes). The accepted ranges mirror the validators in
``supy/scm.py`` and ``suews_phys_scm.f95``.

Only the parameters a user legitimately tunes are exposed here. The
internal closure constants of the K-profile scheme (mixing length,
critical Richardson numbers, counter-gradient and thermal-excess
coefficients, background diffusivity) remain code-level overrides of
:func:`supy.scm.run_scm` — see the documentation page
:doc:`/integration/scm-coupled`.
"""

from enum import Enum
from typing import Optional

from pydantic import BaseModel, ConfigDict, Field, model_validator

from .type import Reference


class SCMStability(str, Enum):
    """Stability function of the stable-conditions closure.

    ``sharp`` reproduces the GABLS1 LES ensemble (Beare et al. 2006);
    ``long_tail`` is required for runs crossing seasons — the sharp
    cut-off decouples the winter stable boundary layer and allows
    unbounded near-surface cooling (cf. Cuxart et al. 2006).
    """

    SHARP = "sharp"
    LONG_TAIL = "long_tail"


class SCMGridConfig(BaseModel):
    """Vertical grid of the atmospheric column."""

    model_config = ConfigDict(title="SCM Vertical Grid")

    thickness_first_layer: float = Field(
        default=20.0,
        gt=0.0,
        description="Thickness of the lowest column layer",
        json_schema_extra={"unit": "m"},
    )
    height_top: float = Field(
        default=3000.0,
        gt=0.0,
        description="Height of the column top",
        json_schema_extra={"unit": "m"},
    )
    ratio_stretch: float = Field(
        default=1.06,
        ge=1.0,
        le=1.5,
        description="Geometric stretching ratio between successive layers",
        json_schema_extra={"unit": "dimensionless"},
    )

    @model_validator(mode="after")
    def _top_above_first_layer(self):
        if self.height_top <= self.thickness_first_layer:
            raise ValueError(
                "scm.grid.height_top must exceed thickness_first_layer"
            )
        return self


class SCMConfig(BaseModel):
    """Coupled single-column model configuration (research preview).

    Used by :func:`supy.scm.run_scm` as the parameter source; keyword
    arguments passed to ``run_scm`` override values given here. Absent
    from the YAML, all parameters take the documented defaults.
    """

    model_config = ConfigDict(title="Single-Column Model (Research Preview)")

    stability: SCMStability = Field(
        default=SCMStability.SHARP,
        description=(
            "Stability function of the stable-conditions closure: 'sharp' "
            "reproduces the GABLS1 LES ensemble; 'long_tail' is required "
            "for multi-season runs (the sharp cut-off decouples the winter "
            "stable boundary layer)"
        ),
        json_schema_extra={"unit": "dimensionless"},
    )
    grid: SCMGridConfig = Field(
        default_factory=SCMGridConfig,
        description="Vertical grid of the atmospheric column",
    )
    height_mixed_layer_init: float = Field(
        default=300.0,
        gt=0.0,
        description="Initial well-mixed layer depth used to construct the "
        "column's starting profiles",
        json_schema_extra={"unit": "m"},
    )
    lapse_rate_theta: float = Field(
        default=0.006,
        gt=0.0,
        le=0.1,
        description="Free-atmosphere potential-temperature lapse rate for "
        "the initial, nudging and anchor profiles",
        json_schema_extra={"unit": "K m^-1"},
    )
    lapse_rate_humidity: float = Field(
        default=-5.0e-7,
        ge=-1.0e-3,
        le=1.0e-3,
        description="Free-atmosphere specific-humidity lapse rate for the "
        "initial, nudging and anchor profiles",
        json_schema_extra={"unit": "kg kg^-1 m^-1"},
    )
    height_nudging_free_atmosphere: float = Field(
        default=1500.0,
        ge=0.0,
        description="Height above which the column is relaxed towards its "
        "initial profiles (single-column stand-in for the large scale)",
        json_schema_extra={"unit": "m"},
    )
    timescale_nudging_free_atmosphere: float = Field(
        default=86400.0,
        gt=0.0,
        description="Relaxation time scale of the free-atmosphere nudging",
        json_schema_extra={"unit": "s"},
    )
    timescale_wind_nudging: float = Field(
        default=1800.0,
        gt=0.0,
        description="Relaxation time scale of the column wind towards a "
        "log profile anchored at the observed speed",
        json_schema_extra={"unit": "s"},
    )
    rate_radiative_cooling: float = Field(
        default=2.0,
        ge=0.0,
        description="Clear-sky longwave radiative cooling applied to the "
        "column (positive = cooling)",
        json_schema_extra={"unit": "K day^-1"},
    )
    length_city: float = Field(
        default=15000.0,
        gt=0.0,
        description="Upstream fetch of the modelled city; sets the "
        "ventilation time scale tau = length_city / wind speed",
        json_schema_extra={"unit": "m"},
    )
    timescale_ventilation_min: float = Field(
        default=900.0,
        gt=0.0,
        description="Lower bound on the ventilation time scale",
        json_schema_extra={"unit": "s"},
    )
    timescale_obs_anchor: float = Field(
        default=0.0,
        ge=0.0,
        description="Synoptic anchor: whole-column relaxation towards a "
        "profile anchored at the observed air state (0 = off; ~86400 for "
        "the rural companion in multi-season runs)",
        json_schema_extra={"unit": "s"},
    )
    count_substeps: int = Field(
        default=5,
        ge=1,
        le=100,
        description="Column substeps per SUEWS timestep",
        json_schema_extra={"unit": "dimensionless"},
    )
    z0m_wind_profile: float = Field(
        default=1.0,
        gt=0.0,
        description="Aerodynamic roughness length of the log profile used "
        "for wind nudging",
        json_schema_extra={"unit": "m"},
    )
    ref: Optional[Reference] = None
