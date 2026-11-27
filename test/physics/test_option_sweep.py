"""Breadth sweep over the physics-option space via YAML -> Rust ``SUEWSSimulation``.

The rest of `test/physics/` exercises two to five hand-picked points of the
physics-option space. A single day of the bundled sample configuration costs
about 0.1 s of model time, so running *every* value of *every*
`ModelPhysics` enum -- one option varied at a time from the sample's defaults
-- costs seconds and turns the option space from "the values we happened to
pick" into "the values that exist".

Every case executes through the public `SUEWSSimulation` (YAML / `SUEWSConfig`
-> Rust engine) path rather than the legacy `to_df_state()` / `from_state()`
adapter, reusing session-scoped fixtures from `test/conftest.py` with
defensive copies so shared state is never mutated.

What the sweep asserts for each runnable value:

- all active output groups (`SUEWS`, `BEERS`, `RSL`, `debug`, `DailyState`,
  plus `EHC` / `SPARTACUS` / `STEBBS` / `snow` when enabled) are finite and
  non-sentinel on the exact forcing timestamps, with `SUEWS.Fcld` matching
  supplied forcing and any kernel-documented absent diagnostics remaining
  100% `NaN`, and
- the water balance closes to the same 1e-4 mm bound
  `test_core_physics.test_water_balance_consistency` uses.

Values the sample cannot support are not silently skipped. They fall into
three declared tables, each with its reason:

- `OPTION_VALUES_REFUSED` -- needs a forcing column or input block the sample
  does not carry, or a method combination the kernel cannot run. The single-
  option sweep asserts the refusal rather than dropping the value, and
  `COUPLED_OPTION_RUNS` exercises every one of those 17 refused entries end to
  end with its required paired forcing columns, input blocks, and options
  (`test_coupled_runs_cover_all_refused_options` enforces complete coverage).
- `OPTION_VALUES_XFAIL` -- runs, but violates an invariant above. Recorded
  `xfail(strict=True)` against an open issue, so the entry fails the moment
  the defect is fixed.
- `OPTION_VALUES_REJECTED` -- the data model refuses the value outright, on
  construction and on assignment, so no run is attempted. The sweep asserts
  that rejection and its message.

`test_option_tables_name_real_values` keeps the three tables, the
representative multi-option combination table, and `COUPLED_OPTION_RUNS` from
rotting: a key naming a field, value, or forcing column that no longer exists
fails.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from enum import Enum
from importlib import import_module
from pathlib import Path
import re
import tempfile
from typing import Any, get_args

from conftest import TIMESTEPS_PER_DAY
from layer5_checks import (
    SUEWS_ABSENT_UNDER_OBSERVED_QN,
    SUEWS_ABSENT_UNDER_OBSERVED_SMD,
    assert_forcing_cloud,
    assert_simulation_output_finiteness,
    expected_dailystate_mask,
    strict_finite_array,
)
import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model import RefValue, SUEWSConfig
from supy.data_model.core.model import (
    EHCStabilityControl,
    EHCSurfaceCoupling,
    EmissionsMethod,
    FAIMethod,
    GSModel,
    HeatRoughnessMethod,
    KdownSplitMethod,
    LAIMethod,
    ModelPhysics,
    MomentumRoughnessMethod,
    NetRadiationMethod,
    OhmIncQf,
    RSLLevel,
    RSLMethod,
    SMDMethod,
    StabilityMethod,
    StorageHeatMethod,
    WaterUseMethod,
)
from supy.data_model.core.site import SoilObservationConfig
from supy.data_model.forcing import FORCING_REGISTRY

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-BALANCE", "C-RESPONSE")]

# Same bound as test_core_physics.test_water_balance_consistency: the closure
# is exact by construction, so this is a numerical-noise margin, not a
# physical tolerance.
WATER_CLOSURE_TOLERANCE_MM = 1e-4

# Canonical surface order matching Fortran (PavSurf..WaterSurf) and debug
# soil-storage columns (`ss_<Surface>_next`).
_SURFACE_ORDER: tuple[tuple[str, str], ...] = (
    ("paved", "Paved"),
    ("bldgs", "Bldgs"),
    ("evetr", "EveTr"),
    ("dectr", "DecTr"),
    ("grass", "Grass"),
    ("bsoil", "BSoil"),
    ("water", "Water"),
)

# Values needing missing inputs or an implemented method combination.
# Each maps to a fragment of the message the model refuses it with.
OPTION_VALUES_REFUSED = {
    ("net_radiation", 0): "forcing column 'qn'",
    ("net_radiation", 1): "forcing column 'ldown'",
    ("net_radiation", 2): "forcing column 'fcld'",
    ("net_radiation", 11): "forcing column 'ldown'",
    ("net_radiation", 12): "forcing column 'fcld'",
    ("net_radiation", 100): "forcing column 'ldown'",
    ("net_radiation", 200): "forcing column 'fcld'",
    # SPARTACUS with buildings needs a facet-resolved storage heat method
    # (gh#1834) and zero vegetation above tree crowns, which the sample's
    # storage_heat=1 and vertical layers do not satisfy; varying one option
    # at a time cannot satisfy the paired rules.
    ("net_radiation", 1001): "SPARTACUS with buildings",
    ("net_radiation", 1002): "SPARTACUS with buildings",
    ("net_radiation", 1003): "SPARTACUS with buildings",
    ("emissions", 0): "forcing column 'qf'",
    ("storage_heat", 0): "forcing column 'qs'",
    # STEBBS storage heat needs SPARTACUS-Surface net radiation, which the
    # sample's net_radiation=3 is not; varying one option at a time cannot
    # satisfy the pair.
    (
        "storage_heat",
        7,
    ): "requires model.physics.net_radiation to be a SPARTACUS-Surface method",
    # Observed soil moisture requires a soil_observation input block before the
    # forcing check runs, which the bundled sample does not carry.
    (
        "soil_moisture",
        1,
    ): "Observed soil moisture requires soil_observation layer bounds",
    ("water_use", 1): "forcing column 'wuh'",
    ("laimethod", 0): "forcing column 'lai'",
    # W16 has no phi implementation for the sample's RSL path (GH#1784).
    ("stability", 2): "stability=2 (W16) has no phi implementation for RSL",
}

# Values that run and then violate one of the sweep's invariants.
OPTION_VALUES_XFAIL = {
    ("snow_use", 1): (
        "water balance does not close (max residual ~0.45 mm): snow-free "
        "land skips its water storage on the snow path (UMEP-dev/SUEWS#1905), "
        "open-water evaporation is not removed from the water-body store "
        "(UMEP-dev/SUEWS#1927), and this check omits snow and melt-water "
        "stores (UMEP-dev/SUEWS#1958)"
    ),
}

# Values the data model rejects at configuration, before any run.
OPTION_VALUES_REJECTED = {
    # ESTM's surface-temperature input has no YAML or forcing path; before
    # gh#1785 the kernel read past a zero-length array and segfaulted.
    ("storage_heat", 4): "storage_heat=4 (ESTM) is not available",
    # Reserved stability codes have no stability function behind them; before
    # gh#1783 the RSL profile returned NaN T2/RH2/Q2/U10 without a warning.
    ("stability", 0): "stability=0 is a reserved code",
    ("stability", 1): "stability=1 is a reserved code",
    # Momentum roughness code 5 (ALTERNATIVE) has no formulation in
    # SUEWS_cal_RoughnessParameters; before gh#2105 z0m_zh/zdm_zh remained
    # unassigned in the kernel.
    (
        "roughness_length_momentum",
        5,
    ): "roughness_length_momentum=5 (ALTERNATIVE) is not available",
}

# Representative valid multi-option physics combinations exercised end-to-end
# through on-disk YAML -> SUEWSSimulation -> Rust on the bundled 1-day forcing.
# Coupled runs for OPTION_VALUES_REFUSED entries live in COUPLED_OPTION_RUNS
# below; cold-snow / 7-surface / observed-sm anchors live in follow-up slices
# of #2100.
REPRESENTATIVE_OPTION_COMBINATIONS: dict[str, dict[str, Enum]] = {
    "ehc_implicit_macdonald_ward": {
        "storage_heat": StorageHeatMethod.EHC,
        "ehc_surface_coupling": EHCSurfaceCoupling.IMPLICIT,
        "control_ehc_stability": EHCStabilityControl.RELAX,
        "roughness_length_momentum": MomentumRoughnessMethod.MACDONALD,
        "surface_conductance": GSModel.WARD,
    },
    "ehc_implicit_secant_kawai_most": {
        "storage_heat": StorageHeatMethod.EHC,
        "ehc_surface_coupling": EHCSurfaceCoupling.IMPLICIT,
        "control_ehc_stability": EHCStabilityControl.SECANT,
        "roughness_length_heat": HeatRoughnessMethod.KAWAI,
        "roughness_sublayer": RSLMethod.MOST,
    },
    "dyohm_biogen_co2_reindl": {
        "storage_heat": StorageHeatMethod.DyOHM,
        "emissions": EmissionsMethod.BIOGEN_RECT_J11,
        "kdown_split_method": KdownSplitMethod.REINDL,
        "roughness_sublayer_level": RSLLevel.DETAILED,
    },
    "dyohm_building_jarvi_businger": {
        "storage_heat": StorageHeatMethod.DyOHM_BUILDING,
        "surface_conductance": GSModel.JARVI,
        "stability": StabilityMethod.BUSINGER_HOEGSTROM,
        "roughness_length_heat": HeatRoughnessMethod.ADAPTIVE,
    },
    "anohm_lambdap_fai_modelled": {
        "storage_heat": StorageHeatMethod.ANOHM,
        "roughness_length_momentum": MomentumRoughnessMethod.LAMBDAP_DEPENDENT,
        "frontal_area_index": FAIMethod.MODELLED,
        "roughness_sublayer": RSLMethod.RST,
    },
    "narp_zenith_ohm_inc_qf": {
        "net_radiation": NetRadiationMethod.LDOWN_AIR_ZENITH,
        "ohm_inc_qf": OhmIncQf.INCLUDE,
        "kdown_split_method": KdownSplitMethod.PEREZ,
        "emissions": EmissionsMethod.L11,
    },
}

# Realistic synthetic 1-day forcing constants for COUPLED_OPTION_RUNS, chosen
# strictly inside FORCING_REGISTRY validation ranges and consistent with the
# bundled winter London sample day.
_STEFAN_BOLTZMANN_W_M2_K4 = 5.670374419e-8
_COUPLED_LDOWN_EMISSIVITY = 0.85
_COUPLED_QN_NIGHT_OFFSET_W_M2 = -25.0
_COUPLED_QN_KDOWN_SLOPE = 0.65
_COUPLED_QS_NIGHT_OFFSET_W_M2 = -10.0
_COUPLED_QS_KDOWN_SLOPE = 0.25
_COUPLED_FCLD_CYCLE = (0.2, 0.5, 0.8, 0.4)
_COUPLED_QF_W_M2 = 25.0
_COUPLED_WUH_MM_PER_STEP = 0.005
_COUPLED_SM_KG_M2 = 120.0
_COUPLED_LAI_M2_M2 = 1.5
# Sample active non-water surfaces carry soil_depth=350 mm and
# soil_store_capacity=150 kg m-2, so fc=0.5 gives fc*d=175 >= 150 kg m-2.
_COUPLED_SOIL_FIELD_CAPACITY = 0.5
_COUPLED_SOIL_NOT_ROCKS = 0.8
_COUPLED_SOIL_BULK_DENSITY_KG_M3 = 1250.0


@dataclass(frozen=True)
class CoupledOptionCase:
    """One coupled 1-day configuration covering refused single-option entries."""

    case_id: str
    covers: tuple[tuple[str, Enum], ...]
    physics_overrides: Mapping[str, Enum]
    forcing_columns: tuple[str, ...] = ()
    enable_stebbs: bool = False
    spartacus_vertical_layers: bool = False
    soil_observation: bool = False


# Coupled 1-day runs exercising every entry of OPTION_VALUES_REFUSED with its
# required paired forcing columns, input blocks, and physics options.
# SPARTACUS methods (1001-1003) are paired with STEBBS (storage_heat=7) rather
# than EHC (storage_heat=5) while #2196 remains open.
COUPLED_OPTION_RUNS: tuple[CoupledOptionCase, ...] = (
    CoupledOptionCase(
        case_id="net_radiation_observed",
        covers=(("net_radiation", NetRadiationMethod.OBSERVED),),
        physics_overrides={"net_radiation": NetRadiationMethod.OBSERVED},
        forcing_columns=("qn",),
    ),
    CoupledOptionCase(
        case_id="net_radiation_ldown_observed",
        covers=(("net_radiation", NetRadiationMethod.LDOWN_OBSERVED),),
        physics_overrides={"net_radiation": NetRadiationMethod.LDOWN_OBSERVED},
        forcing_columns=("ldown",),
    ),
    CoupledOptionCase(
        case_id="net_radiation_ldown_cloud",
        covers=(("net_radiation", NetRadiationMethod.LDOWN_CLOUD),),
        physics_overrides={"net_radiation": NetRadiationMethod.LDOWN_CLOUD},
        forcing_columns=("fcld",),
    ),
    CoupledOptionCase(
        case_id="net_radiation_ldown_surface",
        covers=(("net_radiation", NetRadiationMethod.LDOWN_SURFACE),),
        physics_overrides={"net_radiation": NetRadiationMethod.LDOWN_SURFACE},
        forcing_columns=("ldown",),
    ),
    CoupledOptionCase(
        case_id="net_radiation_ldown_cloud_surface",
        covers=(("net_radiation", NetRadiationMethod.LDOWN_CLOUD_SURFACE),),
        physics_overrides={"net_radiation": NetRadiationMethod.LDOWN_CLOUD_SURFACE},
        forcing_columns=("fcld",),
    ),
    CoupledOptionCase(
        case_id="net_radiation_ldown_zenith",
        covers=(("net_radiation", NetRadiationMethod.LDOWN_ZENITH),),
        physics_overrides={"net_radiation": NetRadiationMethod.LDOWN_ZENITH},
        forcing_columns=("ldown",),
    ),
    CoupledOptionCase(
        case_id="net_radiation_ldown_cloud_zenith",
        covers=(("net_radiation", NetRadiationMethod.LDOWN_CLOUD_ZENITH),),
        physics_overrides={"net_radiation": NetRadiationMethod.LDOWN_CLOUD_ZENITH},
        forcing_columns=("fcld",),
    ),
    CoupledOptionCase(
        case_id="spartacus_ldown_observed_stebbs",
        covers=(
            ("net_radiation", NetRadiationMethod.LDOWN_SS_OBSERVED),
            ("storage_heat", StorageHeatMethod.STEBBS),
        ),
        physics_overrides={
            "net_radiation": NetRadiationMethod.LDOWN_SS_OBSERVED,
            "storage_heat": StorageHeatMethod.STEBBS,
        },
        forcing_columns=("ldown",),
        enable_stebbs=True,
        spartacus_vertical_layers=True,
    ),
    CoupledOptionCase(
        case_id="spartacus_ldown_cloud_stebbs",
        covers=(
            ("net_radiation", NetRadiationMethod.LDOWN_SS_CLOUD),
            ("storage_heat", StorageHeatMethod.STEBBS),
        ),
        physics_overrides={
            "net_radiation": NetRadiationMethod.LDOWN_SS_CLOUD,
            "storage_heat": StorageHeatMethod.STEBBS,
        },
        forcing_columns=("fcld",),
        enable_stebbs=True,
        spartacus_vertical_layers=True,
    ),
    CoupledOptionCase(
        case_id="spartacus_ldown_air_stebbs",
        covers=(
            ("net_radiation", NetRadiationMethod.LDOWN_SS_AIR),
            ("storage_heat", StorageHeatMethod.STEBBS),
        ),
        physics_overrides={
            "net_radiation": NetRadiationMethod.LDOWN_SS_AIR,
            "storage_heat": StorageHeatMethod.STEBBS,
        },
        enable_stebbs=True,
        spartacus_vertical_layers=True,
    ),
    CoupledOptionCase(
        case_id="emissions_observed",
        covers=(("emissions", EmissionsMethod.OBSERVED),),
        physics_overrides={"emissions": EmissionsMethod.OBSERVED},
        forcing_columns=("qf",),
    ),
    CoupledOptionCase(
        case_id="storage_heat_observed",
        covers=(("storage_heat", StorageHeatMethod.OBSERVED),),
        physics_overrides={"storage_heat": StorageHeatMethod.OBSERVED},
        forcing_columns=("qs",),
    ),
    CoupledOptionCase(
        case_id="soil_moisture_observed",
        covers=(("soil_moisture", SMDMethod.OBSERVED),),
        physics_overrides={"soil_moisture": SMDMethod.OBSERVED},
        forcing_columns=("sm",),
        soil_observation=True,
    ),
    CoupledOptionCase(
        case_id="water_use_observed",
        covers=(("water_use", WaterUseMethod.OBSERVED),),
        physics_overrides={"water_use": WaterUseMethod.OBSERVED},
        forcing_columns=("Wuh",),
    ),
    CoupledOptionCase(
        case_id="lai_observed",
        covers=(("laimethod", LAIMethod.OBSERVED),),
        physics_overrides={"laimethod": LAIMethod.OBSERVED},
        forcing_columns=("lai",),
    ),
    CoupledOptionCase(
        case_id="stability_w16_most",
        covers=(("stability", StabilityMethod.HOEGSTROM),),
        physics_overrides={
            "stability": StabilityMethod.HOEGSTROM,
            "roughness_sublayer": RSLMethod.MOST,
        },
    ),
)


def _unwrap_value(value: Any) -> Any:
    """Unwrap ``RefValue`` and ``Enum`` wrappers to the underlying primitive."""
    inner = value.value if isinstance(value, RefValue) else value
    return inner.value if isinstance(inner, Enum) else inner


def _option_label(value: Any) -> str:
    """Return a readable name for an Enum, ``RefValue[Enum]``, or stub value."""
    inner = value.value if isinstance(value, RefValue) else value
    return str(getattr(inner, "name", inner))


def _enum_class(annotation):
    """The Enum behind a `ModelPhysics` field annotation, or None.

    `FlexibleRefValue(X)` wraps the enum in a union with `RefValue[X]`, so the
    union members are searched as well as the annotation itself.
    """
    for candidate in (*get_args(annotation), annotation):
        if isinstance(candidate, type) and issubclass(candidate, Enum):
            return candidate
    return None


def _physics_option_enums():
    """(field name, enum class) for every enum-valued `ModelPhysics` field.

    Derived from the model rather than restated, so a new option joins the
    sweep the moment it is added. The nested `stebbs` block is a sub-model,
    not an enum field, and is out of scope: its switches only bite when
    `storage_heat` selects STEBBS (7), which the sample cannot reach by
    varying one option at a time.
    """
    for name, field in ModelPhysics.model_fields.items():
        enum_cls = _enum_class(field.annotation)
        if enum_cls is not None:
            yield name, enum_cls


def _sweep_params():
    """One pytest param per (field, enum member) that the sweep can run."""
    for field, enum_cls in _physics_option_enums():
        for member in enum_cls:
            key = (field, member.value)
            if key in OPTION_VALUES_REFUSED or key in OPTION_VALUES_REJECTED:
                continue
            marks = []
            if key in OPTION_VALUES_XFAIL:
                marks.append(
                    pytest.mark.xfail(strict=True, reason=OPTION_VALUES_XFAIL[key])
                )
            yield pytest.param(field, member, marks=marks, id=f"{field}={member.name}")


def _refusal_params():
    """One pytest param per declared refusal, with its expected message."""
    for field, enum_cls in _physics_option_enums():
        for member in enum_cls:
            fragment = OPTION_VALUES_REFUSED.get((field, member.value))
            if fragment is not None:
                yield pytest.param(field, member, fragment, id=f"{field}={member.name}")


def _rejected_params():
    """One pytest param per declared configuration-time rejection."""
    for field, enum_cls in _physics_option_enums():
        for member in enum_cls:
            fragment = OPTION_VALUES_REJECTED.get((field, member.value))
            if fragment is not None:
                yield pytest.param(field, member, fragment, id=f"{field}={member.name}")


@pytest.fixture(scope="module")
def sample_sweep_base(sample_config_loaded, sample_data_loaded):
    """Read-only `(sample_config, one-day df_forcing)` built from session fixtures.

    Reuses the session-scoped `sample_config_loaded` and `sample_data_loaded`
    fixtures without calling `to_df_state()`. Every consumer copies before
    mutating (`build_sweep_config` / `patch_physics` deep-copy the config and
    `run_simulation` copies the forcing slice).
    """
    _, df_forcing = sample_data_loaded
    return sample_config_loaded, df_forcing.iloc[:TIMESTEPS_PER_DAY].copy()


def build_sweep_config(
    base_config: SUEWSConfig,
    physics_overrides: Mapping[str, Any],
) -> SUEWSConfig:
    """Return a defensive copy of ``base_config`` with validated physics overrides."""
    patched = base_config.model_copy(deep=True)
    for field, value in physics_overrides.items():
        setattr(patched.model.physics, field, value)
    return patched


def _apply_coupled_forcing(
    base_forcing: pd.DataFrame,
    columns: tuple[str, ...],
) -> pd.DataFrame:
    """Return a defensive forcing copy populated with realistic synthetic columns."""
    forcing = base_forcing.copy()
    n_steps = len(forcing)
    kdown = forcing["kdown"].to_numpy(dtype=float)
    tair_k = forcing["Tair"].to_numpy(dtype=float) + 273.15
    generators: dict[str, np.ndarray] = {
        "qn": _COUPLED_QN_NIGHT_OFFSET_W_M2 + _COUPLED_QN_KDOWN_SLOPE * kdown,
        "ldown": _COUPLED_LDOWN_EMISSIVITY * _STEFAN_BOLTZMANN_W_M2_K4 * (tair_k**4),
        "fcld": np.resize(_COUPLED_FCLD_CYCLE, n_steps),
        "qf": np.full(n_steps, _COUPLED_QF_W_M2, dtype=float),
        "qs": _COUPLED_QS_NIGHT_OFFSET_W_M2 + _COUPLED_QS_KDOWN_SLOPE * kdown,
        "sm": np.full(n_steps, _COUPLED_SM_KG_M2, dtype=float),
        "Wuh": np.full(n_steps, _COUPLED_WUH_MM_PER_STEP, dtype=float),
        "lai": np.full(n_steps, _COUPLED_LAI_M2_M2, dtype=float),
    }
    for col in columns:
        if col not in generators:
            raise ValueError(f"Unsupported coupled forcing column {col!r}")
        forcing[col] = generators[col]
    return forcing


def build_coupled_case(
    base_config: SUEWSConfig,
    base_forcing: pd.DataFrame,
    case: CoupledOptionCase,
) -> tuple[SUEWSConfig, pd.DataFrame]:
    """Build defensive ``(SUEWSConfig, df_forcing)`` inputs for one coupled case."""
    config = build_sweep_config(base_config, case.physics_overrides)
    if case.enable_stebbs:
        config.model.physics.stebbs.enabled = True
    if case.spartacus_vertical_layers:
        for site in config.sites:
            # The top vertical layer in the sample sits above both tree crowns.
            layers = site.properties.vertical_layers
            layers.veg_frac.value[-1] = 0.0
            layers.veg_scale.value[-1] = 0.0
    if case.soil_observation:
        non_water = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil")
        for site in config.sites:
            land_cover = site.properties.land_cover
            active_depths_mm = [
                float(_unwrap_value(getattr(land_cover, name).soil_depth))
                for name in non_water
                if float(_unwrap_value(getattr(land_cover, name).sfr)) > 0.0
            ]
            assert active_depths_mm, "Sample site must have active non-water surfaces"
            layer_depth_m = active_depths_mm[0] / 1000.0
            assert all(
                np.isclose(depth / 1000.0, layer_depth_m, rtol=0.0, atol=1e-6)
                for depth in active_depths_mm
            ), f"Active non-water soil_depth values differ: {active_depths_mm}"
            site.properties.soil_observation = SoilObservationConfig(
                layer_top=0.0,
                layer_depth=layer_depth_m,
                soil_not_rocks=_COUPLED_SOIL_NOT_ROCKS,
                bulk_density=_COUPLED_SOIL_BULK_DENSITY_KG_M3,
            )
            for name in non_water:
                surface = getattr(land_cover, name)
                if float(_unwrap_value(surface.sfr)) > 0.0:
                    surface.soil_field_capacity = RefValue(_COUPLED_SOIL_FIELD_CAPACITY)
    forcing = _apply_coupled_forcing(base_forcing, case.forcing_columns)
    return config, forcing


def patch_physics(
    base_source: SUEWSConfig | pd.DataFrame,
    config: SUEWSConfig | None,
    field: str,
    value: Any,
) -> SUEWSConfig:
    """Return a defensive `SUEWSConfig` copy with one physics option changed.

    Accepts a `SUEWSConfig` as `base_source` (the primary YAML -> Rust sweep
    path) or falls back to `config` when a contract-control test passes a
    legacy `(df_state, df_forcing)` pair. Assignment onto `ModelPhysics`
    triggers Pydantic `validate_assignment=True`.
    """
    source = base_source if isinstance(base_source, SUEWSConfig) else config
    if not isinstance(source, SUEWSConfig):
        raise TypeError(
            "patch_physics requires a SUEWSConfig via base_source or config"
        )
    return build_sweep_config(source, {field: value})


def run_simulation(
    df_forcing: pd.DataFrame,
    config_or_yaml: SUEWSConfig | Path | str,
    *,
    serial_mode: bool = True,
    round_trip_yaml: bool = False,
    work_dir: Path | None = None,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Execute one sweep case through public ``SUEWSSimulation`` -> Rust engine.

    When ``round_trip_yaml=True``, serialises a ``SUEWSConfig`` to an on-disk
    YAML file and loads it back through ``SUEWSSimulation.update_config(path)``
    so the full YAML parser, validator, and Rust engine boundary are exercised.
    """
    forcing = df_forcing.copy()
    sim = SUEWSSimulation()
    if isinstance(config_or_yaml, (str, Path)):
        sim.update_config(Path(config_or_yaml), auto_load_forcing=False)
    elif isinstance(config_or_yaml, SUEWSConfig):
        if round_trip_yaml:
            if work_dir is not None:
                yaml_path = Path(work_dir) / "sweep_case.yml"
                config_or_yaml.to_yaml(yaml_path)
                sim.update_config(yaml_path, auto_load_forcing=False)
            else:
                with tempfile.TemporaryDirectory(prefix="suews_sweep_") as tmp_dir:
                    yaml_path = Path(tmp_dir) / "sweep_case.yml"
                    config_or_yaml.to_yaml(yaml_path)
                    sim.update_config(yaml_path, auto_load_forcing=False)
        else:
            sim.update_config(
                config_or_yaml.model_copy(deep=True),
                auto_load_forcing=False,
            )
    else:
        raise TypeError(
            f"run_simulation expects a SUEWSConfig or YAML path, got {type(config_or_yaml)!r}"
        )

    sim.update_forcing(forcing)
    output = sim.run(
        start_date=forcing.index[0],
        end_date=forcing.index[-1],
        n_jobs=1 if serial_mode else -1,
    )
    return output.df, output.state_final


def _extract_surface_fractions(
    df_state_or_config: SUEWSConfig | pd.DataFrame,
    *,
    grid: int,
) -> np.ndarray:
    """Return the 7 surface fractions for ``grid`` in canonical Fortran order."""
    if isinstance(df_state_or_config, SUEWSConfig):
        matching = [
            site
            for site in df_state_or_config.sites
            if int(_unwrap_value(site.gridiv)) == int(grid)
        ]
        assert len(matching) == 1, (
            f"Expected exactly one site for grid={grid}, found {len(matching)}"
        )
        land_cover = matching[0].properties.land_cover
        weights = np.array(
            [
                float(_unwrap_value(getattr(land_cover, field).sfr))
                for field, _ in _SURFACE_ORDER
            ],
            dtype=float,
        )
        invalid_names = [
            field
            for (field, _), weight in zip(_SURFACE_ORDER, weights, strict=True)
            if not np.isfinite(weight) or weight == -999
        ]
        assert not invalid_names, f"Invalid sfr_surf at grid={grid}: {invalid_names}"
        return weights

    fractions = df_state_or_config["sfr_surf"].loc[grid, [f"({i},)" for i in range(7)]]
    weights = fractions.to_numpy(dtype=float)
    invalid_fractions = fractions.index[~np.isfinite(weights) | (weights == -999)]
    assert invalid_fractions.empty, (
        f"Invalid sfr_surf at grid={grid}: {invalid_fractions.tolist()}"
    )
    return weights


def water_balance_residual_mm(
    df_output: pd.DataFrame,
    df_state: SUEWSConfig | pd.DataFrame,
    grid: int = 1,
) -> float:
    """Largest per-timestep water-balance residual, in mm.

    P + I = E + RO + dS, with storage taken as the surface-fraction-weighted
    soil store from the debug group plus the surface water state -- the same
    accounting `test_core_physics.test_water_balance_consistency` uses.
    Accepts either a `SUEWSConfig` (YAML -> Rust sweep path) or a `df_state`
    DataFrame (used by `test_core_physics.py` and assertion-control tests).
    """
    columns = [("SUEWS", name) for name in ("State", "Rain", "Irr", "Evap", "RO")]
    columns += [("debug", f"ss_{surface}_next") for _, surface in _SURFACE_ORDER]
    missing = [col for col in columns if col not in df_output.columns]
    assert not missing, f"Missing water balance columns for grid={grid}: {missing}"
    water = df_output.loc[grid, columns]
    assert len(water) >= 2 and water.index.is_unique, (
        f"Water balance needs unique intervals for grid={grid}: {water.index.tolist()}"
    )
    assert water.index.is_monotonic_increasing, (
        f"Water balance times are not chronological for grid={grid}"
    )
    values = water.to_numpy(dtype=float)
    invalid = np.argwhere(~np.isfinite(values) | (values == -999))
    assert not invalid.size, "Invalid water balance input: " + "; ".join(
        f"{columns[col]} at grid={grid}, time={water.index[row]}: {values[row, col]}"
        for row, col in invalid[:5]
    )
    weights = _extract_surface_fractions(df_state, grid=grid)
    suews = water["SUEWS"]
    soil_store = water["debug"].dot(weights)
    total_store = soil_store + suews.State
    net_input = suews.Rain + suews.Irr - suews.Evap - suews.RO
    # Only the first output has no predecessor; inputs were checked above.
    return float((total_store.diff().iloc[1:] - net_input.iloc[1:]).abs().max())


def assert_sweep_invariants(
    df_output: pd.DataFrame,
    df_forcing: pd.DataFrame,
    state_or_config: SUEWSConfig | pd.DataFrame,
    *,
    case_label: str,
    grid: int = 1,
    check_all_active_groups: bool = False,
) -> float:
    """Assert timestamp alignment, active finiteness, cloud parity, and water closure."""
    actual_times = df_output.loc[grid].index
    assert actual_times.equals(df_forcing.index), (
        f"Incomplete or misordered water output rows for grid={grid}: "
        f"missing={df_forcing.index.difference(actual_times).tolist()}, "
        f"unexpected={actual_times.difference(df_forcing.index).tolist()}"
    )
    residual = water_balance_residual_mm(df_output, state_or_config, grid=grid)
    suews = df_output.loc[grid, "SUEWS"]
    assert "Fcld" in suews, "SUEWS: missing required output column Fcld"
    assert_forcing_cloud(suews["Fcld"], df_forcing)
    suews_absent: tuple[str, ...] = ()
    if isinstance(state_or_config, SUEWSConfig):
        physics = state_or_config.model.physics
        if (
            int(_unwrap_value(physics.net_radiation)) == 0
            and int(_unwrap_value(physics.snow_use)) == 0
        ):
            suews_absent += SUEWS_ABSENT_UNDER_OBSERVED_QN
        if int(_unwrap_value(physics.soil_moisture)) == 1:
            suews_absent += SUEWS_ABSENT_UNDER_OBSERVED_SMD
    if suews_absent:
        unexpectedly_populated = [
            col for col in suews_absent if not suews[col].isna().all()
        ]
        assert not unexpectedly_populated, (
            f"SUEWS: columns declared absent actually hold values: "
            f"{unexpectedly_populated}"
        )
    strict_finite_array(suews.drop(columns=["Fcld", *suews_absent]), label="SUEWS")

    if check_all_active_groups and isinstance(state_or_config, SUEWSConfig):
        physics = state_or_config.model.physics
        matching = [
            site
            for site in state_or_config.sites
            if int(_unwrap_value(site.gridiv)) == int(grid)
        ]
        assert len(matching) == 1, (
            f"Expected exactly one site for grid={grid}, found {len(matching)}"
        )
        site = matching[0]
        tstep = int(_unwrap_value(state_or_config.model.control.tstep))
        nlayer = int(_unwrap_value(site.properties.vertical_layers.nlayer))
        has_buildings = float(_unwrap_value(site.properties.land_cover.bldgs.sfr)) > 0.0
        stebbs_enabled = bool(_unwrap_value(physics.stebbs.enabled))
        assert_simulation_output_finiteness(
            df_output,
            expected_grids=(grid,),
            expected_timestamps=df_forcing.index,
            tstep=tstep,
            storage_heat=int(_unwrap_value(physics.storage_heat)),
            net_radiation=int(_unwrap_value(physics.net_radiation)),
            snow_use=int(_unwrap_value(physics.snow_use)),
            soil_moisture=int(_unwrap_value(physics.soil_moisture)),
            stebbs_enabled=stebbs_enabled,
            nlayer=nlayer,
            has_buildings=has_buildings,
            forcing=df_forcing,
        )

    assert residual < WATER_CLOSURE_TOLERANCE_MM, (
        f"{case_label}: water balance not closed, max residual {residual:.3e} mm"
    )
    return residual


_W16_MOST_REFERENCE = Path(__file__).resolve().parents[1] / "fixtures" / "w16_most.csv"


def _expected_cloud_ldown_w_m2(df_forcing: pd.DataFrame) -> np.ndarray:
    """Independent Prata cloud-corrected incoming longwave expectation [W m-2]."""
    cloud = df_forcing["fcld"].to_numpy(dtype=float)
    tair = df_forcing["Tair"].to_numpy(dtype=float)
    rh = df_forcing["RH"].to_numpy(dtype=float)
    pressure = df_forcing["pres"].to_numpy(dtype=float)
    temp_k = tair + 273.15
    sat_hpa = 6.1121 * np.exp((18.678 - tair / 234.5) * tair / (tair + 257.14))
    sat_hpa *= 1.00072 + (pressure / 10.0) * (3.2e-6 + 5.9e-10 * tair**2)
    water = 46.5 * (rh / 100.0) * sat_hpa / temp_k
    clear_emissivity = 1.0 - (1.0 + water) * np.exp(-np.sqrt(1.2 + 3.0 * water))
    return (
        _STEFAN_BOLTZMANN_W_M2_K4
        * (temp_k**4)
        * (clear_emissivity + (1.0 - clear_emissivity) * cloud)
    )


def _assert_coupled_option_response(
    df_output: pd.DataFrame,
    df_forcing: pd.DataFrame,
    case_config: SUEWSConfig,
    case: CoupledOptionCase,
    *,
    grid: int = 1,
) -> None:
    """Confirm paired forcing inputs and physics options actually engaged."""
    grid_df = df_output.loc[grid]
    suews = grid_df["SUEWS"]
    if "qn" in case.forcing_columns:
        np.testing.assert_allclose(
            suews["QN"].to_numpy(dtype=float),
            df_forcing["qn"].to_numpy(dtype=float),
            rtol=0.0,
            atol=1e-9,
        )
    if "ldown" in case.forcing_columns:
        np.testing.assert_allclose(
            suews["Ldown"].to_numpy(dtype=float),
            df_forcing["ldown"].to_numpy(dtype=float),
            rtol=0.0,
            atol=1e-9,
        )
    if "fcld" in case.forcing_columns:
        np.testing.assert_allclose(
            suews["Ldown"].to_numpy(dtype=float),
            _expected_cloud_ldown_w_m2(df_forcing),
            rtol=0.0,
            atol=1e-3,
            err_msg=(
                f"{case.case_id}: Ldown does not match Prata cloud-corrected "
                "expectation from forcing fcld"
            ),
        )
    if "qf" in case.forcing_columns:
        np.testing.assert_allclose(
            suews["QF"].to_numpy(dtype=float),
            df_forcing["qf"].to_numpy(dtype=float),
            rtol=0.0,
            atol=1e-9,
        )
    if "qs" in case.forcing_columns:
        np.testing.assert_allclose(
            suews["QS"].to_numpy(dtype=float),
            df_forcing["qs"].to_numpy(dtype=float),
            rtol=0.0,
            atol=1e-9,
        )
    if "Wuh" in case.forcing_columns:
        np.testing.assert_allclose(
            suews["Irr"].to_numpy(dtype=float),
            df_forcing["Wuh"].to_numpy(dtype=float),
            rtol=0.0,
            atol=1e-9,
        )
    if "sm" in case.forcing_columns:
        site = next(
            s for s in case_config.sites if int(_unwrap_value(s.gridiv)) == int(grid)
        )
        land_cover = site.properties.land_cover
        non_water = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil")
        area = 0.0
        field_content = 0.0
        for name in non_water:
            surface = getattr(land_cover, name)
            fraction = float(_unwrap_value(surface.sfr))
            if fraction <= 0.0:
                continue
            area += fraction
            field_content += (
                fraction
                * float(_unwrap_value(surface.soil_field_capacity))
                * float(_unwrap_value(surface.soil_depth))
            )
        expected_smd = (field_content / area) - df_forcing["sm"].to_numpy(dtype=float)
        np.testing.assert_allclose(
            suews["SMD"].to_numpy(dtype=float),
            expected_smd,
            rtol=0.0,
            atol=1e-9,
        )
    if "lai" in case.forcing_columns:
        tstep = int(_unwrap_value(case_config.model.control.tstep))
        eod_mask = expected_dailystate_mask(df_forcing.index, tstep)
        ds_eod = grid_df["DailyState"].loc[eod_mask]
        for col in ("LAI_EveTr", "LAI_DecTr", "LAI_Grass"):
            np.testing.assert_allclose(
                ds_eod[col].to_numpy(dtype=float),
                _COUPLED_LAI_M2_M2,
                rtol=0.0,
                atol=1e-9,
            )
    if case.physics_overrides.get("roughness_sublayer") == RSLMethod.MOST:
        np.testing.assert_array_equal(
            grid_df["RSL"]["flag_RSL"].to_numpy(dtype=float), 0.0
        )
    if case.physics_overrides.get("stability") == StabilityMethod.HOEGSTROM:
        df_expected = pd.read_csv(
            _W16_MOST_REFERENCE,
            index_col=[0, 1],
            parse_dates=[1],
            float_precision="round_trip",
        )
        df_actual = df_output["SUEWS"][["QH", "QE", "T2", "U10"]]
        pd.testing.assert_index_equal(df_actual.index, df_expected.index)
        pd.testing.assert_index_equal(
            df_actual.columns, df_expected.columns, check_names=False
        )
        np.testing.assert_allclose(
            df_actual.to_numpy(dtype=float),
            df_expected.to_numpy(dtype=float),
            rtol=1e-7,
            atol=1e-8,
            equal_nan=False,
            err_msg=(
                f"{case.case_id}: W16/MOST YAML->Rust output differs from "
                "recorded w16_most.csv reference"
            ),
        )


@pytest.mark.parametrize(("field", "value"), list(_sweep_params()))
def test_option_value_runs_finite_and_conserves_water(
    field, value, sample_config_loaded, sample_sweep_base
):
    """One sample day per physics-option value: finite output, water closes."""
    # ARRANGE
    base_source, df_forcing = sample_sweep_base
    case_config = patch_physics(base_source, sample_config_loaded, field, value)

    # ACT
    df_output, _ = run_simulation(df_forcing, case_config, serial_mode=True)

    # ASSERT
    assert_sweep_invariants(
        df_output,
        df_forcing,
        case_config,
        case_label=f"{field}={_option_label(value)}",
        check_all_active_groups=isinstance(case_config, SUEWSConfig),
    )


@pytest.mark.parametrize(
    ("field", "value", "message_fragment"), list(_refusal_params())
)
def test_option_value_unsupported_by_sample_is_refused(
    field, value, message_fragment, sample_config_loaded, sample_sweep_base
):
    """Missing inputs or unsupported combinations fail with a usable message.

    The sweep asserts the refusal rather than skipping the value: a clear
    error naming the missing input or implementation is the contract, and a
    skip would give it no coverage at all.
    """
    # ARRANGE
    base_source, df_forcing = sample_sweep_base
    case_config = patch_physics(base_source, sample_config_loaded, field, value)

    # ACT / ASSERT
    with pytest.raises((ValueError, RuntimeError), match=re.escape(message_fragment)):
        run_simulation(df_forcing, case_config, serial_mode=True)


@pytest.mark.parametrize(
    ("field", "value", "message_fragment"), list(_rejected_params())
)
def test_option_value_rejected_at_configuration(
    field, value, message_fragment, sample_config_loaded
):
    """A value the model cannot run is refused by the data model, not the run.

    The rejection fires on assignment (`validate_assignment=True`), which is
    also the path `patch_physics` takes, so the sweep never reaches the kernel
    with the value.
    """
    # ARRANGE
    physics = sample_config_loaded.model.physics.model_copy(deep=True)

    # ACT / ASSERT
    with pytest.raises(ValueError, match=re.escape(message_fragment)):
        setattr(physics, field, value)


@pytest.mark.parametrize(
    ("case_name", "overrides"),
    [
        pytest.param(name, overrides, id=name)
        for name, overrides in REPRESENTATIVE_OPTION_COMBINATIONS.items()
    ],
)
def test_yaml_rust_option_combinations_run_finite_and_conserve_water(
    case_name, overrides, sample_sweep_base, tmp_path
):
    """Representative valid physics combinations pass end-to-end YAML -> Rust."""
    # ARRANGE
    base_config, df_forcing = sample_sweep_base
    case_config = build_sweep_config(base_config, overrides)

    # ACT
    df_output, _ = run_simulation(
        df_forcing,
        case_config,
        serial_mode=True,
        round_trip_yaml=True,
        work_dir=tmp_path,
    )

    # ASSERT
    assert_sweep_invariants(
        df_output,
        df_forcing,
        case_config,
        case_label=case_name,
        check_all_active_groups=True,
    )


@pytest.mark.api
def test_yaml_roundtrip_matches_in_memory_config_run(sample_sweep_base, tmp_path):
    """On-disk YAML and in-memory SUEWSConfig produce identical engine output.

    Pins the shortcut used by the single-option sweep: patching a deep copy of
    the session `SUEWSConfig` leaves the session fixture untouched and emits
    the same resolved engine YAML and bit-identical output as serialising to
    YAML and re-parsing through `SUEWSConfig.from_yaml`.
    """
    # ARRANGE
    config_to_engine_yaml = import_module("supy._run_rust").config_to_engine_yaml
    base_config, df_forcing = sample_sweep_base
    original_storage_heat = _unwrap_value(base_config.model.physics.storage_heat)
    case_config = build_sweep_config(
        base_config, {"storage_heat": StorageHeatMethod.EHC}
    )

    # ACT
    yaml_path = tmp_path / "ehc_case.yml"
    case_config.to_yaml(yaml_path)
    reloaded = SUEWSConfig.from_yaml(str(yaml_path))
    df_mem, _ = run_simulation(
        df_forcing, case_config, serial_mode=True, round_trip_yaml=False
    )
    df_yaml, _ = run_simulation(
        df_forcing, yaml_path, serial_mode=True, round_trip_yaml=False
    )

    # ASSERT
    assert (
        _unwrap_value(base_config.model.physics.storage_heat) == original_storage_heat
    )
    mem_payload = {
        k: v
        for k, v in yaml.safe_load(config_to_engine_yaml(case_config)).items()
        if not str(k).startswith("_")
    }
    yaml_payload = {
        k: v
        for k, v in yaml.safe_load(config_to_engine_yaml(reloaded)).items()
        if not str(k).startswith("_")
    }
    assert mem_payload == yaml_payload
    pd.testing.assert_frame_equal(df_mem, df_yaml, check_exact=True)


@pytest.mark.parametrize(
    "case",
    [pytest.param(case, id=case.case_id) for case in COUPLED_OPTION_RUNS],
)
def test_coupled_refused_options_run_finite_and_conserve_water(
    case, sample_sweep_base, tmp_path
):
    """Every single-option-refused value runs finite and closes water when coupled."""
    # ARRANGE
    base_config, base_forcing = sample_sweep_base
    case_config, df_forcing = build_coupled_case(base_config, base_forcing, case)

    # ACT
    df_output, _ = run_simulation(
        df_forcing,
        case_config,
        serial_mode=True,
        round_trip_yaml=True,
        work_dir=tmp_path,
    )

    # ASSERT
    assert_sweep_invariants(
        df_output,
        df_forcing,
        case_config,
        case_label=case.case_id,
        check_all_active_groups=True,
    )
    _assert_coupled_option_response(df_output, df_forcing, case_config, case)


def test_coupled_response_assertions_reject_alternative_physics_branches(
    sample_sweep_base, tmp_path
):
    """Cloud-forced Ldown and W16/MOST checks reject outputs from alternative schemes."""
    # ARRANGE: air-derived Ldown (3) + CN98 stability (3) + MOST (0) with fcld supplied
    base_config, base_forcing = sample_sweep_base
    alt_config = build_sweep_config(
        base_config,
        {
            "net_radiation": NetRadiationMethod.LDOWN_AIR,
            "stability": StabilityMethod.CAMPBELL_NORMAN,
            "roughness_sublayer": RSLMethod.MOST,
        },
    )
    df_forcing = _apply_coupled_forcing(base_forcing, ("fcld",))
    cloud_case = next(
        c for c in COUPLED_OPTION_RUNS if c.case_id == "net_radiation_ldown_cloud"
    )
    w16_case = next(c for c in COUPLED_OPTION_RUNS if c.case_id == "stability_w16_most")

    # ACT
    df_alt, _ = run_simulation(
        df_forcing,
        alt_config,
        serial_mode=True,
        round_trip_yaml=True,
        work_dir=tmp_path,
    )

    # ASSERT: cloud and W16 response guards discriminate against air-Ldown / CN98
    with pytest.raises(AssertionError, match="Prata cloud-corrected"):
        _assert_coupled_option_response(df_alt, df_forcing, alt_config, cloud_case)
    with pytest.raises(AssertionError, match=r"w16_most\.csv"):
        _assert_coupled_option_response(df_alt, df_forcing, alt_config, w16_case)


def test_observed_qn_finiteness_guard_respects_snow_use(sample_sweep_base, tmp_path):
    """Observed QN absent-column rules apply only when snow_use=0, not snow_use=1."""
    # ARRANGE
    base_config, base_forcing = sample_sweep_base
    df_forcing = _apply_coupled_forcing(base_forcing, ("qn",))
    df_forcing["snow"] = 0.0

    cfg_no_snow = build_sweep_config(
        base_config, {"net_radiation": NetRadiationMethod.OBSERVED, "snow_use": 0}
    )
    cfg_snow = build_sweep_config(
        base_config, {"net_radiation": NetRadiationMethod.OBSERVED, "snow_use": 1}
    )

    no_snow_dir = tmp_path / "no_snow"
    snow_dir = tmp_path / "snow"
    no_snow_dir.mkdir()
    snow_dir.mkdir()

    # ACT
    df_no_snow, _ = run_simulation(
        df_forcing,
        cfg_no_snow,
        serial_mode=True,
        round_trip_yaml=True,
        work_dir=no_snow_dir,
    )
    df_snow, _ = run_simulation(
        df_forcing,
        cfg_snow,
        serial_mode=True,
        round_trip_yaml=True,
        work_dir=snow_dir,
    )

    # ASSERT: snow_use=0 keeps Kup/Ldown/Lup/Tsurf all-NaN; snow_use=1 populates them
    assert_simulation_output_finiteness(
        df_no_snow,
        expected_grids=(1,),
        expected_timestamps=df_forcing.index,
        net_radiation=0,
        snow_use=0,
        forcing=df_forcing,
    )
    with pytest.raises(AssertionError, match="non-finite"):
        assert_simulation_output_finiteness(
            df_no_snow,
            expected_grids=(1,),
            expected_timestamps=df_forcing.index,
            net_radiation=0,
            snow_use=1,
            forcing=df_forcing,
        )

    assert_simulation_output_finiteness(
        df_snow,
        expected_grids=(1,),
        expected_timestamps=df_forcing.index,
        net_radiation=0,
        snow_use=1,
        forcing=df_forcing,
    )
    with pytest.raises(AssertionError, match="declared absent actually hold values"):
        assert_simulation_output_finiteness(
            df_snow,
            expected_grids=(1,),
            expected_timestamps=df_forcing.index,
            net_radiation=0,
            snow_use=0,
            forcing=df_forcing,
        )


def test_coupled_runs_cover_all_refused_options():
    """COUPLED_OPTION_RUNS must cover every key declared in OPTION_VALUES_REFUSED."""
    # ARRANGE
    case_ids = [case.case_id for case in COUPLED_OPTION_RUNS]
    assert len(case_ids) == len(set(case_ids)), (
        f"Duplicate case_id entries in COUPLED_OPTION_RUNS: {case_ids}"
    )

    covered: set[tuple[str, int]] = set()
    for case in COUPLED_OPTION_RUNS:
        assert case.covers, (
            f"COUPLED_OPTION_RUNS[{case.case_id!r}] must declare at least one "
            "covered OPTION_VALUES_REFUSED entry"
        )
        for field, member in case.covers:
            assert case.physics_overrides.get(field) == member, (
                f"COUPLED_OPTION_RUNS[{case.case_id!r}] claims coverage of "
                f"{(field, member.value)!r} without setting it in physics_overrides"
            )
            covered.add((field, member.value))

    # ACT / ASSERT
    refused = set(OPTION_VALUES_REFUSED)
    missing = sorted(refused - covered)
    assert not missing, (
        f"OPTION_VALUES_REFUSED entries missing from COUPLED_OPTION_RUNS: {missing}"
    )
    extra = sorted(covered - refused)
    assert not extra, (
        f"COUPLED_OPTION_RUNS covers entries not in OPTION_VALUES_REFUSED: {extra}"
    )


def test_option_tables_name_real_values():
    """Every exclusion, refusal, xfail, combination, and coupled key is real.

    Without this the tables rot silently: a renamed field or a dropped enum
    member would leave an entry that excuses nothing, and the value it was
    meant to cover would either vanish from the sweep or start failing under
    a stale reason.
    """
    # ARRANGE
    known = {
        (field, member.value)
        for field, enum_cls in _physics_option_enums()
        for member in enum_cls
    }

    # ACT / ASSERT
    for table_name, table in (
        ("OPTION_VALUES_REFUSED", OPTION_VALUES_REFUSED),
        ("OPTION_VALUES_XFAIL", OPTION_VALUES_XFAIL),
        ("OPTION_VALUES_REJECTED", OPTION_VALUES_REJECTED),
    ):
        unknown = sorted(key for key in table if key not in known)
        assert not unknown, f"{table_name} names values that do not exist: {unknown}"

    overlap = sorted(
        set(OPTION_VALUES_REFUSED) & set(OPTION_VALUES_XFAIL)
        | set(OPTION_VALUES_REFUSED) & set(OPTION_VALUES_REJECTED)
        | set(OPTION_VALUES_XFAIL) & set(OPTION_VALUES_REJECTED)
    )
    assert not overlap, f"a value is in more than one table: {overlap}"

    excluded = (
        set(OPTION_VALUES_REFUSED)
        | set(OPTION_VALUES_XFAIL)
        | set(OPTION_VALUES_REJECTED)
    )
    for case_name, overrides in REPRESENTATIVE_OPTION_COMBINATIONS.items():
        assert len(overrides) >= 2, (
            f"REPRESENTATIVE_OPTION_COMBINATIONS[{case_name!r}] must combine at "
            "least two physics options"
        )
        for field, member in overrides.items():
            key = (field, member.value)
            assert key in known, (
                f"REPRESENTATIVE_OPTION_COMBINATIONS[{case_name!r}] names "
                f"unknown option {key!r}"
            )
            assert key not in excluded, (
                f"REPRESENTATIVE_OPTION_COMBINATIONS[{case_name!r}] overlaps "
                f"excluded single-option table on {key!r}"
            )

    disallowed_in_coupled = set(OPTION_VALUES_XFAIL) | set(OPTION_VALUES_REJECTED)
    for case in COUPLED_OPTION_RUNS:
        for field, member in case.covers:
            key = (field, member.value)
            assert key in known, (
                f"COUPLED_OPTION_RUNS[{case.case_id!r}].covers names "
                f"unknown option {key!r}"
            )
        for field, member in case.physics_overrides.items():
            key = (field, member.value)
            assert key in known, (
                f"COUPLED_OPTION_RUNS[{case.case_id!r}].physics_overrides names "
                f"unknown option {key!r}"
            )
            assert key not in disallowed_in_coupled, (
                f"COUPLED_OPTION_RUNS[{case.case_id!r}] uses xfail/rejected "
                f"option {key!r}"
            )
        for col in case.forcing_columns:
            assert FORCING_REGISTRY.by_file_name(col) is not None, (
                f"COUPLED_OPTION_RUNS[{case.case_id!r}] names unknown forcing "
                f"column {col!r}"
            )
