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
  plus `EHC` / `SPARTACUS` / `snow` when enabled) are finite and non-sentinel
  on the exact forcing timestamps, with `SUEWS.Fcld` matching supplied
  forcing, and
- the water balance closes to the same 1e-4 mm bound
  `test_core_physics.test_water_balance_consistency` uses.

Values the sample cannot support are not silently skipped. They fall into
three declared tables, each with its reason:

- `OPTION_VALUES_REFUSED` -- needs a forcing column or input block the sample
  does not carry, or a method combination the kernel cannot run. The model
  refuses these with an explicit message, so the sweep asserts the refusal
  rather than dropping the value.
- `OPTION_VALUES_XFAIL` -- runs, but violates an invariant above. Recorded
  `xfail(strict=True)` against an open issue, so the entry fails the moment
  the defect is fixed.
- `OPTION_VALUES_REJECTED` -- the data model refuses the value outright, on
  construction and on assignment, so no run is attempted. The sweep asserts
  that rejection and its message.

`test_option_tables_name_real_values` keeps the three tables and the
representative multi-option combination table from rotting: a key naming a
field or value that no longer exists fails.
"""

from __future__ import annotations

from collections.abc import Mapping
from enum import Enum
from importlib import import_module
from pathlib import Path
import re
import tempfile
from typing import Any, get_args

from conftest import TIMESTEPS_PER_DAY
from layer5_checks import (
    DAILYSTATE_ABSENT_UNDER_EHC,
    RSL_ABSENT_UNDER_MOST,
    assert_forcing_cloud,
    assert_simulation_output_finiteness,
    expected_dailystate_mask,
    expected_group_columns,
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
    ModelPhysics,
    MomentumRoughnessMethod,
    NetRadiationMethod,
    OhmIncQf,
    RSLLevel,
    RSLMethod,
    StabilityMethod,
    StorageHeatMethod,
)

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
# Coupled runs for OPTION_VALUES_REFUSED entries and cold-snow / 7-surface /
# observed-sm anchors live in dedicated follow-up slices of #2100.
REPRESENTATIVE_OPTION_COMBINATIONS: dict[str, dict[str, Enum]] = {
    "ehc_implicit_macdonald_ward": {
        "storage_heat": StorageHeatMethod.EHC,
        "ehc_surface_coupling": EHCSurfaceCoupling.IMPLICIT,
        "control_ehc_stability": EHCStabilityControl.RELAX,
        "roughness_length_momentum": MomentumRoughnessMethod.MACDONALD,
        "surface_conductance": GSModel.WARD,
    },
    "ehc_explicit_kawai_most": {
        "storage_heat": StorageHeatMethod.EHC,
        "ehc_surface_coupling": EHCSurfaceCoupling.EXPLICIT,
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


# Indoor EHC diagnostics emitted as -999 -> NaN under explicit EHC coupling
# (ehc_surface_coupling=0, matching test/core/test_ehc_implicit.py).
_EHC_ABSENT_UNDER_EXPLICIT: tuple[str, ...] = ("QIndoor_EHC", "T_Indoor_EHC")
_LAYER_SUFFIX_RE = re.compile(r"_(\d+)$")


def _unused_layer_cols(columns: tuple[str, ...], nlayer: int) -> tuple[str, ...]:
    """Return fixed-width vertical-layer columns above ``nlayer``."""
    return tuple(
        col
        for col in columns
        if (m := _LAYER_SUFFIX_RE.search(col)) is not None and int(m.group(1)) > nlayer
    )


def _assert_rsl_and_dailystate_for_explicit_ehc(
    grid_df: pd.DataFrame,
    *,
    expected_timestamps: pd.DatetimeIndex,
    tstep: int,
) -> None:
    """Assert RSL and DailyState finiteness under explicit EHC."""
    rsl = grid_df["RSL"]
    rsl_cols = expected_group_columns("RSL")
    flag_rsl = strict_finite_array(rsl["flag_RSL"], label="RSL.flag_RSL")
    rsl_mask = np.equal(flag_rsl, 1.0)
    most_mask = np.equal(flag_rsl, 0.0)
    assert bool((rsl_mask | most_mask).all())
    if bool(rsl_mask.any()):
        strict_finite_array(rsl.loc[rsl_mask, list(rsl_cols)], label="RSL[flag_RSL=1]")
    if bool(most_mask.any()):
        assert rsl.loc[most_mask, list(RSL_ABSENT_UNDER_MOST)].isna().all().all()
        most_active = [c for c in rsl_cols if c not in RSL_ABSENT_UNDER_MOST]
        strict_finite_array(rsl.loc[most_mask, most_active], label="RSL[flag_RSL=0]")

    ds = grid_df["DailyState"]
    ds_cols = expected_group_columns("DailyState")
    eod_mask = expected_dailystate_mask(expected_timestamps, tstep)
    assert bool(eod_mask.any())
    if bool((~eod_mask).any()):
        assert ds.loc[~eod_mask, list(ds_cols)].isna().all().all()
    assert ds.loc[eod_mask, list(DAILYSTATE_ABSENT_UNDER_EHC)].isna().all().all()
    ds_active = [c for c in ds_cols if c not in DAILYSTATE_ABSENT_UNDER_EHC]
    strict_finite_array(ds.loc[eod_mask, ds_active], label="DailyState[EOD]")


def _assert_explicit_ehc_active_groups(
    grid_df: pd.DataFrame,
    *,
    expected_timestamps: pd.DatetimeIndex,
    tstep: int,
    net_radiation: int,
    snow_use: int,
    nlayer: int,
    has_buildings: bool,
) -> None:
    """Assert active output groups under explicit EHC (storage_heat=5, coupling=0)."""
    for group in ("debug", "BEERS"):
        sub = grid_df[group]
        cols = expected_group_columns(group)
        missing = sorted(set(cols) - set(sub.columns))
        assert not missing, f"{group}: missing required output columns: {missing}"
        if group == "BEERS" and not has_buildings:
            assert sub[list(cols)].isna().all().all()
        else:
            strict_finite_array(sub[list(cols)], label=group)

    _assert_rsl_and_dailystate_for_explicit_ehc(
        grid_df, expected_timestamps=expected_timestamps, tstep=tstep
    )

    ehc = grid_df["EHC"]
    ehc_cols = expected_group_columns("EHC")
    missing_ehc = sorted(set(ehc_cols) - set(ehc.columns))
    assert not missing_ehc, f"EHC: missing required output columns: {missing_ehc}"
    ehc_absent = (*_EHC_ABSENT_UNDER_EXPLICIT, *_unused_layer_cols(ehc_cols, nlayer))
    assert ehc[list(ehc_absent)].isna().all().all(), (
        f"EHC[explicit]: expected all-NaN columns {ehc_absent}"
    )
    ehc_active = [c for c in ehc_cols if c not in ehc_absent]
    strict_finite_array(ehc[ehc_active], label="EHC[explicit]")

    if net_radiation > 1000:
        sp = grid_df["SPARTACUS"]
        sp_cols = expected_group_columns("SPARTACUS")
        sp_unused = _unused_layer_cols(sp_cols, nlayer)
        if sp_unused:
            assert sp[list(sp_unused)].isna().all().all()
        strict_finite_array(
            sp[[c for c in sp_cols if c not in sp_unused]], label="SPARTACUS"
        )
    if snow_use == 1:
        strict_finite_array(
            grid_df["snow"][list(expected_group_columns("snow"))], label="snow"
        )


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
    strict_finite_array(suews.drop(columns="Fcld"), label="SUEWS")

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
        storage_heat = int(_unwrap_value(physics.storage_heat))
        net_radiation = int(_unwrap_value(physics.net_radiation))
        snow_use = int(_unwrap_value(physics.snow_use))
        ehc_coupling = int(_unwrap_value(physics.ehc_surface_coupling))
        if storage_heat == int(StorageHeatMethod.EHC.value) and ehc_coupling == int(
            EHCSurfaceCoupling.EXPLICIT.value
        ):
            _assert_explicit_ehc_active_groups(
                df_output.xs(grid, level=0),
                expected_timestamps=df_forcing.index,
                tstep=tstep,
                net_radiation=net_radiation,
                snow_use=snow_use,
                nlayer=nlayer,
                has_buildings=has_buildings,
            )
        else:
            assert_simulation_output_finiteness(
                df_output,
                expected_grids=(grid,),
                expected_timestamps=df_forcing.index,
                tstep=tstep,
                storage_heat=storage_heat,
                net_radiation=net_radiation,
                snow_use=snow_use,
                nlayer=nlayer,
                has_buildings=has_buildings,
                forcing=df_forcing,
            )

    assert residual < WATER_CLOSURE_TOLERANCE_MM, (
        f"{case_label}: water balance not closed, max residual {residual:.3e} mm"
    )
    return residual


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


def test_option_tables_name_real_values():
    """Every exclusion, refusal, xfail, and combination key names a real option.

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
