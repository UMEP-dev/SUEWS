"""Breadth sweep over the physics-option space.

The rest of `test/physics/` exercises two to five hand-picked points of the
physics-option space. A single day of the bundled sample configuration costs
about 0.16 s of model time, so running *every* value of *every*
`ModelPhysics` enum -- one option varied at a time from the sample's defaults
-- costs seconds and turns the option space from "the values we happened to
pick" into "the values that exist".

What the sweep asserts for each runnable value:

- the SUEWS output group is finite (bar the columns that mirror a forcing
  column the sample does not carry), and
- the water balance closes to the same 1e-4 mm bound
  `test_core_physics.test_water_balance_consistency` uses.

Values the sample cannot support are not silently skipped. They fall into
three declared tables, each with its reason:

- `OPTION_VALUES_REFUSED` -- needs a forcing column or input block the sample
  does not carry. The model refuses these with an explicit message, so the
  sweep asserts the refusal rather than dropping the value.
- `OPTION_VALUES_XFAIL` -- runs, but violates an invariant above. Recorded
  `xfail(strict=True)` against an open issue, so the entry fails the moment
  the defect is fixed.
- `OPTION_VALUES_NOT_RUN` -- cannot be run at all. Only a failure that takes
  the interpreter down belongs here; `xfail` cannot survive SIGSEGV.

`test_option_tables_name_real_values` keeps the three tables from rotting: a
key naming a field or value that no longer exists fails.
"""

from enum import Enum
import re
from typing import get_args

from conftest import TIMESTEPS_PER_DAY, run_simulation
import numpy as np
import pytest

from supy.data_model.core.model import ModelPhysics

pytestmark = pytest.mark.physics

# Same bound as test_core_physics.test_water_balance_consistency: the closure
# is exact by construction, so this is a numerical-noise margin, not a
# physical tolerance.
WATER_CLOSURE_TOLERANCE_MM = 1e-4

# `Fcld` mirrors the `fcld` forcing column, which the bundled sample carries
# as the -999 missing sentinel throughout. No net-radiation option reachable
# in this sweep diagnoses cloud fraction instead, so the model reports it as
# missing for every option value; that is the input, not a defect.
SUEWS_COLUMNS_ABSENT_FROM_SAMPLE = ("Fcld",)

# Values needing a forcing column or input block the bundled sample does not
# carry. Each maps to a fragment of the message the model refuses it with.
OPTION_VALUES_REFUSED = {
    ("net_radiation", 0): "forcing column 'qn'",
    ("net_radiation", 1): "forcing column 'ldown'",
    ("net_radiation", 2): "forcing column 'fcld'",
    ("net_radiation", 11): "forcing column 'ldown'",
    ("net_radiation", 12): "forcing column 'fcld'",
    ("net_radiation", 100): "forcing column 'ldown'",
    ("net_radiation", 200): "forcing column 'fcld'",
    ("net_radiation", 1001): "forcing column 'ldown'",
    ("net_radiation", 1002): "forcing column 'fcld'",
    ("emissions", 0): "forcing column 'qf'",
    ("storage_heat", 0): "forcing column 'qs'",
    # STEBBS storage heat needs SPARTACUS-Surface net radiation, which the
    # sample's net_radiation=3 is not; varying one option at a time cannot
    # satisfy the pair.
    ("storage_heat", 7): "requires SPARTACUS-Surface net radiation",
    ("soil_moisture_deficit", 1): "forcing column 'xsmd'",
    ("soil_moisture_deficit", 2): "forcing column 'xsmd'",
    ("water_use", 1): "forcing column 'wuh'",
    ("laimethod", 0): "forcing column 'lai'",
}

# Values that run and then violate one of the sweep's invariants.
OPTION_VALUES_XFAIL = {
    ("stability", 0): (
        "reserved stability code: psi has no branch for it, so the RSL "
        "profile returns NaN T2/RH2/Q2/U10 (UMEP-dev/SUEWS#1783)"
    ),
    ("stability", 1): (
        "reserved stability code: psi has no branch for it, so the RSL "
        "profile returns NaN T2/RH2/Q2/U10 (UMEP-dev/SUEWS#1783)"
    ),
    ("stability", 2): (
        "RSL profile returns NaN T2/RH2/Q2/U10 while fluxes stay finite "
        "(UMEP-dev/SUEWS#1784)"
    ),
    ("snow_use", 1): (
        "snow state is never written back, so the water body dumps its "
        "store as ~19999.5 mm of runoff every timestep and the water "
        "balance does not close (UMEP-dev/SUEWS#1757)"
    ),
}

# Values that cannot be run at all.
OPTION_VALUES_NOT_RUN = {
    ("storage_heat", 4): (
        "ESTM segfaults: its Ts5mindata_ir input is never populated on the "
        "YAML path, so the kernel reads past a zero-length array "
        "(UMEP-dev/SUEWS#1785). A SIGSEGV takes the pytest worker with it, "
        "so this cannot be an xfail."
    ),
}


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
            if key in OPTION_VALUES_REFUSED or key in OPTION_VALUES_NOT_RUN:
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


@pytest.fixture(scope="module")
def sample_state_and_forcing(sample_config_loaded, sample_data_loaded):
    """The sample's `(df_state, one-day df_forcing)`, built once per module.

    `SUEWSConfig.to_df_state()` costs about 0.55 s, which would dominate a
    ninety-case sweep if paid per case, so the base state is built once here
    and each case patches only the physics columns (see `patch_physics`).

    READ-ONLY, shared across the module: `patch_physics` copies before
    writing and the forcing frame is never mutated.
    """
    _, df_forcing = sample_data_loaded
    return (
        sample_config_loaded.to_df_state(),
        df_forcing.iloc[:TIMESTEPS_PER_DAY].copy(),
    )


def patch_physics(df_state_base, config, field, value):
    """`df_state_base` with one physics option changed to `value`.

    The change goes through the data model twice over: assignment onto a
    `ModelPhysics` copy is validated (`validate_assignment=True`), and the
    replacement columns come from that model's own `to_df_state`, not from a
    hand-written column poke. `test_patched_state_matches_full_config_state`
    pins the assumption this rests on -- that no column outside
    `ModelPhysics.to_df_state`'s own output is derived from physics.
    """
    physics = config.model.physics.model_copy(deep=True)
    setattr(physics, field, value)
    df_physics = physics.to_df_state(df_state_base.index[0])
    df_state = df_state_base.copy()
    for col in df_physics.columns:
        df_state[col] = df_physics[col].values
    return df_state


def water_balance_residual_mm(df_output, df_state, grid=1):
    """Largest per-timestep water-balance residual, in mm.

    P + I = E + RO + dS, with storage taken as the surface-fraction-weighted
    soil store from the debug group plus the surface water state -- the same
    accounting `test_core_physics.test_water_balance_consistency` uses.
    """
    suews = df_output.loc[grid, "SUEWS"]
    df_soil_store = df_output.loc[grid, "debug"].filter(regex=r"^ss_.*_next$")
    soil_store = df_soil_store.dot(df_state.sfr_surf.iloc[0].values)
    total_store = soil_store + suews.State
    net_input = suews.Rain + suews.Irr - suews.Evap - suews.RO
    return float((total_store.diff().dropna() - net_input).abs().max())


@pytest.mark.parametrize(("field", "value"), list(_sweep_params()))
def test_option_value_runs_finite_and_conserves_water(
    field, value, sample_config_loaded, sample_state_and_forcing
):
    """One sample day per physics-option value: finite output, water closes."""
    # ARRANGE
    df_state_base, df_forcing = sample_state_and_forcing
    df_state = patch_physics(df_state_base, sample_config_loaded, field, value)

    # ACT
    df_output, _ = run_simulation(df_forcing, df_state, serial_mode=True)

    # ASSERT
    suews = df_output.loc[1, "SUEWS"].drop(
        columns=list(SUEWS_COLUMNS_ABSENT_FROM_SAMPLE)
    )
    non_finite = suews.columns[
        ~np.isfinite(suews.to_numpy(dtype=float)).all(axis=0)
    ].tolist()
    assert not non_finite, (
        f"{field}={value.name} produced non-finite SUEWS output in {non_finite}"
    )

    residual = water_balance_residual_mm(df_output, df_state)
    assert residual < WATER_CLOSURE_TOLERANCE_MM, (
        f"{field}={value.name}: water balance not closed, max residual "
        f"{residual:.3e} mm"
    )


@pytest.mark.parametrize(
    ("field", "value", "message_fragment"), list(_refusal_params())
)
def test_option_value_needing_absent_input_is_refused(
    field, value, message_fragment, sample_config_loaded, sample_state_and_forcing
):
    """A value needing input the sample lacks is refused, with a usable message.

    The sweep asserts the refusal rather than skipping the value: a clear
    error naming the missing column is the contract for these options, and a
    skip would give it no coverage at all.
    """
    # ARRANGE
    df_state_base, df_forcing = sample_state_and_forcing
    df_state = patch_physics(df_state_base, sample_config_loaded, field, value)

    # ACT / ASSERT
    with pytest.raises((ValueError, RuntimeError), match=re.escape(message_fragment)):
        run_simulation(df_forcing, df_state, serial_mode=True)


def test_patched_state_matches_full_config_state(
    sample_config_loaded, sample_state_and_forcing
):
    """Patching the physics columns equals rebuilding the whole config state.

    The sweep patches only the columns `ModelPhysics.to_df_state` writes,
    which is sound just as long as nothing outside them is derived from
    physics. This pins that: if a future config section starts reading
    `model.physics`, the sweep's shortcut fails here rather than quietly
    running the wrong state.
    """
    # ARRANGE
    df_state_base, _ = sample_state_and_forcing
    field, value = "storage_heat", 5  # EHC: reaches the extended-facet branch

    # ACT
    patched = patch_physics(df_state_base, sample_config_loaded, field, value)
    rebuilt_config = sample_config_loaded.model_copy(deep=True)
    setattr(rebuilt_config.model.physics, field, value)
    rebuilt = rebuilt_config.to_df_state()

    # ASSERT
    assert list(patched.columns) == list(rebuilt.columns)
    np.testing.assert_array_equal(
        patched.to_numpy(dtype=object), rebuilt.to_numpy(dtype=object)
    )


def test_option_tables_name_real_values():
    """Every exclusion, refusal and xfail key names a field and value that exist.

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
        ("OPTION_VALUES_NOT_RUN", OPTION_VALUES_NOT_RUN),
    ):
        unknown = sorted(key for key in table if key not in known)
        assert not unknown, f"{table_name} names values that do not exist: {unknown}"

    overlap = sorted(
        set(OPTION_VALUES_REFUSED) & set(OPTION_VALUES_XFAIL)
        | set(OPTION_VALUES_REFUSED) & set(OPTION_VALUES_NOT_RUN)
        | set(OPTION_VALUES_XFAIL) & set(OPTION_VALUES_NOT_RUN)
    )
    assert not overlap, f"a value is in more than one table: {overlap}"
