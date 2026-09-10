"""Property tests for the pure-Python numerics with analytic invariants.

Three post-processing surfaces carry invariants that hold for *every* input,
not just for the sample: the Shapley attribution decomposition closes
exactly, output resampling conserves the accumulated variables and cannot
move an averaged one outside its own bin, and forcing interpolation cannot
manufacture values outside the range it interpolates. Each is stated here as
a property over hypothesis-generated inputs rather than as one worked
example.

These are numerical invariants of the model's own post-processing, hence the
`physics` nature marker rather than `api`: what is under test is arithmetic,
not the pandas surface it is expressed in.

Kept deliberately cheap -- 50 examples, no deadline (the resampling
properties build small DataFrames, whose cost varies enough between
platforms that a per-example deadline is noise), and no I/O. The settings
live in one registered profile, `suews-physics`, so every property here
draws the same examples on every machine: a failure in the physics lane must
be reproducible locally, not a seed nobody can recover.
"""

from hypothesis import HealthCheck, given, settings, strategies as st
from hypothesis.extra.numpy import array_shapes, arrays
import numpy as np
import pandas as pd
import pytest

from supy._load import resample_linear_avg, resample_linear_inst, resample_sum
from supy._post import _resample_output
from supy.data_model.output import OUTPUT_REGISTRY
from supy.util._attribution._core import (
    shapley_binary_product,
    shapley_forcing_profile,
    shapley_triple_product,
)
from supy.util._attribution._physics import decompose_flux_budget

pytestmark = pytest.mark.physics

# One fixed profile for the whole module. `derandomize=True` derives the
# example sequence from the test itself rather than from a per-run seed, so
# the physics lane runs the same 50 examples on every platform and every
# rerun; a property that fails on CI fails identically on a developer's
# machine. `hypothesis` is pinned (pyproject dev extra and the CI test
# environments) because the generated sequence also depends on its version.
settings.register_profile(
    "suews-physics",
    deadline=None,
    max_examples=50,
    derandomize=True,
    suppress_health_check=[HealthCheck.too_slow],
)
PROPERTY_SETTINGS = settings.get_profile("suews-physics")

# Bounded, well-scaled factors: the Shapley identities are exact in real
# arithmetic, so the only thing an unbounded strategy would test is
# floating-point cancellation, which is not the property under test.
factors = arrays(
    dtype=np.float64,
    shape=array_shapes(min_dims=1, max_dims=1, min_side=1, max_side=32),
    elements=st.floats(
        min_value=-1e3, max_value=1e3, allow_nan=False, allow_infinity=False
    ),
)


def closure_atol(*terms):
    """Absolute tolerance scaled to the magnitudes actually involved.

    The identities are exact, so the only admissible error is rounding in the
    products that express them; that error scales with the terms, so a fixed
    absolute bound would be either vacuous or wrong depending on the draw.
    """
    scale = max((float(np.abs(t).max()) for t in terms if t.size), default=1.0)
    return 1e-9 * max(scale, 1.0)


# =============================================================================
# Attribution: exact Shapley closure
# =============================================================================


@PROPERTY_SETTINGS
@given(
    x_A=factors,
    x_B=factors,
    y_A=factors,
    y_B=factors,
    z_A=factors,
    z_B=factors,
)
def test_shapley_triple_product_closes(x_A, x_B, y_A, y_B, z_A, z_B):
    """Phi_x + Phi_y + Phi_z == f_B - f_A for f = x*y*z, for any two states."""
    # ARRANGE
    n = min(a.size for a in (x_A, x_B, y_A, y_B, z_A, z_B))
    x_A, x_B, y_A, y_B, z_A, z_B = (a[:n] for a in (x_A, x_B, y_A, y_B, z_A, z_B))

    # ACT
    phi_x, phi_y, phi_z = shapley_triple_product(x_A, x_B, y_A, y_B, z_A, z_B)

    # ASSERT
    delta = x_B * y_B * z_B - x_A * y_A * z_A
    np.testing.assert_allclose(
        phi_x + phi_y + phi_z,
        delta,
        rtol=1e-9,
        atol=closure_atol(delta, phi_x, phi_y, phi_z),
    )


@PROPERTY_SETTINGS
@given(x_A=factors, x_B=factors, y_A=factors, y_B=factors)
def test_shapley_binary_product_closes(x_A, x_B, y_A, y_B):
    """Phi_x + Phi_y == f_B - f_A for f = x*y, for any two states."""
    # ARRANGE
    n = min(a.size for a in (x_A, x_B, y_A, y_B))
    x_A, x_B, y_A, y_B = (a[:n] for a in (x_A, x_B, y_A, y_B))

    # ACT
    phi_x, phi_y = shapley_binary_product(x_A, x_B, y_A, y_B)

    # ASSERT
    delta = x_B * y_B - x_A * y_A
    np.testing.assert_allclose(
        phi_x + phi_y, delta, rtol=1e-9, atol=closure_atol(delta, phi_x, phi_y)
    )


@PROPERTY_SETTINGS
@given(
    F_A=factors,
    F_B=factors,
    R_A=factors,
    R_B=factors,
    S_A=factors,
    S_B=factors,
)
def test_shapley_forcing_profile_closes(F_A, F_B, R_A, R_B, S_A, S_B):
    """Phi_F + Phi_R + Phi_S == f_B - f_A for the f = F*(R+S) wind-profile form."""
    # ARRANGE
    n = min(a.size for a in (F_A, F_B, R_A, R_B, S_A, S_B))
    F_A, F_B, R_A, R_B, S_A, S_B = (a[:n] for a in (F_A, F_B, R_A, R_B, S_A, S_B))

    # ACT
    phi_F, phi_R, phi_S = shapley_forcing_profile(F_A, F_B, R_A, R_B, S_A, S_B)

    # ASSERT
    delta = F_B * (R_B + S_B) - F_A * (R_A + S_A)
    np.testing.assert_allclose(
        phi_F + phi_R + phi_S,
        delta,
        rtol=1e-9,
        atol=closure_atol(delta, phi_F, phi_R, phi_S),
    )


@PROPERTY_SETTINGS
@given(
    flux_A=factors,
    d_radiation=factors,
    d_storage=factors,
    total_contribution=factors,
)
def test_flux_budget_decomposition_closes(
    flux_A, d_radiation, d_storage, total_contribution
):
    """Budget contributions sum to the total when the components sum to d_flux.

    `decompose_flux_budget` allocates a flux's Shapley contribution across
    the terms of its budget by their share of the flux change. Whenever the
    component changes account for the whole flux change -- which is what
    a closed budget means -- the shares sum to one and the allocation is
    exhaustive.
    """
    # ARRANGE
    n = min(a.size for a in (flux_A, d_radiation, d_storage, total_contribution))
    flux_A, d_radiation, d_storage, total_contribution = (
        a[:n] for a in (flux_A, d_radiation, d_storage, total_contribution)
    )
    components_A = {"radiation": np.zeros(n), "storage": np.zeros(n)}
    components_B = {"radiation": d_radiation, "storage": d_storage}
    # A closed budget by construction: the flux change is the sum of the
    # component changes.
    flux_B = flux_A + d_radiation + d_storage

    # ACT
    contributions = decompose_flux_budget(
        flux_A, flux_B, components_A, components_B, total_contribution
    )

    # ASSERT
    total = sum(contributions.values())
    d_flux = flux_B - flux_A
    # Near-zero flux change is documented as NaN (the share is undefined
    # there), so those timesteps are excluded rather than asserted upon.
    resolved = np.abs(d_flux) >= 1e-10
    np.testing.assert_allclose(
        total[resolved],
        total_contribution[resolved],
        rtol=1e-9,
        atol=closure_atol(total_contribution),
    )


# =============================================================================
# Output resampling
# =============================================================================

_SUEWS_AGGREGATION = OUTPUT_REGISTRY.get_aggregation_rules()["SUEWS"]


def _aggregation_name(rule):
    """The registered aggregation's name, whichever form pandas was given.

    The registry hands `resample().agg()` either a pandas method name or a
    callable, and both forms appear in the SUEWS group.
    """
    return rule if isinstance(rule, str) else getattr(rule, "__name__", "")


def _first_var_aggregated_by(kind):
    """First SUEWS variable whose registered aggregation is `kind`.

    Read from the output registry rather than named here, so a variable
    reclassified in the registry does not leave this test asserting the wrong
    rule for it.
    """
    for name, rule in _SUEWS_AGGREGATION.items():
        if _aggregation_name(rule) == kind:
            return name
    pytest.fail(f"no SUEWS variable is aggregated by {kind!r}")


SUM_VAR = _first_var_aggregated_by("sum")
MEAN_VAR = _first_var_aggregated_by("mean")

STEPS_PER_HOUR = 12  # five-minute output


def _synthetic_output(values_sum, values_mean):
    """A single-grid SUEWS output frame carrying the group's full column set.

    `_resample_output` aggregates with the registry's whole per-group rule
    dict, so a frame holding only the two columns under test is rejected by
    pandas. Every other column is present and zero.
    """
    n = values_sum.size
    index = pd.MultiIndex.from_product(
        [
            [1],
            pd.date_range("2012-01-01 00:05", periods=n, freq="300s"),
        ],
        names=["grid", "datetime"],
    )
    columns = pd.MultiIndex.from_product(
        [["SUEWS"], list(_SUEWS_AGGREGATION)], names=["group", "var"]
    )
    df = pd.DataFrame(0.0, index=index, columns=columns)
    df["SUEWS", SUM_VAR] = values_sum
    df["SUEWS", MEAN_VAR] = values_mean
    return df


hourly_blocks = st.integers(min_value=1, max_value=6)
finite_values = st.floats(
    min_value=-1e4, max_value=1e4, allow_nan=False, allow_infinity=False
)


@PROPERTY_SETTINGS
@given(
    hours=hourly_blocks,
    data=st.data(),
)
def test_output_resample_conserves_accumulated_and_bounds_averaged(hours, data):
    """Hourly resampling: sums are conserved, means stay inside their bin.

    The whole-hour window means every bin is complete, so the accumulated
    variable's total is an exact invariant rather than an edge-dependent one.
    """
    # ARRANGE
    n = hours * STEPS_PER_HOUR
    draw_block = arrays(dtype=np.float64, shape=n, elements=finite_values)
    values_sum = data.draw(draw_block)
    values_mean = data.draw(draw_block)
    df_output = _synthetic_output(values_sum, values_mean)

    # ACT
    resampled = _resample_output(df_output, "60min")

    # ASSERT
    assert len(resampled) == hours
    np.testing.assert_allclose(
        resampled["SUEWS", SUM_VAR].sum(),
        values_sum.sum(),
        rtol=1e-12,
        atol=1e-9 * max(float(np.abs(values_sum).sum()), 1.0),
    )
    per_bin = values_mean.reshape(hours, STEPS_PER_HOUR)
    np.testing.assert_allclose(
        resampled["SUEWS", MEAN_VAR].to_numpy(),
        per_bin.mean(axis=1),
        rtol=1e-12,
        atol=1e-9 * max(float(np.abs(values_mean).max()), 1.0),
    )


# =============================================================================
# Forcing interpolation
# =============================================================================

TSTEP_IN_S = 3600
TSTEP_MOD_S = 300
RESAMPLE_RATIO = TSTEP_MOD_S / TSTEP_IN_S

forcing_series = arrays(
    dtype=np.float64,
    shape=array_shapes(min_dims=1, max_dims=1, min_side=2, max_side=24),
    elements=st.floats(
        min_value=-100.0, max_value=100.0, allow_nan=False, allow_infinity=False
    ),
)
precipitation_series = arrays(
    dtype=np.float64,
    shape=array_shapes(min_dims=1, max_dims=1, min_side=2, max_side=24),
    elements=st.floats(
        min_value=0.0, max_value=50.0, allow_nan=False, allow_infinity=False
    ),
)


def _hourly_frame(values, name="x"):
    index = pd.date_range("2012-01-01 01:00", periods=values.size, freq="3600s")
    return pd.DataFrame({name: values}, index=index)


@PROPERTY_SETTINGS
@given(values=forcing_series)
def test_linear_inst_interpolation_stays_within_input_range(values):
    """Interpolating instantaneous forcing cannot leave the input's range.

    Linear interpolation is a convex combination of two samples, and the
    edge fills repeat an existing sample, so no output value can exceed the
    extremes of the series it came from. A resampler that invents a value
    outside them has a sign, index or fill error.
    """
    # ARRANGE
    df_in = _hourly_frame(values)

    # ACT
    df_out = resample_linear_inst(df_in.copy(), TSTEP_IN_S, TSTEP_MOD_S)

    # ASSERT
    assert len(df_out) == values.size * (TSTEP_IN_S // TSTEP_MOD_S)
    assert np.isfinite(df_out.x.to_numpy()).all()
    span = max(float(values.max() - values.min()), 1.0)
    assert df_out.x.min() >= values.min() - 1e-9 * span
    assert df_out.x.max() <= values.max() + 1e-9 * span


@PROPERTY_SETTINGS
@given(values=forcing_series)
def test_linear_avg_interpolation_stays_within_input_range(values):
    """Interpolating period-average forcing cannot leave the input's range."""
    # ARRANGE
    df_in = _hourly_frame(values)

    # ACT
    df_out = resample_linear_avg(df_in.copy(), TSTEP_IN_S, TSTEP_MOD_S)

    # ASSERT
    assert len(df_out) == values.size * (TSTEP_IN_S // TSTEP_MOD_S)
    assert np.isfinite(df_out.x.to_numpy()).all()
    span = max(float(values.max() - values.min()), 1.0)
    assert df_out.x.min() >= values.min() - 1e-9 * span
    assert df_out.x.max() <= values.max() + 1e-9 * span


@PROPERTY_SETTINGS
@given(values=precipitation_series)
def test_sum_resampling_redistributes_without_creating_depth(values):
    """Spreading accumulated depth over finer steps neither creates nor loses it.

    Each hourly depth is spread evenly over its twelve five-minute steps, so
    no output step may exceed the largest input depth times the step ratio,
    none may be negative, and the total may differ from the input total by at
    most one input interval's worth -- the closing interval, which the
    function terminates rather than spreads. Anything beyond that is depth
    the resampler made up or dropped.
    """
    # ARRANGE
    df_in = _hourly_frame(values, name="rain")

    # ACT
    df_out = resample_sum(df_in.copy(), TSTEP_IN_S, TSTEP_MOD_S)

    # ASSERT
    out = df_out.rain.to_numpy()
    assert np.isfinite(out).all()
    assert (out >= -1e-12).all()
    assert out.max() <= values.max() * RESAMPLE_RATIO + 1e-9
    assert abs(out.sum() - values.sum()) <= values.max() + 1e-9
