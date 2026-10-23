"""Consistency of the soil-moisture conductance columns in the debug output.

The debug group writes three columns that describe the soil-moisture term of
the surface conductance (``suews_phys_resist.f95``):

* ``dsp`` -- the soil moisture deficit threshold ``S1/G_sm + S2``;
* ``G_sm`` -- the sensitivity parameter;
* ``x_exp`` -- the argument of the exponential, ``G_sm*(vsmd - dsp)``.

``x_exp`` was written as ``G_sm*(vsmd - S1/G_sm + S2)`` (gh#1877): ``S2``
entered with the wrong sign, so the column disagreed with the threshold
written beside it by ``2*G_sm*S2``. The sample configuration has ``S2 = 0``,
which hides the error, so the test sets a non-zero ``S2``.
"""

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pytest

pytestmark = pytest.mark.physics

# Any non-zero value exposes the sign error; 20 mm lies in the documented
# 10-100 mm range of the deficit threshold.
S2_MM = 20.0


@pytest.mark.core
def test_x_exp_is_g_sm_times_deficit_beyond_threshold(short_sample_sim):
    """``x_exp`` must equal ``G_sm*(vsmd - dsp)`` at every timestep."""
    sim = short_sample_sim()
    conductance = sim._config.sites[0].properties.conductance
    conductance.s2.value = S2_MM
    s1 = conductance.s1.value
    sim._df_state_init = sim._config.to_df_state()
    df_forcing = sim.forcing.df.iloc[:TIMESTEPS_PER_DAY].copy()
    sim.update_forcing(df_forcing)

    df_output = sim.run(end_date=df_forcing.index[-1]).df
    df_debug = df_output.xs("debug", level="group", axis=1).xs(1, level="grid")

    g_sm = df_debug["G_sm"]
    # The override must reach the kernel, or the check below proves nothing.
    np.testing.assert_allclose(df_debug["dsp"], s1 / g_sm + S2_MM, rtol=1e-12)
    np.testing.assert_allclose(
        df_debug["x_exp"].to_numpy(),
        (g_sm * (df_debug["vsmd"] - df_debug["dsp"])).to_numpy(),
        rtol=1e-12,
        atol=1e-12,
        err_msg="debug x_exp is not G_sm*(vsmd - dsp) (gh#1877)",
    )
