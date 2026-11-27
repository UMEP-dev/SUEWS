"""Test suews_phys_anthro - anthropogenic heat flux calculations.

Regression tests for Issue #240 (zero population density division-by-zero guard
in the detailed anthropogenic heat branch, EmissionsMethod=5), scenario
sensitivity across multi-day (holiday -> working-day) runs, and exact daily
vehicle-energy conversion in ``QF_traff``.
"""

from importlib.resources import files

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig
from supy.util import SUEWS_MISSING

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-RESPONSE")]

REQUIRED_FLUXES = ("QF", "QH", "QE", "QN", "QS")

# Two full model days (576 five-minute steps):
# Day 1 is 2012-01-01 (Sunday / holiday, iu=2) and Day 2 is 2012-01-02 (Monday /
# working day, iu=1), exercising both diurnal profiles and weekday/holiday
# traffic rates.
N_DAYS = 2
RUN_STEPS = TIMESTEPS_PER_DAY * N_DAYS
SECONDS_PER_DAY = 86400.0
TRAFFIC_RATE_WEEKDAY = 0.05  # [veh km m-2 day-1]
TRAFFIC_RATE_HOLIDAY = 0.02  # [veh km m-2 day-1]
FLAT_TRAFFIC_RATE = 0.04  # [veh km m-2 day-1]
FLAT_VEHICLE_ENERGY_FACTOR = 4_320_000.0  # [J km-1] -> exact 2.0 W m-2


def _build_anthro_config(scenario: str) -> SUEWSConfig:
    """Build a scenario-specific SUEWSConfig on the detailed anthropogenic branch.

    Uses ``emissions=5`` (detailed anthropogenic branch with J11 heat basis)
    because that branch computes ``QF_build`` via division by ``DP_x_RhoPop``
    (where Issue #240 guarded against zero population density) and adds
    area-based traffic heat ``QF_traff`` independently of population.
    """
    data = yaml.safe_load(
        files("supy")
        .joinpath("sample_data/sample_config.yml")
        .read_text(encoding="utf-8")
    )
    data["model"]["physics"]["emissions"] = {"value": 5}
    anthro = data["sites"][0]["properties"]["anthropogenic_emissions"]
    heat = anthro["heat"]
    co2 = anthro["co2"]
    co2["type_traffic_rate"] = {"value": 1.0}  # [veh km m-2 day-1]

    if scenario == "zero_population":
        heat["popdensdaytime"] = {"working_day": 0.0, "holiday": 0.0}
        heat["popdensnighttime"] = 0.0
        co2["traffic_rate"] = {"working_day": 0.0, "holiday": 0.0}
    elif scenario == "zero_population_with_traffic":
        heat["popdensdaytime"] = {"working_day": 0.0, "holiday": 0.0}
        heat["popdensnighttime"] = 0.0
        co2["traffic_rate"] = {
            "working_day": TRAFFIC_RATE_WEEKDAY,
            "holiday": TRAFFIC_RATE_HOLIDAY,
        }
    elif scenario == "zero_population_flat_traffic":
        heat["popdensdaytime"] = {"working_day": 0.0, "holiday": 0.0}
        heat["popdensnighttime"] = 0.0
        co2["traffic_rate"] = {
            "working_day": FLAT_TRAFFIC_RATE,
            "holiday": FLAT_TRAFFIC_RATE,
        }
        co2["emission_factor_energy_vehicle"] = {"value": FLAT_VEHICLE_ENERGY_FACTOR}
        flat_profile = {str(hour): 1.0 for hour in range(1, 25)}
        co2["profile_traffic_24hr"] = {
            "working_day": flat_profile,
            "holiday": flat_profile,
        }
    elif scenario == "positive_population_with_traffic":
        co2["traffic_rate"] = {
            "working_day": TRAFFIC_RATE_WEEKDAY,
            "holiday": TRAFFIC_RATE_HOLIDAY,
        }
    else:
        raise ValueError(f"Unknown anthropogenic test scenario: {scenario!r}")

    return SUEWSConfig.from_dict(data)


@pytest.fixture(scope="module")
def anthro_expected_index(sample_forcing_parsed) -> pd.MultiIndex:
    """Derive the expected single-grid two-day (grid, datetime) index from inputs."""
    _, df_forcing_2d = sample_forcing_parsed(RUN_STEPS)
    expected_timestamps = pd.DatetimeIndex(df_forcing_2d.index)
    assert len(expected_timestamps) == RUN_STEPS, (
        f"Expected {RUN_STEPS} forcing timestamps, got {len(expected_timestamps)}"
    )
    assert expected_timestamps.is_unique, "Forcing timestamps must be unique"
    expected_grid = _build_anthro_config("zero_population").sites[0].gridiv
    return pd.MultiIndex.from_arrays(
        [[expected_grid] * RUN_STEPS, expected_timestamps],
        names=["grid", "datetime"],
    )


@pytest.fixture(scope="module")
def anthro_scenario_outputs(sample_forcing_parsed):
    """Run all anthropogenic scenarios once over an explicit two-day window."""
    _, df_forcing_2d = sample_forcing_parsed(RUN_STEPS)
    start_date = df_forcing_2d.index[0]
    end_date = df_forcing_2d.index[-1]

    outputs = {}
    for scenario in (
        "zero_population",
        "zero_population_with_traffic",
        "zero_population_flat_traffic",
        "positive_population_with_traffic",
    ):
        sim = SUEWSSimulation(_build_anthro_config(scenario))
        sim.update_forcing(df_forcing_2d)
        outputs[scenario] = sim.run(start_date=start_date, end_date=end_date).df
    return outputs


def _assert_finite_non_sentinel(values, *, label: str) -> np.ndarray:
    """Assert every value is finite (no NaN or +/-Inf) and not the -999.0 sentinel.

    Matches ``_run_rust._replace_sentinel_in_place`` exact ``== -999.0`` output
    sentinel semantics so legitimate large negative fluxes (e.g. ``-950`` or
    ``-1086.97 W m-2`` closing SEB) are accepted.
    """
    arr = np.asarray(values, dtype=float)
    assert arr.size > 0, f"{label} is empty"
    assert np.isfinite(arr).all(), f"{label} contains non-finite values (NaN or +/-Inf)"
    assert not np.equal(arr, SUEWS_MISSING).any(), (
        f"{label} contains SUEWS missing sentinel value ({SUEWS_MISSING})"
    )
    return arr


def _assert_scenario_fluxes_valid(
    df_output: pd.DataFrame,
    scenario: str,
    expected_index: pd.MultiIndex,
) -> None:
    """Verify exact (grid, datetime) index, uniqueness, finiteness, and sentinels."""
    assert isinstance(df_output.index, pd.MultiIndex), (
        f"Output for scenario={scenario} must use a (grid, datetime) MultiIndex"
    )
    assert list(df_output.index.names) == ["grid", "datetime"], (
        f"Unexpected index names for scenario={scenario}: {df_output.index.names!r}"
    )
    assert df_output.index.is_unique, (
        f"Output index for scenario={scenario} contains duplicate (grid, datetime) rows"
    )
    expected_grids = expected_index.get_level_values("grid").unique().tolist()
    actual_grids = df_output.index.get_level_values("grid").unique().tolist()
    assert actual_grids == expected_grids, (
        f"Grid ID mismatch for scenario={scenario}: "
        f"got {actual_grids}, expected {expected_grids}"
    )
    assert df_output.index.equals(expected_index), (
        f"Output (grid, datetime) index mismatch for scenario={scenario}: "
        f"got {len(df_output)} rows, expected {len(expected_index)}"
    )

    for flux in REQUIRED_FLUXES:
        flux_values = df_output["SUEWS"][flux]
        assert len(flux_values) == RUN_STEPS, (
            f"{flux} length mismatch for scenario={scenario}: "
            f"got {len(flux_values)}, expected {RUN_STEPS}"
        )
        _assert_finite_non_sentinel(
            flux_values.to_numpy(), label=f"{flux} (scenario={scenario})"
        )


def _assert_zero_population_contract(
    df_output: pd.DataFrame,
    scenario: str,
    expected_index: pd.MultiIndex,
) -> None:
    """Enforce zero-population scenario invariants across all timesteps."""
    _assert_scenario_fluxes_valid(df_output, scenario, expected_index)
    qf_values = df_output["SUEWS"]["QF"]
    assert (qf_values >= 0.0).all(), (
        f"QF should be non-negative for scenario={scenario}"
    )

    if scenario == "zero_population":
        # With zero population and zero traffic, QF_metab = QF_build = QF_traff = 0.
        np.testing.assert_allclose(qf_values.to_numpy(), 0.0, atol=1e-12)
    else:
        # With zero population and area-based traffic (TrafficUnits=1), traffic
        # drives strictly positive, diurnally varying QF even though QF_build=0.
        assert (qf_values > 0.0).all(), (
            "Expected strictly positive traffic QF with zero population"
        )
        assert qf_values.max() > qf_values.min() * 2.0, (
            "Expected clear diurnal traffic variation across a full 24-hour cycle"
        )

        # Day 1 (2012-01-01 Sunday, holiday) uses TRAFFIC_RATE_HOLIDAY (0.02);
        # Day 2 (2012-01-02 Monday, working day) uses TRAFFIC_RATE_WEEKDAY (0.05).
        # The sample traffic profile is identical on working days and holidays,
        # so Day 2 mean QF scales with the weekday/holiday traffic-rate ratio.
        qf_day1 = qf_values.iloc[:TIMESTEPS_PER_DAY]
        qf_day2 = qf_values.iloc[TIMESTEPS_PER_DAY:]
        expected_ratio = TRAFFIC_RATE_WEEKDAY / TRAFFIC_RATE_HOLIDAY
        assert qf_day2.mean() / qf_day1.mean() == pytest.approx(
            expected_ratio, rel=0.05
        )


def _assert_flat_traffic_contract(
    df_flat: pd.DataFrame,
    expected_index: pd.MultiIndex,
) -> None:
    """Verify finiteness and exact daily vehicle-energy conversion to W m-2."""
    _assert_scenario_fluxes_valid(
        df_flat, "zero_population_flat_traffic", expected_index
    )
    qf_flat = df_flat["SUEWS"]["QF"]
    expected_watts = FLAT_TRAFFIC_RATE * FLAT_VEHICLE_ENERGY_FACTOR / SECONDS_PER_DAY
    np.testing.assert_allclose(qf_flat.to_numpy(), expected_watts, rtol=0.0, atol=1e-10)


def _assert_distinct_scenarios_contract(
    outputs: dict[str, pd.DataFrame],
    expected_index: pd.MultiIndex,
) -> None:
    """Verify finite, sentinel-free outputs and distinct scenario QF ordering."""
    for name in (
        "zero_population",
        "zero_population_with_traffic",
        "positive_population_with_traffic",
    ):
        _assert_scenario_fluxes_valid(outputs[name], name, expected_index)

    qf_zero = outputs["zero_population"]["SUEWS"]["QF"].to_numpy()
    qf_traffic = outputs["zero_population_with_traffic"]["SUEWS"]["QF"].to_numpy()
    qf_populated = outputs["positive_population_with_traffic"]["SUEWS"]["QF"].to_numpy()

    # Traffic adds non-trivial heat flux above the zero-population zero-traffic case.
    assert (qf_traffic > qf_zero + 0.1).all()

    # Adding population activates building + metabolic heat above traffic alone.
    assert (qf_populated > qf_traffic + 1.0).all()


@pytest.mark.parametrize(
    "scenario",
    [
        "zero_population",
        "zero_population_with_traffic",
    ],
)
def test_zero_population_qf(scenario, anthro_scenario_outputs, anthro_expected_index):
    """QF stays finite and obeys scenario physics when population density is zero.

    Regression test for issue #240 exercising the detailed anthropogenic branch
    over two full days (Sunday holiday followed by Monday working day).
    """
    _assert_zero_population_contract(
        anthro_scenario_outputs[scenario], scenario, anthro_expected_index
    )


def test_flat_profile_converts_daily_vehicle_energy_to_watts(
    anthro_scenario_outputs, anthro_expected_index
):
    """Exact daily-energy conservation control for area-based traffic heat flux.

    With zero population, TrafficUnits=1, and a flat unit 24-hour profile,
    QF_traff must equal ``traffic_rate * emission_factor_energy_vehicle / 86400``
    at every timestep (0.04 veh km m-2 day-1 * 4.32 MJ km-1 / 86400 s = 2.0 W m-2).
    """
    _assert_flat_traffic_contract(
        anthro_scenario_outputs["zero_population_flat_traffic"],
        anthro_expected_index,
    )


def test_anthro_scenarios_produce_distinct_qf(
    anthro_scenario_outputs, anthro_expected_index
):
    """Each anthropogenic scenario produces physically distinct QF trajectories."""
    _assert_distinct_scenarios_contract(anthro_scenario_outputs, anthro_expected_index)


@pytest.mark.parametrize(
    ("scenario", "flux", "bad_value"),
    [
        ("zero_population", "QH", np.inf),
        ("zero_population", "QH", -np.inf),
        ("zero_population", "QH", np.nan),
        ("zero_population", "QE", SUEWS_MISSING),
        ("zero_population_with_traffic", "QH", np.inf),
        ("zero_population_with_traffic", "QE", SUEWS_MISSING),
        ("zero_population_flat_traffic", "QH", np.inf),
        ("zero_population_flat_traffic", "QE", SUEWS_MISSING),
        ("positive_population_with_traffic", "QF", np.inf),
        ("positive_population_with_traffic", "QE", SUEWS_MISSING),
    ],
)
def test_anthro_guards_reject_nonfinite_and_sentinel_values(
    anthro_scenario_outputs, anthro_expected_index, scenario, flux, bad_value
):
    """Injecting +/-Inf, NaN, or -999 into any required flux fails assertions."""
    corrupted = {name: df.copy() for name, df in anthro_scenario_outputs.items()}
    corrupted[scenario].iloc[
        56, corrupted[scenario].columns.get_loc(("SUEWS", flux))
    ] = bad_value

    if scenario in {"zero_population", "zero_population_with_traffic"}:
        with pytest.raises(AssertionError):
            _assert_zero_population_contract(
                corrupted[scenario], scenario, anthro_expected_index
            )
    elif scenario == "zero_population_flat_traffic":
        with pytest.raises(AssertionError):
            _assert_flat_traffic_contract(corrupted[scenario], anthro_expected_index)
    else:
        with pytest.raises(AssertionError):
            _assert_distinct_scenarios_contract(corrupted, anthro_expected_index)


@pytest.mark.parametrize("valid_negative_flux", [-950.0, -1086.97])
def test_anthro_guards_accept_valid_large_negative_flux(
    anthro_scenario_outputs, anthro_expected_index, valid_negative_flux
):
    """Finite negative fluxes below -900 W m-2 (not -999.0) are valid outputs."""
    df_valid = anthro_scenario_outputs["zero_population"].copy()
    df_valid.iloc[56, df_valid.columns.get_loc(("SUEWS", "QH"))] = valid_negative_flux
    _assert_zero_population_contract(df_valid, "zero_population", anthro_expected_index)


def _corrupt_anthro_index(df: pd.DataFrame, corruption: str) -> pd.DataFrame:
    """Apply a grid/timestamp counterexample while preserving real flux values."""
    times = df.index.get_level_values("datetime")
    if corruption == "grid_99_shifted_400d":
        bad_index = pd.MultiIndex.from_arrays(
            [[99] * len(df), times + pd.Timedelta(days=400)],
            names=["grid", "datetime"],
        )
        return df.set_axis(bad_index)
    if corruption == "day2_duplicate_of_day1":
        day1 = df.iloc[:TIMESTEPS_PER_DAY]
        return pd.concat([day1, day1])
    if corruption == "two_one_day_grids":
        day1 = df.iloc[:TIMESTEPS_PER_DAY]
        day1_times = day1.index.get_level_values("datetime")
        base_grid = int(day1.index.get_level_values("grid")[0])
        bad_index = pd.MultiIndex.from_arrays(
            [
                [base_grid] * TIMESTEPS_PER_DAY + [base_grid + 1] * TIMESTEPS_PER_DAY,
                day1_times.append(day1_times),
            ],
            names=["grid", "datetime"],
        )
        return pd.concat([day1, day1]).set_axis(bad_index)
    raise ValueError(f"Unknown index corruption mode: {corruption!r}")


@pytest.mark.parametrize(
    "corruption",
    [
        "grid_99_shifted_400d",
        "day2_duplicate_of_day1",
        "two_one_day_grids",
    ],
)
def test_anthro_guards_reject_corrupted_grid_and_timestamps(
    anthro_scenario_outputs, anthro_expected_index, corruption
):
    """Grid shifts, duplicate day-1 rows, and two 1-day grids fail all contracts."""
    corrupted = {
        name: _corrupt_anthro_index(df, corruption)
        for name, df in anthro_scenario_outputs.items()
    }

    with pytest.raises(AssertionError):
        _assert_zero_population_contract(
            corrupted["zero_population"], "zero_population", anthro_expected_index
        )
    with pytest.raises(AssertionError):
        _assert_zero_population_contract(
            corrupted["zero_population_with_traffic"],
            "zero_population_with_traffic",
            anthro_expected_index,
        )
    with pytest.raises(AssertionError):
        _assert_flat_traffic_contract(
            corrupted["zero_population_flat_traffic"], anthro_expected_index
        )
    with pytest.raises(AssertionError):
        _assert_distinct_scenarios_contract(corrupted, anthro_expected_index)


def test_day_night_profile_defaults_and_shipped_scale_metabolic_bounds(
    sample_forcing_parsed,
):
    """Omitted and shipped day-night profiles yield physically bounded metabolic CO2 (#2043).

    Under ``emissions=12`` (J11 heat + anthropogenic CO2), omitting
    ``popprof_24hr`` and ``profile_human_activity_24hr`` defaults to uniform
    ``1.0`` (night level), so ``FcMetab`` equals the exact nighttime metabolic
    flux ``PopDensNighttime * MinFCMetab / 10000`` at every timestep (instead
    of turning negative under the former ``1/24`` default). With the shipped
    ``[1.0, 2.0]`` profiles (801/802 and 55663/55664), ``FcMetab`` transitions
    between the nighttime minimum and full daytime maximum
    ``PopDensDaytime * MaxFCMetab / 10000``.
    """
    _, df_forcing_2d = sample_forcing_parsed(RUN_STEPS)
    start_date = df_forcing_2d.index[0]
    end_date = df_forcing_2d.index[-1]

    raw_shipped = yaml.safe_load(
        files("supy")
        .joinpath("sample_data/sample_config.yml")
        .read_text(encoding="utf-8")
    )
    raw_shipped["model"]["physics"]["emissions"] = {"value": 12}
    anthro_s = raw_shipped["sites"][0]["properties"]["anthropogenic_emissions"]
    pop_night = float(anthro_s["heat"]["popdensnighttime"])
    pop_day = float(anthro_s["heat"]["popdensdaytime"]["working_day"])
    min_fc = float(anthro_s["co2"]["emission_co2_metabolism_min"]["value"])
    max_fc = float(anthro_s["co2"]["emission_co2_metabolism_max"]["value"])
    expected_fc_night = pop_night * min_fc / 10000.0
    expected_fc_day = pop_day * max_fc / 10000.0

    # 1. Shipped [1.0, 2.0] profiles reach both night minimum and full day maximum.
    sim_shipped = SUEWSSimulation(SUEWSConfig.from_dict(raw_shipped))
    sim_shipped.update_forcing(df_forcing_2d)
    fc_metab_shipped = sim_shipped.run(
        start_date=start_date, end_date=end_date
    ).df["SUEWS"]["FcMetab"].to_numpy(dtype=float)
    assert np.isfinite(fc_metab_shipped).all()
    assert fc_metab_shipped.min() == pytest.approx(expected_fc_night, rel=1e-6)
    assert fc_metab_shipped.max() == pytest.approx(expected_fc_day, rel=1e-6)

    # 2. Omitted profiles default to uniform 1.0 (pure night baseline, never negative).
    raw_default = yaml.safe_load(
        files("supy")
        .joinpath("sample_data/sample_config.yml")
        .read_text(encoding="utf-8")
    )
    raw_default["model"]["physics"]["emissions"] = {"value": 12}
    raw_default["sites"][0]["properties"]["anthropogenic_emissions"]["heat"].pop(
        "popprof_24hr", None
    )
    raw_default["sites"][0]["properties"]["anthropogenic_emissions"]["co2"].pop(
        "profile_human_activity_24hr", None
    )
    sim_default = SUEWSSimulation(SUEWSConfig.from_dict(raw_default))
    sim_default.update_forcing(df_forcing_2d)
    fc_metab_default = sim_default.run(
        start_date=start_date, end_date=end_date
    ).df["SUEWS"]["FcMetab"].to_numpy(dtype=float)
    np.testing.assert_allclose(
        fc_metab_default, expected_fc_night, rtol=1e-6, atol=1e-12
    )
