"""
Model-performance attribution for surface energy fluxes.

This module diagnoses model error against observations. It complements the
scenario-to-scenario attribution tools for T2/q2/U10 by treating observations as
the reference state and model output as the evaluated state.
"""

from __future__ import annotations

from typing import Literal

import numpy as np
import pandas as pd

from ._core import shapley_binary_product
from ._helpers import align_scenarios, extract_suews_group
from ._physics import decompose_flux_budget
from ._result import AttributionResult


FluxVariable = Literal["QE", "QH"]


def _normalise_columns(df: pd.DataFrame) -> pd.DataFrame:
    """Return a flat DataFrame with common SUEWS flux column names."""
    out = extract_suews_group(df).copy()
    rename = {col: str(col).upper() for col in out.columns}
    out = out.rename(columns=rename)
    return out


def _ensure_qs(df: pd.DataFrame) -> pd.DataFrame:
    """Ensure QS is available, using the surface energy residual if needed."""
    if "QS" not in df.columns:
        required = {"QN", "QF", "QH", "QE"}
        missing = required - set(df.columns)
        if missing:
            raise ValueError(
                "QS is required or must be derivable from QN + QF - QH - QE; "
                f"missing columns: {sorted(missing)}"
            )
        df = df.copy()
        df["QS"] = df["QN"] + df["QF"] - df["QH"] - df["QE"]
    return df


def _required_flux_frame(df: pd.DataFrame, label: str) -> pd.DataFrame:
    df = _ensure_qs(_normalise_columns(df))
    required = {"QN", "QF", "QH", "QE", "QS"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"{label} is missing required columns: {sorted(missing)}")
    return df


def _available_energy(df: pd.DataFrame) -> np.ndarray:
    """Available turbulent energy A = QN + QF - QS."""
    return (df["QN"] + df["QF"] - df["QS"]).to_numpy(dtype=float)


def _safe_fraction(numerator: np.ndarray, denominator: np.ndarray, min_energy: float) -> np.ndarray:
    with np.errstate(divide="ignore", invalid="ignore"):
        return np.where(np.abs(denominator) >= min_energy, numerator / denominator, np.nan)


def diagnose_flux_performance(
    df_obs: pd.DataFrame,
    df_model: pd.DataFrame,
    variable: FluxVariable = "QE",
    *,
    min_energy: float = 1.0,
) -> AttributionResult:
    """
    Attribute model error in QE or QH to energy supply and partitioning.

    The target flux is represented as:

    ``flux = available_energy * flux_fraction``

    where ``available_energy = QN + QF - QS`` and ``flux_fraction`` is either
    ``QE / available_energy`` or ``QH / available_energy``. The model-observation
    difference is decomposed exactly with a binary Shapley decomposition:

    ``delta_flux = contribution_available_energy + contribution_partition``

    The available-energy contribution is then allocated to ``QN``, ``QF`` and
    ``QS`` according to their signed contribution to the available-energy
    difference.

    Parameters
    ----------
    df_obs, df_model
        Observation and model output data. Columns may be flat or SUEWS
        MultiIndex output. Required variables are QN, QF, QH, QE and QS. If QS
        is absent it is derived as QN + QF - QH - QE.
    variable
        Flux to diagnose, either ``"QE"`` or ``"QH"``.
    min_energy
        Minimum absolute available energy used when calculating flux fractions.
        Timesteps below this threshold are retained in ``delta_total`` but their
        component attribution is set to NaN.

    Returns
    -------
    AttributionResult
        Contributions in W m-2. Positive values mean the model is larger than
        the observation.
    """
    if variable not in {"QE", "QH"}:
        raise ValueError("diagnose_flux_performance currently supports 'QE' and 'QH'")

    obs = _required_flux_frame(df_obs, "df_obs")
    model = _required_flux_frame(df_model, "df_model")
    obs, model, common_idx = align_scenarios(obs, model)

    flux_obs = obs[variable].to_numpy(dtype=float)
    flux_model = model[variable].to_numpy(dtype=float)
    a_obs = _available_energy(obs)
    a_model = _available_energy(model)

    fraction_obs = _safe_fraction(flux_obs, a_obs, min_energy)
    fraction_model = _safe_fraction(flux_model, a_model, min_energy)

    phi_a, phi_fraction = shapley_binary_product(
        a_obs, a_model, fraction_obs, fraction_model
    )

    components_obs = {
        "QN": obs["QN"].to_numpy(dtype=float),
        "QF": obs["QF"].to_numpy(dtype=float),
        "QS": -obs["QS"].to_numpy(dtype=float),
    }
    components_model = {
        "QN": model["QN"].to_numpy(dtype=float),
        "QF": model["QF"].to_numpy(dtype=float),
        "QS": -model["QS"].to_numpy(dtype=float),
    }
    energy_breakdown = decompose_flux_budget(
        a_obs, a_model, components_obs, components_model, phi_a
    )

    contributions = pd.DataFrame(
        {
            "delta_total": flux_model - flux_obs,
            "available_energy": phi_a,
            "partition": phi_fraction,
            "energy_QN": energy_breakdown["QN"],
            "energy_QF": energy_breakdown["QF"],
            "energy_QS": energy_breakdown["QS"],
            "obs_available_energy": a_obs,
            "model_available_energy": a_model,
            "obs_fraction": fraction_obs,
            "model_fraction": fraction_model,
        },
        index=common_idx,
    )
    contributions["flag_low_available_energy"] = (
        (np.abs(a_obs) < min_energy) | (np.abs(a_model) < min_energy)
    )

    summary = contributions.describe().T[["mean", "std", "min", "max"]]
    metadata = {
        "n_timesteps": len(common_idx),
        "period_start": str(common_idx.min()),
        "period_end": str(common_idx.max()),
        "min_energy": min_energy,
        "n_low_available_energy_flagged": int(
            contributions["flag_low_available_energy"].sum()
        ),
        "unit": "W m-2",
        "interpretation": "positive contributions mean model exceeds observation",
    }

    return AttributionResult(
        variable=variable,
        contributions=contributions,
        summary=summary,
        metadata=metadata,
    )
