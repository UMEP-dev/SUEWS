"""
Near-surface diagnostics attribution module for SUEWS.

Decomposes changes in near-surface variables (T2, q2, U10) between two scenarios
into physically attributable components using exact Shapley decomposition.

Supported Variables
-------------------
- **T2** (2m temperature): Linear flux-gradient profile
- **q2** (2m specific humidity): Linear flux-gradient profile
- **U10** (10m wind speed): Logarithmic momentum profile

Mathematical Foundation
-----------------------
**T2 and q2** follow linear flux-gradient profiles:
    T2 = T_ref + r_h * Q_H / (rho * c_p)
    q2 = q_ref + r_v * Q_E / (rho * L_v)

The change between scenarios can be decomposed as:
    delta_T2 = delta(r * Q_H * gamma)

Using Shapley values for triple products ensures exact closure.

**U10** follows the logarithmic wind profile:
    U10 = (u*/k) * [ln((z-d)/z0m) - psi_m(zeta)]

Which decomposes into:
    U10 = F * (R + S)

Where:
- F = u*/k (forcing term - surface stress)
- R = ln((z-d)/z0m) (roughness term - geometric)
- S = -psi_m(zeta) (stability correction)

The Shapley decomposition for this structure yields:
- Phi_forcing = dF * (P_A + P_B) / 2,  where P = R + S
- Phi_roughness = dR * (F_A + F_B) / 2
- Phi_stability = dS * (F_A + F_B) / 2
"""

from typing import Literal

import pandas as pd

# Import all public functions and classes
from ._result import AttributionResult
from ._t2 import attribute_t2, diagnose_t2
from ._q2 import attribute_q2, diagnose_q2
from ._u10 import attribute_u10, diagnose_u10


# =============================================================================
# Generic Dispatcher Functions
# =============================================================================


def attribute(
    df_output_A: pd.DataFrame,
    df_output_B: pd.DataFrame,
    variable: Literal["T2", "q2", "U10"] = "T2",
    **kwargs,
) -> AttributionResult:
    """
    Generic attribution function for near-surface variables.

    Dispatches to variable-specific implementation internally.

    Parameters
    ----------
    df_output_A : pd.DataFrame
        SUEWS output DataFrame for scenario A (reference/baseline)
    df_output_B : pd.DataFrame
        SUEWS output DataFrame for scenario B (modified/test)
    variable : str, optional
        Variable to attribute: 'T2' (temperature), 'q2' (humidity),
        or 'U10' (10m wind speed). Default 'T2'.
    **kwargs
        Additional keyword arguments passed to the specific function.
        - For T2: hierarchical (bool), min_flux (float), df_forcing_A/B
        - For q2: min_flux (float), df_forcing_A/B
        - For U10: z_ref (float), min_ustar (float)

    Returns
    -------
    AttributionResult
        Attribution decomposition results.

    Examples
    --------
    >>> result = attribute(df_baseline, df_scenario, variable="T2")
    >>> print(result)

    >>> result = attribute(df_baseline, df_scenario, variable="q2")
    >>> print(result)

    >>> result = attribute(df_baseline, df_scenario, variable="U10")
    >>> print(result)
    """
    if variable == "T2":
        return attribute_t2(df_output_A, df_output_B, **kwargs)
    elif variable == "q2":
        return attribute_q2(df_output_A, df_output_B, **kwargs)
    elif variable == "U10":
        return attribute_u10(df_output_A, df_output_B, **kwargs)
    else:
        raise ValueError(
            f"Unknown variable: {variable}. Supported variables: 'T2', 'q2', 'U10'"
        )


def diagnose(
    df_output: pd.DataFrame,
    variable: Literal["T2", "q2", "U10"] = "T2",
    **kwargs,
) -> AttributionResult:
    """
    Generic diagnostic function for near-surface variables.

    Automatically identifies anomalous values and attributes the causes.

    Parameters
    ----------
    df_output : pd.DataFrame
        SUEWS output DataFrame
    variable : str, optional
        Variable to diagnose: 'T2' (temperature), 'q2' (humidity),
        or 'U10' (10m wind speed). Default 'T2'.
    **kwargs
        Additional keyword arguments passed to the specific function.
        - method: 'anomaly', 'extreme', or 'diurnal'
        - threshold: float for anomaly detection
        - df_forcing: forcing DataFrame (T2, q2 only)
        - hierarchical: bool (T2 only)
        - z_ref: float (U10 only)

    Returns
    -------
    AttributionResult
        Attribution decomposition with diagnostic interpretation.

    Examples
    --------
    >>> result = diagnose(df_output, variable="T2", method="anomaly")
    >>> print(result)

    >>> result = diagnose(df_output, variable="q2", method="diurnal")
    >>> print(result)

    >>> result = diagnose(df_output, variable="U10", method="extreme")
    >>> print(result)
    """
    if variable == "T2":
        return diagnose_t2(df_output, **kwargs)
    elif variable == "q2":
        return diagnose_q2(df_output, **kwargs)
    elif variable == "U10":
        return diagnose_u10(df_output, **kwargs)
    else:
        raise ValueError(
            f"Unknown variable: {variable}. Supported variables: 'T2', 'q2', 'U10'"
        )


# Public API
__all__ = [
    # Variable-specific functions
    "attribute_t2",
    "attribute_q2",
    "attribute_u10",
    "diagnose_t2",
    "diagnose_q2",
    "diagnose_u10",
    # Generic dispatchers
    "attribute",
    "diagnose",
    # Result container
    "AttributionResult",
]
