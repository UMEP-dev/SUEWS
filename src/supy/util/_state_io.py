"""State persistence utilities for SUEWS/SuPy.

This module provides functionality to save and load simulation states,
separating internal runtime values from user-configurable parameters.
"""

from pathlib import Path
import pickle
from typing import Any, Dict, Optional, Union
import warnings

import pandas as pd
import yaml

from ..data_model import InitialStates, SUEWSConfig


def extract_user_facing_state(
    df_state: pd.DataFrame, config: Optional[SUEWSConfig] = None
) -> Dict[str, Any]:
    """Extract user-configurable values from state DataFrame.

    Args:
        df_state: State DataFrame
        config: Optional SUEWSConfig for structure reference

    Returns:
        dict: User-facing initial state values
    """
    # Get first grid_id (assumes single grid for now)
    grid_id = df_state.index[0] if len(df_state) > 0 else 1

    user_state = {
        "initial_states": {
            # Surface water states
            "soilstore": {
                "paved": float(df_state.loc[grid_id, ("soilstore_id", "(0,)")]),
                "bldgs": float(df_state.loc[grid_id, ("soilstore_id", "(1,)")]),
                "evetr": float(df_state.loc[grid_id, ("soilstore_id", "(2,)")]),
                "dectr": float(df_state.loc[grid_id, ("soilstore_id", "(3,)")]),
                "grass": float(df_state.loc[grid_id, ("soilstore_id", "(4,)")]),
                "bsoil": float(df_state.loc[grid_id, ("soilstore_id", "(5,)")]),
                "water": float(df_state.loc[grid_id, ("soilstore_id", "(6,)")]),
            },
            # Surface states
            "state": {
                "paved": float(df_state.loc[grid_id, ("state_id", "(0,)")]),
                "bldgs": float(df_state.loc[grid_id, ("state_id", "(1,)")]),
                "evetr": float(df_state.loc[grid_id, ("state_id", "(2,)")]),
                "dectr": float(df_state.loc[grid_id, ("state_id", "(3,)")]),
                "grass": float(df_state.loc[grid_id, ("state_id", "(4,)")]),
                "bsoil": float(df_state.loc[grid_id, ("state_id", "(5,)")]),
                "water": float(df_state.loc[grid_id, ("state_id", "(6,)")]),
            },
            # Snow states (if present)
            "snow": _extract_snow_state(df_state, grid_id),
            # Vegetation states
            "vegetation": _extract_vegetation_state(df_state, grid_id),
            # Temperature states
            "temperature": _extract_temperature_state(df_state, grid_id),
            # Other user-configurable states
            "tair_av": float(df_state.loc[grid_id, ("tair_av", "0")]),
        }
    }

    # Remove empty sections
    user_state["initial_states"] = {
        k: v
        for k, v in user_state["initial_states"].items()
        if v and any(vv != 0.0 for vv in (v.values() if isinstance(v, dict) else [v]))
    }

    return user_state


def _extract_snow_state(
    df_state: pd.DataFrame, grid_id: int
) -> Optional[Dict[str, Any]]:
    """Extract snow-related state if snow is enabled."""
    try:
        snow_state = {}
        for i, surf in enumerate([
            "paved",
            "bldgs",
            "evetr",
            "dectr",
            "grass",
            "bsoil",
            "water",
        ]):
            snow_state[surf] = {
                "snowpack": float(df_state.loc[grid_id, ("snowpack", f"({i},)")]),
                "snowfrac": float(df_state.loc[grid_id, ("snowfrac", f"({i},)")]),
                "snowwater": float(df_state.loc[grid_id, ("snowwater", f"({i},)")]),
                "snowdens": float(df_state.loc[grid_id, ("snowdens", f"({i},)")]),
            }
        return snow_state
    except KeyError:
        return None


def _extract_vegetation_state(
    df_state: pd.DataFrame, grid_id: int
) -> Optional[Dict[str, Any]]:
    """Extract vegetation-specific state."""
    try:
        veg_state = {}
        for i, veg in enumerate(["evetr", "dectr", "grass"]):
            veg_state[veg] = {
                "lai": float(df_state.loc[grid_id, ("lai_id", f"({i},)")]),
                "gdd": float(df_state.loc[grid_id, ("gdd_id", f"({i},)")]),
                "sdd": float(df_state.loc[grid_id, ("sdd_id", f"({i},)")]),
            }
        return veg_state
    except KeyError:
        return None


def _extract_temperature_state(
    df_state: pd.DataFrame, grid_id: int
) -> Optional[Dict[str, Any]]:
    """Extract temperature state for surfaces."""
    try:
        temp_state = {}
        # Surface temperatures
        for i, surf in enumerate([
            "paved",
            "bldgs",
            "evetr",
            "dectr",
            "grass",
            "bsoil",
            "water",
        ]):
            temps = []
            for layer in range(5):
                temps.append(float(df_state.loc[grid_id, ("temp", f"({i},{layer})")]))
            temp_state[surf] = temps
        return temp_state
    except KeyError:
        return None


def load_initial_state(
    yaml_path: Union[str, Path],
    pickle_path: Optional[Union[str, Path]] = None,
    strict: bool = False,
) -> InitialStates:
    """Load initial state from YAML and optional pickle file.

    Args:
        yaml_path: Path to user-facing YAML configuration
        pickle_path: Optional path to complete state pickle
        strict: If True, raise errors for missing pickle file

    Returns:
        InitialStates: Loaded initial state object

    Raises:
        FileNotFoundError: If yaml_path doesn't exist
        ValueError: If pickle file is incompatible
    """
    yaml_path = Path(yaml_path)
    if not yaml_path.exists():
        raise FileNotFoundError(f"YAML file not found: {yaml_path}")

    # Start with default initial state
    initial_state = InitialStates()

    # Load complete state from pickle if available
    if pickle_path:
        pickle_path = Path(pickle_path)
        if pickle_path.exists():
            try:
                with open(pickle_path, "rb") as f:
                    pickle_data = pickle.load(f)

                # Check version compatibility
                if "version" in pickle_data:
                    saved_version = pickle_data["version"]
                    # Simple version check - could be more sophisticated
                    if not saved_version.startswith("2025"):
                        warnings.warn(
                            f"Pickle file was saved with SUEWS version {saved_version}, "
                            "which may not be fully compatible with current version."
                        )

                # Apply internal state values
                if "df_state" in pickle_data:
                    _apply_internal_state(initial_state, pickle_data["df_state"])

            except Exception as e:
                if strict:
                    raise ValueError(f"Failed to load pickle file: {e}")
                else:
                    warnings.warn(f"Could not load pickle file, using defaults: {e}")
        elif strict:
            raise FileNotFoundError(f"Pickle file not found: {pickle_path}")

    # Override with user-provided YAML values
    with open(yaml_path, "r", encoding="utf-8") as f:
        # Skip comment header
        lines = f.readlines()
        yaml_content = "".join([
            line for line in lines if not line.strip().startswith("#")
        ])
        user_config = yaml.safe_load(yaml_content)

    if user_config and "initial_states" in user_config:
        _apply_user_state(initial_state, user_config["initial_states"])

    return initial_state


def _apply_internal_state(initial_state: InitialStates, df_state: pd.DataFrame) -> None:
    """Apply internal state values from pickle to InitialStates object."""
    # This would apply internal fields like dqndt, dqnsdt, etc.
    # For now, focusing on the structure - actual implementation would
    # need to map DataFrame columns to InitialStates attributes

    grid_id = df_state.index[0] if len(df_state) > 0 else 1

    # Apply internal tracking fields (these are marked as internal_only)
    internal_fields = [
        "dqndt",
        "dqnsdt",
        "dt_since_start",
        "tstep_prev",
        "lenday_id",
        "tmax_id",
        "tmin_id",
        "qn_av",
        "qn_s_av",
        "snowfallcum",
    ]

    for field in internal_fields:
        if (field, "0") in df_state.columns:
            try:
                setattr(
                    initial_state, field, float(df_state.loc[grid_id, (field, "0")])
                )
            except Exception:
                pass  # Field might not exist in this version


def _apply_user_state(
    initial_state: InitialStates, user_config: Dict[str, Any]
) -> None:
    """Apply user-provided state values to InitialStates object."""
    # Apply soilstore values
    if "soilstore" in user_config:
        for surf, value in user_config["soilstore"].items():
            if hasattr(initial_state, surf):
                getattr(initial_state, surf).soilstore = value

    # Apply surface states
    if "state" in user_config:
        for surf, value in user_config["state"].items():
            if hasattr(initial_state, surf):
                getattr(initial_state, surf).state = value

    # Apply vegetation states
    if "vegetation" in user_config:
        for veg, veg_state in user_config["vegetation"].items():
            if hasattr(initial_state, veg):
                surf = getattr(initial_state, veg)
                if "lai" in veg_state:
                    surf.lai_id = {"value": veg_state["lai"]}
                if "gdd" in veg_state:
                    surf.gdd_id = {"value": veg_state["gdd"]}
                if "sdd" in veg_state:
                    surf.sdd_id = {"value": veg_state["sdd"]}

    # Apply other user-configurable fields
    if "tair_av" in user_config:
        initial_state.tair_av = user_config["tair_av"]
