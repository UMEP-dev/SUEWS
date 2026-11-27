import json
import math

import numpy as np
import pandas as pd

from .._misc import infer_freq_offset

_SURFACE_NAMES = ("paved", "bldg", "evetr", "dectr", "grass", "bsoil", "water")
_VEG_SURFACE_SPECS = (
    # (grid_idx, veg_idx, veg_name, df_alb_col, ckpt_alb_key)
    (2, 0, "evetr", "albevetr_id", "alb_evetr_id"),
    (3, 1, "dectr", "albdectr_id", "alb_dectr_id"),
    (4, 2, "grass", "albgrass_id", "alb_grass_id"),
)
_DECTR_GRID_IDX = 3


def _sanitise_values(values: list) -> list[float]:
    return [math.nan if v is None else float(v) for v in values]


def _decode_spinup_checkpoint(checkpoint) -> tuple[dict[int, dict], dict[int, dict]]:
    """Decode evolved hydro and phenology state dictionaries per pure-cover grid."""
    from .. import suews_bridge as sb  # noqa: PLC0415

    if checkpoint is None or not getattr(checkpoint, "grid_states", None):
        raise RuntimeError(
            "Spin-up simulation did not produce a final checkpoint state."
        )

    hydro_by_grid: dict[int, dict] = {}
    pheno_by_grid: dict[int, dict] = {}
    for grid_idx in range(len(_SURFACE_NAMES)):
        if grid_idx not in checkpoint.grid_states:
            raise RuntimeError(
                f"Spin-up checkpoint is missing final state for grid {grid_idx}."
            )
        payload = json.loads(checkpoint.grid_states[grid_idx])
        state_obj = payload.get("state", payload)
        members = state_obj["members"]
        hydro_raw = members["hydro_state"]
        pheno_raw = members["phenology_state"]

        hydro_state = sb.HydroState.from_values_payload(
            hydro_raw["schema_version"],
            _sanitise_values(hydro_raw["values"]),
            hydro_raw["dims"],
        )
        pheno_state = sb.PhenologyState.from_values_payload(
            pheno_raw["schema_version"],
            _sanitise_values(pheno_raw["values"]),
        )
        hydro_by_grid[grid_idx] = hydro_state.to_dict()
        pheno_by_grid[grid_idx] = pheno_state.to_dict()

    return hydro_by_grid, pheno_by_grid


def _apply_grid_evolved_state(
    df_target: pd.DataFrame,
    row_selector,
    hydro_dict: dict[str, float],
    pheno_dict: dict[str, float],
) -> None:
    """Write one grid's evolved hydro and phenology states into ``df_target``."""
    top_cols = set(df_target.columns.get_level_values(0))
    for surf_idx, surf_name in enumerate(_SURFACE_NAMES):
        idx_col = f"({surf_idx},)"
        if "soilstore_surf" in top_cols:
            df_target.loc[row_selector, ("soilstore_surf", idx_col)] = hydro_dict[
                f"soilstore_surf.{surf_name}"
            ]
        if "state_surf" in top_cols:
            df_target.loc[row_selector, ("state_surf", idx_col)] = hydro_dict[
                f"state_surf.{surf_name}"
            ]
        if "alb" in top_cols:
            df_target.loc[row_selector, ("alb", idx_col)] = pheno_dict[
                f"alb.{surf_name}"
            ]

    for _, veg_idx, veg_name, df_alb_col, ckpt_alb_key in _VEG_SURFACE_SPECS:
        for state_var in ("lai_id", "gdd_id", "sdd_id"):
            if state_var in top_cols:
                df_target.loc[row_selector, (state_var, f"({veg_idx},)")] = pheno_dict[
                    f"{state_var}.{veg_name}"
                ]
        if df_alb_col in top_cols:
            df_target.loc[row_selector, df_alb_col] = pheno_dict[ckpt_alb_key]

    for var in ("porosity_id", "decidcap_id"):
        if var in top_cols:
            df_target.loc[row_selector, var] = pheno_dict[var]


def _assign_spinup_state(
    df_target: pd.DataFrame,
    hydro_by_grid: dict[int, dict[str, float]],
    pheno_by_grid: dict[int, dict[str, float]],
) -> None:
    """Transfer each surface's evolved state from its pure-cover spin-up grid."""
    top_cols = set(df_target.columns.get_level_values(0))
    for surf_idx, surf_name in enumerate(_SURFACE_NAMES):
        idx_col = f"({surf_idx},)"
        if "soilstore_surf" in top_cols:
            df_target.loc[:, ("soilstore_surf", idx_col)] = hydro_by_grid[surf_idx][
                f"soilstore_surf.{surf_name}"
            ]
        if "state_surf" in top_cols:
            df_target.loc[:, ("state_surf", idx_col)] = hydro_by_grid[surf_idx][
                f"state_surf.{surf_name}"
            ]
        if "alb" in top_cols:
            df_target.loc[:, ("alb", idx_col)] = pheno_by_grid[surf_idx][
                f"alb.{surf_name}"
            ]

    for grid_idx, veg_idx, veg_name, df_alb_col, ckpt_alb_key in _VEG_SURFACE_SPECS:
        for state_var in ("lai_id", "gdd_id", "sdd_id"):
            if state_var in top_cols:
                df_target.loc[:, (state_var, f"({veg_idx},)")] = pheno_by_grid[
                    grid_idx
                ][f"{state_var}.{veg_name}"]
        if df_alb_col in top_cols:
            df_target.loc[:, df_alb_col] = pheno_by_grid[grid_idx][ckpt_alb_key]

    # Properties specific to deciduous trees (grid 3 = 100% dectr).
    for var in ("porosity_id", "decidcap_id"):
        if var in top_cols:
            df_target.loc[:, var] = pheno_by_grid[_DECTR_GRID_IDX][var]


def get_spinup_state(
    df_state: pd.DataFrame,
    df_forcing: pd.DataFrame,
    start_analysis: str,
    spinup_days: int = 365,
    save_spinup: bool = False,
) -> pd.DataFrame | tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """
    Get the spin-up state for the model.

    Parameters
    ----------
    df_state : pd.DataFrame
        Dataframe containing the initial state of the model.
    df_forcing : pd.DataFrame
        Dataframe containing the forcing data for the model. Duplication
        requires a regular, increasing DatetimeIndex. Its frequency is
        inferred from the timestamps if frequency metadata is absent.
    start_analysis : str
        Start date of the analysis period in the format 'YYYY-MM-DD'.
    spinup_days : int, optional
        Number of days to spin up the model, by default 365.
    save_spinup : bool, optional
        Whether to output the spin-up output and state as part of the output for debugging, by default False.

    Returns
    -------
    pd.DataFrame
        Dataframe containing the spin-up state for the model if `save_spinup` is False.
    tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]
        A tuple of dataframes containing the spin-up state, spin-up output, and per-land-cover spun-up state if `save_spinup` is True.
    """
    # this function is inspired by the spinup function in the supy-lcz project led by @matthiasdemuzere
    from ..data_model import SUEWSConfig  # noqa: PLC0415
    from ..suews_sim import SUEWSSimulation  # noqa: PLC0415

    if not isinstance(df_forcing.index, pd.DatetimeIndex):
        raise ValueError("Spin-up forcing must have a pandas.DatetimeIndex.")
    if df_forcing.empty:
        raise ValueError("Spin-up forcing data is empty.")
    if df_forcing.index.hasnans:
        raise ValueError("Spin-up forcing timestamps must not contain NaT.")

    # if df_forcing is shorter than one year, raise error
    len_forcing = df_forcing.index.max() - df_forcing.index.min()
    if len_forcing < pd.Timedelta(days=364):
        raise ValueError(
            "The forcing dataframe is shorter than one year, spin-up not possible!"
        )

    # if df_forcing is longer than one year but shorter than two years
    # then duplicate the forcing dataframe to cover the requested spin-up span
    if len_forcing < pd.Timedelta(days=365 * 2):
        freq = df_forcing.index.freq
        if freq is None:
            # infer_freq needs three timestamps; two define a single interval.
            freq = (
                infer_freq_offset(df_forcing.index)
                if len(df_forcing.index) >= 3
                else pd.tseries.frequencies.to_offset(
                    df_forcing.index[1] - df_forcing.index[0]
                )
            )
        if (
            freq is None
            or not df_forcing.index.is_monotonic_increasing
            or not df_forcing.index.is_unique
        ):
            raise ValueError(
                "Spin-up forcing requires a regular, increasing DatetimeIndex "
                "to duplicate the forcing data."
            )
        n_year = np.ceil(spinup_days / 365).astype(int)
        print(
            f"The forcing dataframe is longer than one year but shorter than two years, duplicating the forcing dataframe to {n_year} years"
        )
        df_forcing_spinup = pd.concat([df_forcing] * n_year, ignore_index=True)
        df_forcing_spinup.index = pd.date_range(
            df_forcing.index.max() - (df_forcing_spinup.index.size - 1) * freq,
            df_forcing.index.max(),
            freq=freq,
        )
    else:
        df_forcing_spinup = df_forcing

    # duplicate the state dataframe for later output
    df_state_spinup = df_state.copy()

    # Cycle over all fractions (ft = fractiontype), each time set to 100%
    # construct a new state, run the model, and store the state after spin-up
    # expand the state to seven rows, one for each fraction
    df_state_lc_init = pd.concat(
        [df_state.iloc[[0]]] * 7, ignore_index=True, names=["grid"]
    )
    # recover index name
    df_state_lc_init.index.rename("grid", inplace=True)

    # set the fractions to 100% for each fraction with a 7*7 matrix with 1 on the diagonal
    df_state_lc_init["sfr_surf"] = np.eye(7)

    # set porosity to 0.5, Issue #78
    df_state_lc_init["porosity_id"] = 0.5
    df_state_lc_init["pormax_dec"] = 0.9
    df_state_lc_init["pormin_dec"] = 0.1

    # construct forcing for spin-up
    end_spinup = pd.to_datetime(start_analysis)
    start_spinup = end_spinup - pd.Timedelta(days=spinup_days)

    print(f"==> starting spin-up simulations from {start_spinup} to {end_spinup}!")

    # Slice forcing
    df_forcing_spinup = df_forcing_spinup.loc[start_spinup:end_spinup]

    simulation = SUEWSSimulation(SUEWSConfig.from_df_state(df_state_lc_init))
    simulation.update_forcing(df_forcing_spinup)
    output = simulation.run()
    df_output_lc = output.df

    print("==> spin-up simulations completed, assigning spin-up states!")
    # Decode the genuinely evolved final state per pure-cover grid from the
    # Rust/Fortran checkpoint (``output.state_final`` is an un-evolved legacy
    # DFState copy kept only for schema compatibility).
    hydro_by_grid, pheno_by_grid = _decode_spinup_checkpoint(output.checkpoint)

    df_state_lc = output.state_final.copy()
    for grid_idx in range(len(_SURFACE_NAMES)):
        _apply_grid_evolved_state(
            df_state_lc,
            grid_idx,
            hydro_by_grid[grid_idx],
            pheno_by_grid[grid_idx],
        )

    _assign_spinup_state(df_state_spinup, hydro_by_grid, pheno_by_grid)

    if save_spinup:
        return df_state_spinup, df_output_lc, df_state_lc
    return df_state_spinup
