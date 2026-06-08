"""Write a ``df_state`` back into a current-schema (``2025a``) table set.

Modern supy reads input tables (``load_InitialCond_grid_df``) but never writes
them -- it moved to YAML at runtime. This module supplies the missing exporter,
the ``df_state -> 2025a tables`` hop that closes the
``legacy tables <-> modern YAML`` round-trip: with the reverse table converter
(:mod:`supy.util.converter.table.reverse`) it turns an (optionally edited) YAML
config back into the original legacy tables.

Strategy. The 2025a table *structure* -- which files, columns, ``Code``
references, and the values of derived/transient columns that
``load_InitialCond_grid_df`` recomputes -- is taken from a template table set
(the forward-converted ``2025a`` tables, naturally available in a round-trip).
Every *input* value is then overwritten from ``df_state`` through the inverse of
``var2siteselect.json`` + ``code2file.json`` (the same maps the loader uses
forward). Reload-equivalence is the contract: ``load(write(df_state)) ==
df_state`` (verified to 0/1306 differing columns for the canonical KCL config).

Value-shape handling (mirrors the forward maps):

- ``"Col"`` -- a direct SiteSelect column.
- ``{CodeCol: "Col"}`` -- a scalar in the characteristic file ``CodeCol`` points
  at; the surface index is the entry's *position* in the dict (global 0-6 for a
  7-surface var like ``alb``; veg-local 0-2 for a veg-only var like ``laimax``).
- ``{CodeCol: ["Col_WD", "Col_WE"]}`` -- weekday/weekend pair (df indices 0/1).
- ``{CodeCol: {"Col": ":"}}`` -- a 24-hour profile; left at the template value
  (it round-trips faithfully) pending per-hour edit propagation.

A small ``_TRANSFORMS`` table inverts the unit conversions the loader applies
(currently ``surfacearea``: hectare <-> |m^2|).
"""

from __future__ import annotations

import json
from pathlib import Path
from shutil import copytree, rmtree

import pandas as pd

from ...._env import logger_supy, trv_supy_module
from .reverse import _read_table, _write_table

# Forward maps the loader uses; inverted here.
_VAR2SITESELECT = json.loads(
    (trv_supy_module / "var2siteselect.json").read_text(encoding="utf-8")
)
_CODE2FILE = json.loads(
    (trv_supy_module / "code2file.json").read_text(encoding="utf-8")
)

# Inverse of the unit conversions load_InitialCond_grid_df applies. The loader
# stores surface area in m^2 from a hectare-scaled table column.
_TRANSFORMS = {"surfacearea": lambda v: v / 10000.0}


def _fmt(value) -> str:
    """Format a numeric value as the shortest faithful on-disk token."""
    try:
        fv = float(value)
    except (TypeError, ValueError):
        return str(value)
    if fv == int(fv) and abs(fv) < 1e15:
        return str(int(fv))
    return repr(fv)


class _TableStore:
    """In-memory view of a table set, addressable by (file, Code, column)."""

    def __init__(self, input_dir: Path):
        self._tables: dict[str, tuple[list[str], list[list[str]]]] = {}
        self._paths: dict[str, Path] = {}
        for path in input_dir.glob("SUEWS_*.txt"):
            header, rows = _read_table(path)
            self._tables[path.name] = (header, rows)
            self._paths[path.name] = path

    def site_row(self, grid) -> tuple[list[str], list[str]]:
        """Return (header, the SiteSelect data row) for ``grid``."""
        header, rows = self._tables["SUEWS_SiteSelect.txt"]
        gi = header.index("Grid")
        for row in rows:
            if row[gi] == str(grid):
                return header, row
        raise KeyError(f"grid {grid} not in SiteSelect template")

    @staticmethod
    def set_site(header: list[str], row: list[str], col: str, value) -> bool:
        if col not in header:
            return False
        row[header.index(col)] = _fmt(value)
        return True

    def set_cell(self, fname: str, code: str, col: str, value) -> bool:
        table = self._tables.get(fname)
        if table is None:
            return False
        header, rows = table
        if col not in header:
            return False
        ci = header.index(col)
        for row in rows:
            if row[0] == str(code):
                row[ci] = _fmt(value)
                return True
        return False

    def flush(self) -> None:
        for fname, (header, rows) in self._tables.items():
            _write_table(self._paths[fname], header, rows)


def _df_get(df_state: pd.DataFrame, grid, var: str, index: str):
    """Value of ``df_state[grid, (var, index)]`` or None if absent."""
    key = (var, index)
    if key not in df_state.columns:
        return None
    return df_state.loc[grid, key]


def _fill_from_df_state(store: _TableStore, df_state: pd.DataFrame, grid) -> int:
    """Overwrite every mapped input cell for ``grid`` from ``df_state``."""
    ss_header, ss_row = store.site_row(grid)
    ss_index = {c: i for i, c in enumerate(ss_header)}
    written = 0

    for var, spec in _VAR2SITESELECT.items():
        transform = _TRANSFORMS.get(var, lambda v: v)

        if isinstance(spec, str):  # direct SiteSelect column
            value = _df_get(df_state, grid, var, "0")
            if value is not None:
                written += store.set_site(ss_header, ss_row, spec, transform(value))
            continue

        if not isinstance(spec, dict):
            continue

        for position, (code_col, col_spec) in enumerate(spec.items()):
            fname = _CODE2FILE.get(code_col)
            code = ss_row[ss_index[code_col]] if code_col in ss_index else None
            if fname is None or code is None:
                continue

            if isinstance(col_spec, str):  # scalar in a characteristic file
                value = _df_get(df_state, grid, var, f"({position},)")
                if value is None:
                    value = _df_get(df_state, grid, var, "0")
                if value is not None:
                    written += store.set_cell(fname, code, col_spec, transform(value))

            elif isinstance(col_spec, list):  # weekday / weekend pair
                for j, col in enumerate(col_spec):
                    value = _df_get(df_state, grid, var, f"({j},)")
                    if value is not None:
                        written += store.set_cell(fname, code, col, transform(value))

            # dict col_spec == 24-hour profile: left at the template value, which
            # round-trips faithfully; per-hour edit propagation is a follow-up.

    return written


def df_state_to_tables(df_state: pd.DataFrame, template_dir, out_dir) -> Path:
    """Regenerate a ``2025a`` table set from ``df_state``.

    Parameters
    ----------
    df_state : pandas.DataFrame
        Current-schema state (one row per grid), e.g. ``config.to_df_state()``.
    template_dir : str or Path
        A ``2025a`` table set providing structure (files, columns, ``Code``
        references, derived/profile values). In a round-trip this is the
        forward-converted output.
    out_dir : str or Path
        Directory to write the regenerated table set into.

    Returns
    -------
    Path
        The directory holding the SUEWS ``*.txt`` / ``*.nml`` table set.
    """
    template_dir, out_dir = Path(template_dir), Path(out_dir)
    rmtree(out_dir, ignore_errors=True)

    # Copy the whole template tree so RunControl paths, grid-layout / SPARTACUS /
    # InitialConditions namelists, and the input-dir name are all preserved.
    copytree(template_dir, out_dir)
    table_dir = next(
        d
        for d in [out_dir, *out_dir.rglob("*")]
        if d.is_dir() and (d / "SUEWS_SiteSelect.txt").exists()
    )

    store = _TableStore(table_dir)
    total = 0
    for grid in df_state.index:
        total += _fill_from_df_state(store, df_state, grid)
    store.flush()
    logger_supy.info(
        f"df_state_to_tables: wrote {total} input cells into {table_dir}"
    )
    return table_dir
