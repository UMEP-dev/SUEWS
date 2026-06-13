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
- ``{ProfCol: ":"}`` / ``{CodeCol: {...}}`` -- values reached through a second
  Code hop (24-hour profiles, OHM coefficients, BiogenCO2, soil). These are
  written via a de-aliasing pass: references sharing a secondary Code but
  holding distinct values get clone-on-write rows (fresh Codes, parents
  repointed), so one family's values never overwrite another's.

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


def _norm_code(token) -> str:
    """Canonical form of a Code token for row matching.

    Converted table sets are not consistent about integer formatting --
    SiteSelect may say ``661.0`` where the characteristic table row says
    ``661`` -- so Codes compare through this normalisation.
    """
    return _fmt(token)


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
        """Return (header, the SiteSelect data row) for ``grid``.

        The template provides *structure*, not identity: when the requested
        grid is absent (e.g. writing a config into another site's era
        template), the first data row is adopted and renumbered to ``grid``.
        """
        header, rows = self._tables["SUEWS_SiteSelect.txt"]
        gi = header.index("Grid")
        target = _norm_code(grid)
        for row in rows:
            if _norm_code(row[gi]) == target:
                return header, row
        if not rows:
            raise KeyError(f"SiteSelect template has no data rows for grid {grid}")
        logger_supy.warning(
            f"grid {grid} not in SiteSelect template; adopting the template's "
            f"first row (grid {rows[0][gi]}) and renumbering"
        )
        rows[0][gi] = _fmt(grid)
        return header, rows[0]

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
        target = _norm_code(code)
        for row in rows:
            if row and _norm_code(row[0]) == target:
                row[ci] = _fmt(value)
                return True
        return False

    def get_cell(self, fname: str, code: str, col: str):
        """Return the string token at (file, Code, column), or None."""
        table = self._tables.get(fname)
        if table is None:
            return None
        header, rows = table
        if col not in header:
            return None
        ci = header.index(col)
        target = _norm_code(code)
        for row in rows:
            if row and _norm_code(row[0]) == target:
                return row[ci]
        return None

    def clone_row(self, fname: str, code: str) -> str | None:
        """Append a copy of ``code``'s row under a fresh integer Code; return it.

        Used by the de-aliasing pass: when two references resolve to one
        secondary row but need different values, the second gets its own row.
        """
        table = self._tables.get(fname)
        if table is None:
            return None
        _, rows = table
        target = _norm_code(code)
        src = next((r for r in rows if r and _norm_code(r[0]) == target), None)
        if src is None:
            return None
        numeric = []
        for r in rows:
            if not r:
                continue
            try:
                numeric.append(int(float(r[0])))
            except ValueError:
                continue
        new_code = str((max(numeric) if numeric else 0) + 1)
        new_row = list(src)
        new_row[0] = new_code
        rows.append(new_row)
        return new_code

    def flush(self) -> None:
        for fname, (header, rows) in self._tables.items():
            _write_table(self._paths[fname], header, rows)


def _df_get(df_state: pd.DataFrame, grid, var: str, index: str):
    """Value of ``df_state[grid, (var, index)]`` or None if absent."""
    key = (var, index)
    if key not in df_state.columns:
        return None
    return df_state.loc[grid, key]


_SITESELECT = "__siteselect__"


def _write_split_remainder(store, fname, code, col, position, value) -> int:
    """Route a waterdist runoff/soil-store remainder to its legacy column.

    The waterdist final row holds a single combined remainder per source surface
    in df_state -- the loader sums the legacy ``ToRunoff`` and ``ToSoilStore``
    columns into it. Route it back to the structurally-correct column (impervious
    source surfaces, paved=0 and bldgs=1, drain to runoff; pervious surfaces 2..5
    to soil store) and zero the other so both legacy columns are fully determined
    by df_state rather than left at template values.
    """
    runoff_col = next((c for c in col if "Runoff" in c), None)
    soilstore_col = next((c for c in col if "SoilStore" in c), None)
    impervious = position in {0, 1}
    target = runoff_col if impervious else soilstore_col
    other = soilstore_col if impervious else runoff_col
    written = 0
    if target is not None:
        written += store.set_cell(fname, code, target, value)
    if other is not None:
        written += store.set_cell(fname, code, other, 0.0)
    return written


def _fill_from_df_state(store: _TableStore, df_state: pd.DataFrame, grid) -> int:
    """Overwrite every mapped input cell for ``grid`` from ``df_state``.

    Direct / surface-scalar / weekday-weekend values write straight into their
    rows (the primary characteristic Codes are distinct per surface). Values
    reached through a *second* Code hop -- 24-hour profiles, OHM coefficients,
    BiogenCO2, soil -- are collected as ``slots`` and resolved afterwards by
    :func:`_resolve_slots`, which de-aliases shared secondary Codes via
    clone-on-write so distinct values never overwrite each other.
    """
    ss_header, ss_row = store.site_row(grid)
    ss_index = {c: i for i, c in enumerate(ss_header)}
    written = 0
    # slot key: (sec_file, sec_code, parent_file, parent_code, parent_col)
    #           -> {col: value}  (accumulated across the vars sharing the row)
    slots: dict[tuple, dict] = {}

    def df(var, index):
        return _df_get(df_state, grid, var, index)

    def add_slot(sec_file, sec_code, parent_file, parent_code, parent_col, payload):
        if not payload:
            return
        key = (sec_file, sec_code, parent_file, parent_code, parent_col)
        slots.setdefault(key, {}).update(payload)

    for var, spec in _VAR2SITESELECT.items():
        transform = _TRANSFORMS.get(var, lambda v: v)

        if isinstance(spec, str):  # direct SiteSelect column
            value = df(var, "0")
            if value is not None:
                written += store.set_site(ss_header, ss_row, spec, transform(value))
            continue

        if isinstance(spec, list) and spec and isinstance(spec[0], dict):
            # list of {CodeCol: col} dicts: parameter-vector per surface
            # (e.g. storedrainprm); df index is (param, surface-position).
            for k, sub in enumerate(spec):
                for position, (code_col, col) in enumerate(sub.items()):
                    fname = _CODE2FILE.get(code_col)
                    code = ss_row[ss_index[code_col]] if code_col in ss_index else None
                    if fname is None or code is None:
                        continue
                    value = df(var, f"({k}, {position})")
                    if value is None:
                        continue
                    if isinstance(col, list):
                        # waterdist final row maps to two legacy columns
                        # (ToRunoff / ToSoilStore); route the single df remainder
                        # to the structurally-correct one and zero the other.
                        written += _write_split_remainder(
                            store, fname, code, col, position, transform(value)
                        )
                    else:
                        written += store.set_cell(fname, code, col, transform(value))
            continue

        if isinstance(spec, list):  # direct SiteSelect weekday/weekend pair
            for j, col in enumerate(spec):
                value = df(var, f"({j},)")
                if value is not None:
                    written += store.set_site(ss_header, ss_row, col, transform(value))
            continue

        if not isinstance(spec, dict):
            continue

        for position, (code_col, col_spec) in enumerate(spec.items()):
            if code_col == "const":  # a literal default, no df source
                continue
            fname = _CODE2FILE.get(code_col)
            code = ss_row[ss_index[code_col]] if code_col in ss_index else None
            if fname is None or code is None:
                continue

            if col_spec == ":":  # L1 profile: SiteSelect column -> Profiles.txt
                payload = {
                    str(h): transform(df(var, f"({h}, {position})"))
                    for h in range(24)
                    if df(var, f"({h}, {position})") is not None
                }
                add_slot(fname, code, _SITESELECT, str(grid), code_col, payload)

            elif isinstance(col_spec, str):  # scalar in a characteristic file
                value = df(var, f"({position},)")
                if value is None:
                    value = df(var, "0")
                if value is not None:
                    written += store.set_cell(fname, code, col_spec, transform(value))

            elif isinstance(col_spec, list):
                # 1-D: weekday/weekend pair, df index (j,). 2-D: parameter
                # vector per surface (e.g. laipower), df index (j, position).
                for j, col in enumerate(col_spec):
                    value = df(var, f"({j},)")
                    if value is None:
                        value = df(var, f"({j}, {position})")
                    if value is not None:
                        written += store.set_cell(fname, code, col, transform(value))

            elif isinstance(col_spec, dict):  # L2: second Code hop
                for q, (sec_col, target) in enumerate(col_spec.items()):
                    sec_file = _CODE2FILE.get(sec_col)
                    sec_code = store.get_cell(fname, code, sec_col)
                    if sec_file is None or sec_code is None:
                        continue
                    payload = {}
                    if target == ":":  # L2 profile -> Profiles.txt
                        for h in range(24):
                            value = df(var, f"({h}, {q})")
                            if value is not None:
                                payload[str(h)] = transform(value)
                    elif isinstance(target, list):  # e.g. OHM a1/a2/a3
                        for k, col in enumerate(target):
                            value = df(var, f"({position}, {q}, {k})")
                            if value is not None:
                                payload[col] = transform(value)
                    else:  # scalar in the secondary file
                        value = df(var, f"({position},)")
                        if value is None:
                            value = df(var, "0")
                        if value is not None:
                            payload[target] = transform(value)
                    add_slot(sec_file, sec_code, fname, code, sec_col, payload)

    written += _resolve_slots(store, slots, ss_row, ss_index)
    return written


def _resolve_slots(store: _TableStore, slots: dict, ss_row, ss_index) -> int:
    """Write the collected nested ``slots``, de-aliasing shared Codes.

    Slots resolving to one ``(file, Code)`` are grouped: the first distinct
    value-payload keeps the original Code; each further distinct payload gets a
    cloned row, and its parent reference column is repointed at the clone.
    Identical payloads keep aliasing one Code (no clone), which preserves
    ``legacy -> modern -> legacy`` faithfulness: there, references sharing a
    Code carry identical values by construction.
    """
    groups: dict[tuple, list] = {}
    for key, payload in slots.items():
        groups.setdefault((key[0], key[1]), []).append((key, payload))

    written = 0
    for (sec_file, sec_code), members in groups.items():
        assigned: dict[tuple, str] = {}  # payload signature -> Code carrying it
        for key, payload in members:
            sig = tuple(sorted(payload.items()))
            if sig in assigned:
                target = assigned[sig]
            else:
                target = (
                    sec_code if not assigned else store.clone_row(sec_file, sec_code)
                )
                if target is None:
                    continue
                for col, value in payload.items():
                    written += store.set_cell(sec_file, target, col, value)
                assigned[sig] = target
            if target != sec_code:
                _, _, parent_file, parent_code, parent_col = key
                if parent_file == _SITESELECT:
                    if parent_col in ss_index:
                        ss_row[ss_index[parent_col]] = _fmt(target)
                else:
                    store.set_cell(parent_file, parent_code, parent_col, target)
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

    total += _update_nml_from_df(out_dir, df_state)
    logger_supy.info(
        f"df_state_to_tables: wrote {total} input cells into {table_dir}"
    )
    return table_dir


# InitialConditions keys whose df var the name conventions cannot derive:
# legacy per-surface scalars map to indexed df slots, and a few use suffixes
# other than the trailing-0 convention.
_NML_KEY_MAP = {
    "porosity0": ("porosity_id", "0"),
    "decidcap0": ("decidcap_id", "0"),
    "albdectr0": ("albdectr_id", "0"),
    "albevetr0": ("albevetr_id", "0"),
    "albgrass0": ("albgrass_id", "0"),
    "laiinitialevetr": ("lai_id", "(0,)"),
    "laiinitialdectr": ("lai_id", "(1,)"),
    "laiinitialgrass": ("lai_id", "(2,)"),
    "soilstorepavedstate": ("soilstore_surf", "(0,)"),
    "soilstorebldgsstate": ("soilstore_surf", "(1,)"),
    "soilstoreevetrstate": ("soilstore_surf", "(2,)"),
    "soilstoredectrstate": ("soilstore_surf", "(3,)"),
    "soilstoregrassstate": ("soilstore_surf", "(4,)"),
    "soilstorebsoilstate": ("soilstore_surf", "(5,)"),
    "soilstorewaterstate": ("soilstore_surf", "(6,)"),
}

# Keys safe to ADD when an InitialConditions file omits them: declared in the
# era namelist since 2016a (verified against the 2016a-tag source), so every
# legacy reader accepts them. soilstorewaterstate is deliberately excluded --
# the 2016a declaration does not include it.
_NML_ADD_IF_MISSING = {
    "porosity0": ("porosity_id", "0"),
    "snowalb0": ("snowalb", "0"),
}


def _update_nml_from_df(out_dir: Path, df_state: pd.DataFrame) -> int:
    """Write ``df_state`` scalars into the RunControl / InitialConditions nml.

    The table fill covers ``SUEWS_*.txt``; method flags and initial states live
    in the namelists (``stabilitymethod``, ``snowalb``, ...). Keys match df
    vars by lowercase name, with a trailing ``0`` stripped for the
    InitialConditions convention (``snowalb0`` -> ``snowalb``). Non-scalar and
    string-valued keys (paths, codes) are left at their template values, as are
    keys with no df counterpart. The namelists are global, so multi-grid frames
    use the first grid (with a warning).
    """
    import f90nml

    grid = df_state.index[0]
    if len(df_state.index) > 1:
        logger_supy.warning(
            "df_state_to_tables: namelists are global; using grid "
            f"{grid} of {len(df_state.index)} for nml-backed values"
        )
    written = 0
    for path in out_dir.rglob("*.nml"):
        name = path.name.lower()
        if not name.startswith(("runcontrol", "initialconditions")):
            continue
        nml = f90nml.read(str(path))
        changed = False
        for section in nml.values():
            for key in list(section.keys()):
                current = section[key]
                if isinstance(current, (bool, str, list)):
                    continue  # flags-as-bools, paths, vectors: not df-backed
                candidates = []
                if key in _NML_KEY_MAP:
                    candidates.append(_NML_KEY_MAP[key])
                candidates.append((key, "0"))
                if key.endswith("0"):
                    candidates.append((key[:-1], "0"))
                for cand, index in candidates:
                    value = _df_get(df_state, grid, cand, index)
                    if value is None:
                        continue
                    if isinstance(current, int):
                        new = round(float(value))
                    else:
                        new = float(value)
                    if new != current:
                        section[key] = new
                        changed = True
                        written += 1
                    break
        # Add df-backed keys the file omits but every era's namelist declares
        # (a namelist read tolerates absent keys, so era files often skip
        # them; the declaration has carried these since 2016a). Keys absent
        # from older declarations (e.g. soilstorewaterstate) must NOT be
        # added -- an undeclared key aborts the era's namelist read.
        if name.startswith("initialconditions"):
            present = {k for section in nml.values() for k in section}
            for key, (var, index) in _NML_ADD_IF_MISSING.items():
                if key in present:
                    continue
                value = _df_get(df_state, grid, var, index)
                if value is None:
                    continue
                first_section = next(iter(nml.values()))
                first_section[key] = float(value)
                changed = True
                written += 1
        if changed:
            nml.write(str(path), force=True)
    return written
