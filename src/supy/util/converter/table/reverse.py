"""Reverse table conversion: regenerate legacy SUEWS tables from current ones.

The forward path (``table.py::convert_table``) walks ``rules.csv`` edges from a
legacy version up to ``2025a``, applying, per step, Add / Delete / Rename /
Rename_File actions plus a couple of file-creation special cases. This module
inverts that walk so a current-schema (``2025a``) table set can regenerate the
*original* legacy tables faithfully.

Invertibility (validated against the real ``2018b`` canonical-KCL tables):

- ``Add`` (forward) inverts to dropping the column -- the legacy table never
  had it.
- ``Rename`` (forward ``old -> new``; ``rules.csv`` stores ``Variable`` = old,
  ``Value`` = new) inverts to ``new -> old``.
- ``Rename_File`` (``File`` = old, ``Value`` = new) inverts symmetrically.
- ``Delete`` (forward) is the *only* information-losing action: the legacy
  table carried a column the current schema drops, and the rule does not record
  the value (often ``col=-999``). The original value(s) and position must be
  captured up front -- the ``legacy_extras`` block from
  :func:`capture_legacy_extras`.
- The ``2023a -> 2024a`` step *creates* ``SUEWS_SPARTACUS.nml`` and
  ``GridLayoutKc.nml``; the reverse deletes them below ``2024a``.

Only deletes of variables that *actually exist in the source* are real losses:
some variables are added then deleted within one ``from_ver -> 2025a`` walk
(net-zero) and must be neither captured nor restored. For ``2018b`` the captured
set is a single column, ``FcEF_v_kgkm`` in ``SUEWS_AnthropogenicHeat.txt``;
``DiagQS`` / ``DiagQN`` (RunControl keys not present in ``2018b``) are skipped.
"""

from __future__ import annotations

import math
from pathlib import Path
from shutil import copyfile
from typing import Optional

import f90nml

from ...._env import logger_supy
from ...._load import LEGACY_RUNCONTROL_KEY_ALIASES
from ...._misc import path_insensitive
from .legacy import LegacyTable, read_legacy_table, write_legacy_table
from .table import rules

TARGET_VER = "2025a"

# Files the 2023a -> 2024a step creates from nothing; deleted on the way down.
_CREATED_AT_2024A = ("SUEWS_SPARTACUS.nml", "GridLayoutKc.nml")


# --------------------------------------------------------------------------- #
# Version path
# --------------------------------------------------------------------------- #
def forward_path(from_ver: str, to_ver: str = TARGET_VER) -> list[str]:
    """Ordered version chain ``from_ver -> ... -> to_ver`` along rules edges.

    The conversion graph is linear in practice (each version has a single
    successor), so a straight successor walk reproduces the path Dijkstra would
    pick, without the cost-tuple bookkeeping of ``table.version_list``.

    Parameters
    ----------
    from_ver : str
        Source legacy version tag (e.g. ``"2018b"``).
    to_ver : str, optional
        Target version, by default ``"2025a"``.

    Returns
    -------
    list of str
        The ordered version chain, inclusive of both ends.
    """
    succ = dict(zip(rules["From"].tolist(), rules["To"].tolist()))
    path = [from_ver]
    cur = from_ver
    seen = {from_ver}
    while cur != to_ver and cur in succ:
        cur = succ[cur]
        if cur in seen:  # guard against a malformed cyclic rules table
            break
        path.append(cur)
        seen.add(cur)
    if path[-1] != to_ver:
        raise ValueError(
            f"no forward rules path from {from_ver!r} to {to_ver!r}; "
            f"reached {path[-1]!r}"
        )
    return path


def _step_rules(from_ver: str, to_ver: str):
    """All rules for a single forward edge ``from_ver -> to_ver``."""
    return rules[(rules["From"] == from_ver) & (rules["To"] == to_ver)]


# --------------------------------------------------------------------------- #
# Tolerant table / namelist IO
# --------------------------------------------------------------------------- #
def _read_table(path: Path) -> tuple[list[str], list[list[str]]]:
    """Parse a SUEWS ``*.txt`` table into ``(header, rows)``.

    Layout: line 1 = column indices, line 2 = header names, then whitespace-
    delimited data rows terminated by a ``-9`` sentinel line. The first column
    is always the row ``Code``. Values are kept as their exact on-disk tokens.

    Parameters
    ----------
    path : Path
        Table file to read.

    Returns
    -------
    tuple
        ``(header, rows)`` where ``header`` is the list of column names and
        ``rows`` is a list of token lists (one per data row).
    """
    table = read_legacy_table(path)
    return table.headers, table.rows


def _write_table(path: Path, header: list[str], rows: list[list[str]]) -> None:
    """Write ``(header, rows)`` back in the two-line-header SUEWS table format.

    Parameters
    ----------
    path : Path
        Destination file.
    header : list of str
        Column names.
    rows : list of list of str
        Data rows as token lists.
    """
    write_legacy_table(path, LegacyTable(headers=list(header), rows=list(rows)))


# --------------------------------------------------------------------------- #
# Capture (legacy_extras)
# --------------------------------------------------------------------------- #
def _path_rules(from_ver: str, to_ver: str = TARGET_VER):
    """All rules fired along the forward ``from_ver -> to_ver`` walk."""
    path_from = set(forward_path(from_ver, to_ver)[:-1])
    return rules[rules["From"].isin(path_from)]


def _deletes_on_path(from_ver: str, to_ver: str = TARGET_VER):
    """Delete rules fired along the forward ``from_ver -> to_ver`` walk."""
    on_path = _path_rules(from_ver, to_ver)
    return on_path[on_path["Action"] == "Delete"]


def _touched_tables(from_ver: str, to_ver: str = TARGET_VER) -> set:
    """SUEWS ``*.txt`` tables whose columns the forward walk Add/Delete/Renames.

    Column ops reference the file by its *source-side* name (they run before
    the same step's Rename_File), so these names match the original table set.
    Used to scope the final column-order reconciliation to tables that can
    actually drift, leaving untouched tables (and any with comment lines) alone.
    """
    on_path = _path_rules(from_ver, to_ver)
    col_ops = on_path[on_path["Action"].isin(["Add", "Delete", "Rename"])]
    return {f for f in col_ops["File"].astype(str) if f.endswith(".txt")}


def _read_nml_scalar(path: Path, var: str):
    """Return the scalar value of ``var`` in a namelist, or None if absent."""
    nml = f90nml.read(str(path))
    var_l = var.lower()
    for section in nml.values():
        if var_l in section:
            return section[var_l]
    return None


def capture_legacy_extras(src_dir, from_ver: str) -> dict:
    """Capture source-existing values that the forward conversion would delete.

    Walks every ``Delete`` rule on the ``from_ver -> 2025a`` path and records,
    for each deleted variable that is *actually present* in the source table
    set, its original value(s) and column position. Variables absent from the
    source (added and then deleted within the chain) are skipped -- they are not
    real losses.

    Parameters
    ----------
    src_dir : str or Path
        Directory holding the original legacy table set (``RunControl.nml``,
        ``SUEWS_*.txt``, namelists).
    from_ver : str
        The source legacy version tag (e.g. ``"2018b"``).

    Returns
    -------
    dict
        ``{"from_ver": from_ver, "deleted": {file: {var: spec}}}`` where, for a
        table column, ``spec`` is ``{"col": 1-based-index, "values": {Code:
        token}}`` and, for a namelist key, ``spec`` is ``{"scalar": value}``.
        Files / variables absent from the source are omitted.
    """
    src_dir = Path(src_dir)
    deleted: dict[str, dict] = {}

    for _, rule in _deletes_on_path(from_ver).iterrows():
        fname = str(rule["File"])
        var = str(rule["Variable"])
        path = src_dir / fname
        if not path.exists():
            logger_supy.debug(
                f"capture_legacy_extras: {fname} absent in source, skip {var}"
            )
            continue

        if fname.endswith(".nml"):
            value = _read_nml_scalar(path, var)
            spec = None if value is None else {"scalar": value}
        else:
            header, rows = _read_table(path)
            if var not in header:
                spec = None
            else:
                idx = header.index(var)
                spec = {
                    "col": idx + 1,
                    "values": {r[0]: r[idx] for r in rows if len(r) > idx},
                }

        if spec is None:
            logger_supy.debug(
                f"capture_legacy_extras: {var} not in source {fname}, skip"
            )
            continue

        deleted.setdefault(fname, {})[var] = spec
        logger_supy.info(f"capture_legacy_extras: captured {fname}::{var}")

    # Capture the source column order of every table the walk perturbs, so the
    # reverse can reconcile column order exactly (mid-chain restores cannot rely
    # on absolute indices once intervening Adds/Deletes shift the layout).
    headers: dict[str, list[str]] = {}
    for fname in _touched_tables(from_ver):
        path = src_dir / fname
        if path.exists():
            header, _ = _read_table(path)
            if header:
                headers[fname] = header

    # Capture the original SUEWS_Profiles.txt code set and the single exact
    # synthetic day/night default code (if any) that forward conversion will
    # allocate, so reverse conversion prunes only that converter-owned row and
    # never deletes user-owned or user-added profile rows.
    from .profile_manager import ProfileManager  # noqa: PLC0415

    day_night_base = int(ProfileManager.DEFAULT_PROFILES["DAY_NIGHT_DEFAULT"])
    day_night_sentinel = str(day_night_base)
    path_rules = _path_rules(from_ver)
    walk_adds_day_night = bool(
        (
            (path_rules["Action"] == "Add")
            & (path_rules["Value"].astype(str).str.strip() == day_night_sentinel)
        ).any()
    )
    profiles_path = src_dir / "SUEWS_Profiles.txt"
    profile_codes: Optional[list[str]] = None
    synthetic_day_night_code: Optional[int] = None
    if profiles_path.exists():
        _, prof_rows = _read_table(profiles_path)
        profile_codes = [r[0] for r in prof_rows if r]
        if walk_adds_day_night:
            source_codes_int = {
                parsed
                for r in prof_rows
                if r and (parsed := _parse_profile_code_int(r[0])) is not None
            }
            existing_preferred = next(
                (
                    r
                    for r in prof_rows
                    if r and _parse_profile_code_int(r[0]) == day_night_base
                ),
                None,
            )
            if existing_preferred is None:
                synthetic_day_night_code = day_night_base
            elif not _is_uniform_one_profile_row(existing_preferred):
                candidate = day_night_base + 1
                while candidate in source_codes_int:
                    candidate += 1
                synthetic_day_night_code = candidate

    # Capture pre-#864 RunControl aliases migrated or dropped at 2024a -> 2025a
    # so reverse conversion restores original spellings and any displaced values.
    runcontrol_aliases: dict[str, dict] = {}
    path_from = set(forward_path(from_ver, TARGET_VER)[:-1])
    if "2024a" in path_from:
        rc_path = Path(path_insensitive(src_dir / "RunControl.nml"))
        if rc_path.is_file():
            rc_nml = f90nml.read(str(rc_path))
            if rc_nml:
                rc_sec = (
                    rc_nml["runcontrol"]
                    if "runcontrol" in rc_nml
                    else next(iter(rc_nml.values()))
                )
                adds_canonical = "2023a" in path_from
                for legacy_key, canonical_key in LEGACY_RUNCONTROL_KEY_ALIASES:
                    if legacy_key in rc_sec:
                        if canonical_key in rc_sec or adds_canonical:
                            runcontrol_aliases[legacy_key] = {
                                "canonical": canonical_key,
                                "mode": "restore",
                                "scalar": rc_sec[legacy_key],
                            }
                        else:
                            runcontrol_aliases[legacy_key] = {
                                "canonical": canonical_key,
                                "mode": "rename",
                            }

    extras_out: dict = {
        "from_ver": from_ver,
        "deleted": deleted,
        "headers": headers,
    }
    if profile_codes is not None:
        extras_out["profile_codes"] = profile_codes
    if synthetic_day_night_code is not None:
        extras_out["synthetic_day_night_code"] = synthetic_day_night_code
    if runcontrol_aliases:
        extras_out["runcontrol_aliases"] = runcontrol_aliases
    return extras_out


# --------------------------------------------------------------------------- #
# Reverse primitives (table)
# --------------------------------------------------------------------------- #
def _drop_table_column(path: Path, var: str) -> None:
    """Remove column ``var`` from a table (undo of a forward Add)."""
    header, rows = _read_table(path)
    if var not in header:
        return
    idx = header.index(var)
    header.pop(idx)
    for r in rows:
        if len(r) > idx:
            r.pop(idx)
    _write_table(path, header, rows)


def _rename_table_column(path: Path, old: str, new: str) -> None:
    """Rename column ``old`` -> ``new`` in a table (undo of a forward Rename)."""
    header, rows = _read_table(path)
    if old not in header:
        return
    header[header.index(old)] = new
    _write_table(path, header, rows)


def _restore_table_column(
    path: Path, var: str, col: int, values: dict, default: str = "-999"
) -> None:
    """Re-insert a forward-deleted column at ``col`` with per-Code values."""
    header, rows = _read_table(path)
    if var in header:
        return
    idx = min(max(col - 1, 0), len(header))
    header.insert(idx, var)
    for r in rows:
        code = r[0] if r else None
        r.insert(idx, values.get(code, default))
    _write_table(path, header, rows)


def _reorder_table_columns(path: Path, target_header: list[str]) -> None:
    """Reorder a table's columns to ``target_header`` (the source order).

    The reverse engine reconstructs the correct column *set* and values; this
    pins the *order*, which mid-chain index arithmetic cannot guarantee once
    intervening Adds/Deletes shift the layout. Skipped (with a warning) if the
    column sets do not match, so a genuine reconstruction gap stays visible.
    """
    header, rows = _read_table(path)
    if header == target_header:
        return
    if set(header) != set(target_header):
        logger_supy.warning(
            f"_reorder_table_columns: column-set mismatch in {path.name}; "
            f"missing={set(target_header) - set(header)} "
            f"extra={set(header) - set(target_header)} -- leaving as-is"
        )
        return
    order = [header.index(c) for c in target_header]
    new_rows = [[r[i] for i in order if i < len(r)] for r in rows]
    _write_table(path, target_header, new_rows)


# --------------------------------------------------------------------------- #
# Reverse primitives (namelist)
# --------------------------------------------------------------------------- #
def _drop_nml_key(path: Path, var: str) -> None:
    """Remove key ``var`` from a namelist (undo of a forward Add)."""
    if not path.exists():
        return
    nml = f90nml.read(str(path))
    var_l = var.lower()
    changed = False
    for section in nml.values():
        if var_l in section:
            del section[var_l]
            changed = True
    if changed:
        nml.write(str(path), force=True)


def _rename_nml_key(path: Path, old: str, new: str) -> None:
    """Rename key ``old`` -> ``new`` in a namelist (undo of a forward Rename)."""
    if not path.exists():
        return
    nml = f90nml.read(str(path))
    old_l, new_l = old.lower(), new.lower()
    changed = False
    for section in nml.values():
        if old_l in section:
            section[new_l] = section.pop(old_l)
            changed = True
    if changed:
        nml.write(str(path), force=True)


def _restore_nml_key(path: Path, var: str, scalar) -> None:
    """Re-insert a forward-deleted namelist key (undo of a forward Delete)."""
    if not path.exists():
        return
    nml = f90nml.read(str(path))
    title = next(iter(nml.keys()))
    nml[title][var.lower()] = scalar
    nml.write(str(path), force=True)


# --------------------------------------------------------------------------- #
# Reverse engine
# --------------------------------------------------------------------------- #
def _rename_file(d: Path, old: str, new: str) -> None:
    """Rename ``new`` back to ``old`` on disk (undo of a forward Rename_File)."""
    src, dst = d / new, d / old
    if src.exists():
        src.rename(dst)


# When a variable moves between SiteSelect and a characteristic table, the
# per-row value mapping follows this SiteSelect code column.
_MOVE_LINK_COLS = {
    "SUEWS_AnthropogenicHeat.txt": "AnthropogenicCode",
    "SUEWS_AnthropogenicEmission.txt": "AnthropogenicCode",
}


def _nml_scalar(token: str):
    """Coerce a table token to the scalar type a namelist write expects."""
    try:
        f = float(token)
        return int(f) if f == int(f) else f
    except (TypeError, ValueError):
        return token


def _harvest_moves(d: Path, step) -> dict:
    """Capture live values for same-step cross-file moves before they drop.

    A forward *move* is encoded in ``rules.csv`` as ``Add(file_new, var)`` +
    ``Delete(file_old, var)`` within one version step (e.g. the anthropogenic
    profile codes, SiteSelect -> AnthropogenicHeat at 2017a->2018a; ``z``,
    RunControl.nml -> SiteSelect at 2016a->2017a). Reversing the step drops
    ``file_new``'s column (Add-undo) and restores ``file_old``'s (Delete-undo);
    this harvests ``file_new``'s values first, mapped into ``file_old``'s row
    space, so the restore reflects the current table state rather than the
    ``legacy_extras`` snapshot.

    Returns ``{(file_old, var): {"values": {rowkey: token}}}`` for tables or
    ``{(file_old, var): {"scalar": value}}`` for namelist targets.
    """
    adds: dict[str, str] = {}
    for _, r in step[step["Action"] == "Add"].iterrows():
        adds[str(r["Variable"])] = str(r["File"])

    out: dict = {}
    for _, r in step[step["Action"] == "Delete"].iterrows():
        var, file_old = str(r["Variable"]), str(r["File"])
        file_new = adds.get(var)
        if file_new is None or file_new == file_old:
            continue
        src = d / file_new
        if not src.exists():
            continue

        if file_old.endswith(".nml"):
            # Table -> nml reversed: collapse the per-row values to a scalar.
            header, rows = _read_table(src)
            if var not in header or not rows:
                continue
            vi = header.index(var)
            tokens = {row[vi] for row in rows if len(row) > vi}
            if len(tokens) > 1:
                logger_supy.warning(
                    f"move-carry {var}: heterogeneous per-row values {tokens}; "
                    f"the {file_old} scalar takes the first row's value"
                )
            out[file_old, var] = {"scalar": _nml_scalar(rows[0][vi])}

        elif file_old == "SUEWS_SiteSelect.txt":
            # Characteristic table -> SiteSelect: follow the linking Code.
            link = _MOVE_LINK_COLS.get(file_new)
            tgt = d / file_old
            if link is None or not tgt.exists():
                if link is None:
                    logger_supy.warning(
                        f"move-carry {var}: no link column registered for "
                        f"{file_new}; falling back to the extras snapshot"
                    )
                continue
            sh, srows = _read_table(src)
            th, trows = _read_table(tgt)
            if var not in sh or link not in th:
                continue
            vi, li = sh.index(var), th.index(link)
            by_code = {row[0]: row[vi] for row in srows if len(row) > vi}
            values = {
                row[0]: by_code[row[li]]
                for row in trows
                if len(row) > li and row[li] in by_code
            }
            if values:
                out[file_old, var] = {"values": values}

        else:
            # Same-keyed tables: carry per first-column key directly.
            sh, srows = _read_table(src)
            if var not in sh:
                continue
            vi = sh.index(var)
            out[file_old, var] = {
                "values": {row[0]: row[vi] for row in srows if len(row) > vi}
            }

    # Renamed moves and one-to-many splits from the shared registry: restore
    # the source from the destination columns' consensus (first value, warning
    # when they disagree -- the old schema cannot represent the difference).
    from .table import _CARRY_REGISTRY

    step_key = None
    if len(step):
        step_key = (str(step.iloc[0]["From"]), str(step.iloc[0]["To"]))
    for entry in _CARRY_REGISTRY.get(step_key, []):
        sfile, svar = entry["src"]
        per_key: dict[str, list] = {}
        for dfile, dvar in entry["dst"]:
            path = d / dfile
            if not path.exists():
                continue
            header, rows = _read_table(path)
            if dvar not in header:
                continue
            vi = header.index(dvar)
            for row in rows:
                if len(row) > vi:
                    per_key.setdefault(row[0], []).append(row[vi])
        if not per_key:
            continue
        if sfile.endswith(".nml"):
            tokens = [v for vals in per_key.values() for v in vals]
            if len(set(tokens)) > 1:
                logger_supy.warning(
                    f"carry {svar}: destination values disagree {sorted(set(tokens))};"
                    f" the {sfile} scalar takes the first"
                )
            out[sfile, svar] = {"scalar": _nml_scalar(tokens[0])}
        else:
            values = {}
            for key, vals in per_key.items():
                if len(set(vals)) > 1:
                    logger_supy.warning(
                        f"carry {svar}: row {key} destinations disagree "
                        f"{sorted(set(vals))}; keeping {vals[0]}"
                    )
                values[key] = vals[0]
            out[sfile, svar] = {"values": values}
    return out


def _parse_profile_code_int(token: object) -> Optional[int]:
    """Parse an integer or whole-number decimal profile code token."""
    text = str(token).strip()
    if not text:
        return None
    try:
        return int(text)
    except ValueError:
        try:
            fval = float(text)
        except ValueError:
            return None
        if math.isfinite(fval) and fval.is_integer():
            return int(fval)
        return None


def _is_uniform_one_profile_row(row: list[str]) -> bool:
    """Return True iff *row* carries 24 finite hourly values all equal to 1.0."""
    if len(row) < 25:
        return False
    for token in row[1:25]:
        try:
            fval = float(token)
        except ValueError:
            return False
        if not math.isfinite(fval) or fval != 1.0:
            return False
    return True


def _reverse_step(d: Path, from_ver: str, to_ver: str, extras: dict) -> None:
    """Undo one forward edge ``from_ver -> to_ver`` in directory ``d``.

    On entry ``d`` holds the ``to_ver``-schema tables; on exit it holds the
    ``from_ver``-schema tables. Actions are undone in the reverse of the forward
    application order (Rename, Delete, Add, Rename_File): Rename_File first, then
    Add (drop), then Delete (restore), then Rename.

    Parameters
    ----------
    d : Path
        Working directory of tables.
    from_ver, to_ver : str
        The forward edge being inverted.
    extras : dict
        ``legacy_extras["deleted"]`` mapping (file -> var -> spec).
    """
    step = _step_rules(from_ver, to_ver)
    deleted = extras.get("deleted", {})

    # 1. Undo file creation special-cases.
    if (from_ver, to_ver) == ("2023a", "2024a"):
        for fname in _CREATED_AT_2024A:
            (d / fname).unlink(missing_ok=True)

    # 1b. Undo historical RunControl alias migration (2024a -> 2025a).
    if (from_ver, to_ver) == ("2024a", "2025a"):
        rc_aliases = extras.get("runcontrol_aliases", {})
        if rc_aliases:
            rc_path = Path(path_insensitive(d / "RunControl.nml"))
            if rc_path.is_file():
                for legacy_key, spec in rc_aliases.items():
                    if spec.get("mode") == "rename":
                        _rename_nml_key(rc_path, spec["canonical"], legacy_key)
                    elif spec.get("mode") == "restore":
                        _restore_nml_key(rc_path, legacy_key, spec["scalar"])

    # 2. Undo Rename_File first, so column ops below see the source-side name.
    for _, r in step[step["Action"] == "Rename_File"].iterrows():
        _rename_file(d, old=str(r["File"]), new=str(r["Value"]))

    # 2b. Harvest cross-file moves (same-step Delete in one file + Add in
    # another for the same variable) BEFORE the Add-undo drops the modern
    # column. The carried values take precedence over ``legacy_extras`` in the
    # Delete-undo, so the regenerated legacy table reflects the current state,
    # not the stale source snapshot. Without this, e.g. the anthropogenic
    # profile codes (SiteSelect -> AnthropogenicHeat at 2017a->2018a) reset to
    # whatever the extras donor carried.
    carried = _harvest_moves(d, step)

    from .profile_manager import ProfileManager  # noqa: PLC0415

    day_night_base = int(ProfileManager.DEFAULT_PROFILES["DAY_NIGHT_DEFAULT"])
    day_night_sentinel = str(day_night_base)
    dropped_day_night_codes: set[int] = set()

    # 3. Group the column/key actions by their (source-side) file name.
    col_actions = step[step["Action"].isin(["Add", "Delete", "Rename"])]
    for fname in sorted(set(col_actions["File"].astype(str))):
        path = d / fname
        is_nml = fname.endswith(".nml")
        grp = col_actions[col_actions["File"].astype(str) == fname]

        # 3a. Undo Adds (drop the columns/keys the forward step introduced).
        for _, r in grp[grp["Action"] == "Add"].iterrows():
            var = str(r["Variable"])
            if is_nml:
                _drop_nml_key(path, var)
            elif path.exists():
                if str(r["Value"]).strip() == day_night_sentinel:
                    tbl_header, tbl_rows = _read_table(path)
                    if var in tbl_header:
                        col_idx = tbl_header.index(var)
                        for tbl_row in tbl_rows:
                            if col_idx < len(tbl_row):
                                parsed = _parse_profile_code_int(tbl_row[col_idx])
                                if parsed is not None and parsed >= day_night_base:
                                    dropped_day_night_codes.add(parsed)
                _drop_table_column(path, var)

        # 3b. Undo Deletes (restore source-present columns/keys; carried
        # move values take precedence over the extras snapshot).
        for _, r in grp[grp["Action"] == "Delete"].iterrows():
            var = str(r["Variable"])
            spec = deleted.get(fname, {}).get(var)
            carry = carried.get((fname, var))
            if spec is None and carry is None:
                continue  # not in source (add-then-delete transient): skip
            if is_nml:
                scalar = carry["scalar"] if carry else spec["scalar"]
                _restore_nml_key(path, var, scalar)
            elif path.exists():
                values = dict(spec["values"]) if spec else {}
                if carry:
                    values.update(carry["values"])
                col = spec["col"] if spec else len(_read_table(path)[0]) + 1
                _restore_table_column(path, var, col, values)

        # 3c. Undo Renames (forward old->new becomes new->old).
        for _, r in grp[grp["Action"] == "Rename"].iterrows():
            old, new = str(r["Variable"]), str(r["Value"])
            if is_nml:
                _rename_nml_key(path, new, old)
            elif path.exists():
                _rename_table_column(path, new, old)

    # 4. Prune the single unchanged synthetic uniform-1.0 day/night default
    # profile row allocated by forward conversion (``synthetic_day_night_code``),
    # ONLY when it was referenced by the day/night Add columns dropped in
    # step 3a, absent from source ``profile_codes``, and unreferenced in any
    # remaining table. User-owned rows, user-added rows (even uniform 1.0 and
    # referenced by a dropped column), and modified rows are never removed.
    profiles_path = d / "SUEWS_Profiles.txt"
    source_profile_codes = extras.get("profile_codes")
    raw_synthetic_code = extras.get("synthetic_day_night_code")
    synthetic_code = (
        _parse_profile_code_int(raw_synthetic_code)
        if raw_synthetic_code is not None
        else None
    )
    if (
        synthetic_code is not None
        and synthetic_code >= day_night_base
        and synthetic_code in dropped_day_night_codes
        and profiles_path.exists()
        and source_profile_codes is not None
    ):
        source_codes_int = {
            parsed
            for code in source_profile_codes
            if (parsed := _parse_profile_code_int(code)) is not None
        }
        referenced_codes_int: set[int] = set()
        for tbl in d.glob("SUEWS_*.txt"):
            if tbl.name == "SUEWS_Profiles.txt":
                continue
            _, tbl_rows = _read_table(tbl)
            for row in tbl_rows:
                for token in row[1:]:
                    parsed = _parse_profile_code_int(token)
                    if parsed is not None:
                        referenced_codes_int.add(parsed)
        prof_header, prof_rows = _read_table(profiles_path)
        pruned: list[list[str]] = []
        for row in prof_rows:
            if not row:
                continue
            code_int = _parse_profile_code_int(row[0])
            if (
                code_int == synthetic_code
                and code_int not in source_codes_int
                and code_int not in referenced_codes_int
                and _is_uniform_one_profile_row(row)
            ):
                continue
            pruned.append(row)
        if len(pruned) != len(prof_rows):
            _write_table(profiles_path, prof_header, pruned)


def reverse_convert_table(
    src_dir,
    out_dir,
    to_ver: str,
    legacy_extras: dict,
    from_ver: Optional[str] = None,
    legacy_terminators: bool = True,
) -> None:
    """Regenerate ``to_ver`` legacy tables from a ``2025a`` table set.

    Inverts the forward ``convert_table`` walk: copies the current-schema tables
    into ``out_dir`` and replays each forward edge backwards, restoring
    forward-deleted columns from ``legacy_extras``.

    Parameters
    ----------
    src_dir : str or Path
        Directory holding the current-schema (``2025a``) table set.
    out_dir : str or Path
        Directory to write the regenerated ``to_ver`` tables into.
    to_ver : str
        Target legacy version tag (e.g. ``"2018b"``).
    legacy_extras : dict
        The block from :func:`capture_legacy_extras` for the same ``to_ver``.
    from_ver : str, optional
        Schema version of ``src_dir``; defaults to ``"2025a"``.
    legacy_terminators : bool, optional
        Append the two ``-9`` end-of-data sentinel rows the legacy Fortran
        reader requires (``clean_legacy_table`` strips them for the modern
        form). Default True so the output is execution-ready; the round-trip
        data comparison is unaffected because the table reader stops at the
        first ``-9`` row. Verified: the 2016a binary runs the resulting tables
        end to end.
    """
    src_dir, out_dir = Path(src_dir), Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    from_ver = from_ver or TARGET_VER

    # Copy the current-schema table set into the working directory.
    for pattern in ("SUEWS_*.txt", "*.nml"):
        for f in src_dir.glob(pattern):
            copyfile(f, out_dir / f.name)

    # Replay each forward edge in reverse.
    path = forward_path(to_ver, from_ver)
    for a, b in reversed(list(zip(path[:-1], path[1:]))):
        logger_supy.info(f"reverse_convert_table: undoing {a} -> {b}")
        _reverse_step(out_dir, a, b, legacy_extras)

    # Reconcile column order against the captured source headers.
    for fname, target_header in legacy_extras.get("headers", {}).items():
        path_table = out_dir / fname
        if path_table.exists():
            _reorder_table_columns(path_table, target_header)

    # Append the legacy end-of-data sentinel rows the Fortran reader needs.
    if legacy_terminators:
        for path_table in out_dir.glob("SUEWS_*.txt"):
            text = path_table.read_text(encoding="utf-8")
            if not text.endswith("\n"):
                text += "\n"
            path_table.write_text(text + "-9\n-9\n", encoding="utf-8")
