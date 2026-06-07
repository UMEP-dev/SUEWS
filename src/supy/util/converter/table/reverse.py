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

from pathlib import Path
from shutil import copyfile

from chardet import detect
import f90nml

from ...._env import logger_supy
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
def _decode_text(path: Path) -> str:
    """Read a legacy table as text, tolerating non-UTF-8 source encodings.

    Legacy SUEWS tables predate the repo's UTF-8 mandate and carry stray
    Latin-1 bytes (e.g. an accented author name). The forward converter
    normalises these with ``convert_utf8`` before reading; capture reads the raw
    source, so it detects the encoding (chardet, Latin-1 fallback) rather than
    assuming UTF-8.

    Parameters
    ----------
    path : Path
        File to read.

    Returns
    -------
    str
        Decoded text.
    """
    raw = path.read_bytes()
    try:
        return raw.decode("utf-8")
    except UnicodeDecodeError:
        enc = (detect(raw) or {}).get("encoding") or "latin-1"
        return raw.decode(enc, errors="replace")


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
    lines = _decode_text(path).splitlines()
    if len(lines) < 2:
        return [], []
    header = lines[1].split()
    rows: list[list[str]] = []
    for line in lines[2:]:
        toks = line.split()
        if not toks or toks[0].startswith("-9"):
            break
        rows.append(toks)
    return header, rows


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
    index_line = " ".join(str(i + 1) for i in range(len(header)))
    body = [index_line, " ".join(header)]
    body.extend(" ".join(r) for r in rows)
    path.write_text("\n".join(body) + "\n", encoding="utf-8")


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

    return {"from_ver": from_ver, "deleted": deleted, "headers": headers}


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

    # 2. Undo Rename_File first, so column ops below see the source-side name.
    for _, r in step[step["Action"] == "Rename_File"].iterrows():
        _rename_file(d, old=str(r["File"]), new=str(r["Value"]))

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
                _drop_table_column(path, var)

        # 3b. Undo Deletes (restore source-present columns/keys from extras).
        for _, r in grp[grp["Action"] == "Delete"].iterrows():
            var = str(r["Variable"])
            spec = deleted.get(fname, {}).get(var)
            if spec is None:
                continue  # not in source (add-then-delete transient): skip
            if is_nml:
                _restore_nml_key(path, var, spec["scalar"])
            elif path.exists():
                _restore_table_column(path, var, spec["col"], spec["values"])

        # 3c. Undo Renames (forward old->new becomes new->old).
        for _, r in grp[grp["Action"] == "Rename"].iterrows():
            old, new = str(r["Variable"]), str(r["Value"])
            if is_nml:
                _rename_nml_key(path, new, old)
            elif path.exists():
                _rename_table_column(path, new, old)


def reverse_convert_table(
    src_dir,
    out_dir,
    to_ver: str,
    legacy_extras: dict,
    from_ver: str | None = None,
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
