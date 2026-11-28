#!/usr/bin/env python3
"""Static lint for pytest dependency-axis markers and test-lane coverage (gh#1300, gh#2349).

Complements the pytest_collection_finish hook in test/conftest.py. The
hook is the authoritative runtime check, but it requires importing supy
(which requires a compiled build). This script is a supy-free static
pass so CI can lint marker presence and lane coverage without building --
keep CI cheap for test-only PRs.

Checks two invariants under ``test/``:
1. Every pytest-discoverable file (``test_*.py`` or ``*_test.py``) outside
   ``AUTO_APPLIED_DIRS`` declares exactly one primary dependency axis in
   ``AXIS_MARKERS`` (``tooling``, ``unit``, ``native``, or ``physics``) via
   module-level ``pytestmark``, without retired ``api`` markers or conflicting
   per-test/class axis decorators. Open-PR files listed in
   ``LOCKED_PR_FILE_AXES`` may retain their legacy ``api`` marker until those
   PRs land and ``W1-7`` removes the compatibility shim.
2. Every pytest-discoverable file under ``test/`` lies inside either
   ``Makefile``'s ``TEST_DEFAULT_PATHS`` or ``PERIPHERAL_TEST_DIRS``, and
   every entry in ``TEST_DEFAULT_PATHS`` parses cleanly and exists under
   ``test/``.

The ``Makefile`` parser intentionally supports only direct literal
``TEST_DEFAULT_PATHS`` assignments (plus backslash continuations, inline
comments, and ``+=`` appends) and fails closed on dynamic Make constructs
(``include``, ``eval``, ``call``, ``define``, conditionals, variable
references, computed names, and unsupported operators).

Exits 0 when every check passes, 1 otherwise.
"""

from __future__ import annotations

import ast
from collections.abc import Mapping
import os
from pathlib import Path
import re
import stat
import sys

AXIS_MARKERS: frozenset[str] = frozenset({"tooling", "unit", "native", "physics"})
LEGACY_SHIM_MARKER: str = "api"

# Existing master files touched by open held PRs, plus brand-new api-marked
# files in open PRs not yet merged to master. Static lint allows legacy `api`
# on these exact repo-relative POSIX paths, and test/conftest.py attaches the
# mapped primary axis at collection time until those PRs land and W1-7 removes
# the Schema-1 compatibility shim.
LOCKED_PR_FILE_AXES: dict[str, str] = {
    # Existing master files touched by open PRs (pytestmark left untouched):
    "test/core/test_check_data_interface_version_history.py": "tooling",  # #2423
    "test/core/test_util_gap_filler_debug.py": "unit",  # #2431
    "test/data_model/test_finite_senescence_migration.py": "unit",  # #2407
    "test/data_model/test_hourly_profile_keys.py": "unit",  # #2407
    "test/data_model/test_legacy_table_roundtrip.py": "unit",  # #2407
    "test/data_model/test_physics_options.py": "unit",  # #2373
    "test/data_model/test_validation.py": "unit",  # #2373
    "test/data_model/test_yaml_processing.py": "unit",  # #2373
    "test/data_model/test_yaml_upgrade.py": "unit",  # #2407
    "test/io_tests/test_dailystate_output.py": "physics",  # #1759, #2066
    "test/io_tests/test_engine_anthro_snow_guards.py": "physics",  # #2373, #2407
    "test/io_tests/test_engine_bioco2_ohm_lai_guards.py": "native",  # #2066
    "test/io_tests/test_native_spartacus_ehc_guards.py": "physics",  # #2373
    "test/io_tests/test_yaml_annotation.py": "unit",  # #2066
    "test/physics/test_option_sweep.py": "physics",  # #2373
    # Brand-new api-marked files in open PRs not yet merged to master:
    "test/core/test_util_profile_manager_csv.py": "unit",  # #2429
    "test/data_model/test_day_night_profile_scale.py": "unit",  # #2407
    "test/data_model/test_validator_stack2_g.py": "unit",  # #2406
    "test/docs/test_code_examples.py": "tooling",  # #2400
}

# Orthogonal markers (e.g. `e2e`) attached by test/conftest.py for locked
# open-PR files that launch real engine subprocesses, without editing the
# locked files themselves before those PRs merge.
LOCKED_PR_FILE_EXTRA_MARKERS: dict[str, tuple[str, ...]] = {
    "test/io_tests/test_engine_anthro_snow_guards.py": ("e2e",),  # #2373, #2407
    "test/io_tests/test_engine_bioco2_ohm_lai_guards.py": ("e2e",),  # #2066
    "test/io_tests/test_native_spartacus_ehc_guards.py": ("e2e",),  # #2373
}

# Directories whose tests pick up their primary axis (`native`) automatically
# via pytest_collection_modifyitems. Files under these prefixes do not declare
# a module-level axis (but are still checked for conflicts and lane coverage).
AUTO_APPLIED_DIRS: tuple[str, ...] = ("test/umep/",)

# Documented peripheral test surfaces intentionally excluded from Makefile's
# TEST_DEFAULT_PATHS (see Makefile and .claude/rules/00-project-essentials.md).
PERIPHERAL_TEST_DIRS: tuple[str, ...] = (
    "test/cmd",
    "test/mcp",
    "test/docs",
    "test/knowledge",
    "test/umep",
)

# Make parser regexes (applied to leading-whitespace-trimmed non-recipe lines).
_MAKE_WHITESPACE = " \t"
_COND_DIRECTIVE_RE = re.compile(r"^(?:ifeq|ifneq|ifdef|ifndef)(?:[ \t(]|$)")
_DEFINE_DIRECTIVE_RE = re.compile(
    r"^(?:(?:override|export|private)[ \t]+)*define[ \t]+[^ \t]+"
)
_INCLUDE_RE = re.compile(r"^(?:-include|sinclude|include)[ \t]+")
_UNSUPPORTED_DIRECTIVE_RE = re.compile(
    r"^(?:(?:override|private)[ \t]+)*"
    r"(?P<directive>export|unexport|undefine|vpath)"
    r"(?:[ \t]*$|[ \t]+[^:# \t])"
)
_DYNAMIC_MAKE_CALL_RE = re.compile(r"\$[({][ \t\f\v]*(?:eval|call)\b")
_RECIPEPREFIX_RE = re.compile(r"\.RECIPEPREFIX\b")
_PORTABLE_TOKEN_RE = re.compile(r"^[A-Za-z0-9_./-]+$")
_WINDOWS_RESERVED_NAMES: frozenset[str] = frozenset({
    "CON",
    "PRN",
    "AUX",
    "NUL",
    *(f"COM{i}" for i in range(1, 10)),
    *(f"LPT{i}" for i in range(1, 10)),
})
_MENTIONS_TARGET_RE = re.compile(r"\bTEST_DEFAULT_PATHS\b")
_PLAIN_ASSIGN_RE = re.compile(
    r"^TEST_DEFAULT_PATHS[ \t]*(?P<op>::=|:=|\?=|\+=|=)(?P<value>.*)$"
)
_ANY_VAR_ASSIGN_RE = re.compile(
    r"^(?:(?:override|export|private)[ \t]+)*"
    r"[^:#= \t]+[ \t]*(?::::=|::=|:=|\?=|\+=|!=|=)"
)
_RULE_HEADER_RE = re.compile(r"^[^#\t][^:=]*:")


def _to_ascii(text: str) -> str:
    """Escape non-ASCII characters so diagnostics stay strictly 0x00..0x7F."""
    return text.encode("ascii", errors="backslashreplace").decode("ascii")


def _is_link_or_junction(path: Path) -> bool:
    """Return True if ``path`` is a symlink or a Windows directory junction."""
    return path.is_symlink() or path.is_junction()


def _is_test_filename(filename: str) -> bool:
    """Return True if ``filename`` matches pytest discovery on POSIX or Windows."""
    if not filename.endswith(".py"):
        return False
    lowered = filename.lower()
    return lowered.startswith("test_") or lowered.endswith("_test.py")


def _lstat_entry(
    entry: Path,
    resolved_root: Path,
    errors: list[str],
) -> os.stat_result | None:
    """Return ``os.lstat(entry)`` or append a structured error on ``OSError``."""
    try:
        return os.lstat(entry)
    except OSError as exc:
        rel = entry.relative_to(resolved_root).as_posix()
        errors.append(f"unable to stat test entry {rel}: {exc}")
        return None


def _is_junction_entry(
    entry: Path,
    resolved_root: Path,
    errors: list[str],
) -> bool:
    """Return ``entry.is_junction()``, recording any ``OSError``."""
    try:
        return entry.is_junction()
    except OSError as exc:
        rel = entry.relative_to(resolved_root).as_posix()
        errors.append(f"unable to inspect test entry {rel}: {exc}")
        return False


def _validate_link_file(
    entry: Path,
    filename: str,
    resolved_root: Path,
    errors: list[str],
) -> bool:
    """Validate a symlinked file entry and report whether it is a test file."""
    rel = entry.relative_to(resolved_root).as_posix()
    try:
        target = entry.resolve(strict=True)
        target_stat = os.stat(target)
    except FileNotFoundError:
        errors.append(f"broken symlink under test/: {rel}")
        return False
    except (OSError, RuntimeError):
        errors.append(f"cyclic or unresolvable symlink under test/: {rel}")
        return False
    if not target.is_relative_to(resolved_root):
        errors.append(f"symlink under test/ escapes repository root: {rel}")
        return False
    return _is_test_filename(filename) and stat.S_ISREG(target_stat.st_mode)


def _walk_test_tree(
    resolved_root: Path,
    test_root: Path,
) -> tuple[list[Path], list[str]]:
    """Walk ``test_root`` once, collecting test files and layout/traversal errors."""
    discovered: list[Path] = []
    errors: list[str] = []

    def _on_walk_error(exc: OSError) -> None:
        raw_path = getattr(exc, "filename", None)
        if raw_path is not None:
            path_obj = Path(raw_path)
            label = (
                path_obj.relative_to(resolved_root).as_posix()
                if path_obj.is_relative_to(resolved_root)
                else path_obj.as_posix()
            )
            errors.append(f"unable to traverse test directory {label}: {exc}")
        else:
            errors.append(f"unable to traverse test directory: {exc}")

    for dirpath, dirnames, filenames in os.walk(
        test_root, followlinks=False, onerror=_on_walk_error
    ):
        current_dir = Path(dirpath)
        dirnames[:] = sorted(d for d in dirnames if d != "__pycache__")

        kept_dirs: list[str] = []
        for dirname in dirnames:
            entry = current_dir / dirname
            st = _lstat_entry(entry, resolved_root, errors)
            if st is None:
                continue
            if stat.S_ISLNK(st.st_mode) or _is_junction_entry(
                entry, resolved_root, errors
            ):
                rel = entry.relative_to(resolved_root).as_posix()
                errors.append(
                    f"unsupported directory symlink or junction under test/: {rel}"
                )
            else:
                kept_dirs.append(dirname)
        dirnames[:] = kept_dirs

        for filename in sorted(filenames):
            entry = current_dir / filename
            st = _lstat_entry(entry, resolved_root, errors)
            if st is None:
                continue
            if stat.S_ISLNK(st.st_mode) or _is_junction_entry(
                entry, resolved_root, errors
            ):
                if _validate_link_file(entry, filename, resolved_root, errors):
                    discovered.append(entry)
                continue
            if _is_test_filename(filename) and stat.S_ISREG(st.st_mode):
                discovered.append(entry)

    return (sorted(discovered), errors)


def iter_test_files(test_root: Path) -> list[Path]:
    """Return sorted pytest-discoverable files under ``test_root``.

    Parameters
    ----------
    test_root : Path
        Root directory of the test tree (typically ``<repo>/test``).

    Returns
    -------
    list[Path]
        Sorted list of unique ``test_*.py`` and ``*_test.py`` files,
        excluding ``__pycache__`` entries and directory symlinks/junctions.
    """
    discovered: list[Path] = []
    for dirpath, dirnames, filenames in os.walk(test_root, followlinks=False):
        current_dir = Path(dirpath)
        dirnames[:] = sorted(
            d
            for d in dirnames
            if d != "__pycache__" and not _is_link_or_junction(current_dir / d)
        )
        for filename in sorted(filenames):
            if not _is_test_filename(filename):
                continue
            entry = current_dir / filename
            if entry.is_file():
                discovered.append(entry)
    return sorted(discovered)


_MUTATING_LIST_METHODS: frozenset[str] = frozenset({
    "append",
    "clear",
    "extend",
    "insert",
    "pop",
    "remove",
    "reverse",
    "sort",
    "__iadd__",
    "__imul__",
})


def _direct_pytest_marker_name(node: ast.AST) -> str | None:
    """Return marker name when ``node`` is ``pytest.mark.<name>`` or ``mark.<name>``."""
    target = node.func if isinstance(node, ast.Call) else node
    if isinstance(target, ast.Attribute):
        parent = target.value
        if (isinstance(parent, ast.Attribute) and parent.attr == "mark") or (
            isinstance(parent, ast.Name) and parent.id == "mark"
        ):
            return target.attr
    return None


def _is_dynamic_mark_call(node: ast.AST) -> bool:
    """Return True for dynamic ``getattr(pytest.mark, ...)`` style expressions."""
    if not isinstance(node, ast.Call):
        return False
    if isinstance(node.func, ast.Name) and node.func.id == "getattr" and node.args:
        first = node.args[0]
        if (isinstance(first, ast.Attribute) and first.attr == "mark") or (
            isinstance(first, ast.Name) and first.id == "mark"
        ):
            return True
    return False


def _iter_assigned_names(target: ast.AST):
    """Yield every variable name written by assignment target ``target``."""
    if isinstance(target, ast.Name):
        yield target.id
    elif isinstance(target, (ast.Tuple, ast.List)):
        for elt in target.elts:
            yield from _iter_assigned_names(elt)
    elif isinstance(target, ast.Starred):
        yield from _iter_assigned_names(target.value)


def _stmt_targets_name(stmt: ast.stmt, name: str) -> bool:
    """Return True if ``stmt`` assigns, annotates, or augments ``name``."""
    if isinstance(stmt, ast.Assign):
        return any(name in set(_iter_assigned_names(t)) for t in stmt.targets)
    if isinstance(stmt, (ast.AnnAssign, ast.AugAssign)):
        return name in set(_iter_assigned_names(stmt.target))
    return False


def _is_pytest_param_call(node: ast.Call) -> bool:
    """Return True when ``node`` is a ``pytest.param(...)`` or ``param(...)`` call."""
    func = node.func
    if isinstance(func, ast.Name) and func.id == "param":
        return True
    return (
        isinstance(func, ast.Attribute)
        and func.attr == "param"
        and isinstance(func.value, ast.Name)
        and func.value.id == "pytest"
    )


def _collect_safe_load_nodes(tree: ast.Module) -> set[int]:
    """Return AST node ids of read-only ``Name`` loads permitted for marker constants."""
    safe: set[int] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Call) and _is_pytest_param_call(node):
            for kw in node.keywords:
                if kw.arg == "marks" and isinstance(kw.value, ast.Name):
                    safe.add(id(kw.value))
        elif isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            for dec in node.decorator_list:
                if isinstance(dec, ast.Name):
                    safe.add(id(dec))
    return safe


def _tainted_names_in_node(
    node: ast.AST,
    top_stmts: set[ast.stmt],
    safe_load_nodes: set[int],
) -> set[str]:
    """Return variable names mutated, reassigned outside top level, or escaped by ``node``."""
    names: set[str] = set()
    if isinstance(node, ast.Assign) and node not in top_stmts:
        for target in node.targets:
            names.update(_iter_assigned_names(target))
    elif (isinstance(node, ast.AnnAssign) and node not in top_stmts) or isinstance(
        node, ast.AugAssign
    ):
        names.update(_iter_assigned_names(node.target))
    elif isinstance(node, ast.Delete):
        for target in node.targets:
            names.update(_iter_assigned_names(target))
            if isinstance(target, ast.Subscript) and isinstance(target.value, ast.Name):
                names.add(target.value.id)
    elif (
        isinstance(node, ast.Call)
        and isinstance(node.func, ast.Attribute)
        and isinstance(node.func.value, ast.Name)
        and node.func.attr in _MUTATING_LIST_METHODS
    ):
        names.add(node.func.value.id)
    elif (
        isinstance(node, ast.Subscript)
        and isinstance(node.ctx, ast.Store)
        and isinstance(node.value, ast.Name)
    ):
        names.add(node.value.id)
    elif (
        isinstance(node, ast.Name)
        and isinstance(node.ctx, ast.Load)
        and id(node) not in safe_load_nodes
    ):
        names.add(node.id)
    return names


def _collect_mutated_or_escaped_names(tree: ast.Module) -> set[str]:
    """Return variable names mutated, deleted, reassigned outside top level, or escaped."""
    top_stmts = set(tree.body)
    safe_load_nodes = _collect_safe_load_nodes(tree)
    tainted: set[str] = set()
    for node in ast.walk(tree):
        tainted.update(_tainted_names_in_node(node, top_stmts, safe_load_nodes))
    return tainted


def _build_single_assign_env(tree: ast.Module) -> dict[str, ast.AST]:
    """Map immutable module-level constants assigned once to their RHS expression."""
    counts: dict[str, int] = {}
    values: dict[str, ast.AST] = {}
    for stmt in tree.body:
        if isinstance(stmt, ast.Assign):
            for target in stmt.targets:
                if isinstance(target, ast.Name):
                    counts[target.id] = counts.get(target.id, 0) + 1
                    values[target.id] = stmt.value
                else:
                    for name in _iter_assigned_names(target):
                        counts[name] = counts.get(name, 0) + 2
        elif (
            isinstance(stmt, ast.AnnAssign)
            and isinstance(stmt.target, ast.Name)
            and stmt.value is not None
        ):
            counts[stmt.target.id] = counts.get(stmt.target.id, 0) + 1
            values[stmt.target.id] = stmt.value
        elif isinstance(stmt, ast.AugAssign):
            for name in _iter_assigned_names(stmt.target):
                counts[name] = counts.get(name, 0) + 2
    tainted = _collect_mutated_or_escaped_names(tree)
    return {
        name: values[name]
        for name, count in counts.items()
        if count == 1 and name not in tainted
    }


def _is_empty_list_literal(node: ast.AST | None) -> bool:
    """Return True when ``node`` is an empty list literal ``[]``."""
    return isinstance(node, ast.List) and not node.elts


def _record_local_target_write(
    target: ast.AST,
    value: ast.AST | None,
    init_vars: set[str],
    invalid: set[str],
) -> None:
    """Track a single assignment target inside a function for local mark-list analysis."""
    if isinstance(target, ast.Name):
        if _is_empty_list_literal(value) and target.id not in init_vars:
            init_vars.add(target.id)
        else:
            invalid.add(target.id)
    elif isinstance(target, ast.Subscript) and isinstance(target.value, ast.Name):
        invalid.add(target.value.id)


def _collect_local_mark_lists(func_node: ast.AST) -> dict[str, list[ast.AST]]:
    """Collect local list vars initialised to ``[]`` and mutated only via ``.append(...)``."""
    init_vars: set[str] = set()
    invalid: set[str] = set()
    appended: dict[str, list[ast.AST]] = {}

    for node in ast.walk(func_node):
        if isinstance(node, ast.Assign):
            for target in node.targets:
                _record_local_target_write(target, node.value, init_vars, invalid)
        elif isinstance(node, ast.AnnAssign):
            _record_local_target_write(node.target, node.value, init_vars, invalid)
        elif isinstance(node, ast.AugAssign) and isinstance(node.target, ast.Name):
            invalid.add(node.target.id)
        elif (
            isinstance(node, ast.Call)
            and isinstance(node.func, ast.Attribute)
            and isinstance(node.func.value, ast.Name)
        ):
            var_name = node.func.value.id
            if node.func.attr == "append" and len(node.args) == 1 and not node.keywords:
                appended.setdefault(var_name, []).append(node.args[0])
            elif node.func.attr in _MUTATING_LIST_METHODS:
                invalid.add(var_name)

    return {name: appended.get(name, []) for name in init_vars if name not in invalid}


def _validate_marker_expr(
    node: ast.AST,
    *,
    mod_env: Mapping[str, ast.AST],
    local_lists: Mapping[str, list[ast.AST]],
    allow_refs: bool,
    seen: frozenset[str] = frozenset(),
) -> tuple[set[str], list[str]]:
    """Extract marker names and unsupported forms from a marker-bearing expression."""
    direct = _direct_pytest_marker_name(node)
    if direct is not None:
        return ({direct}, [])
    if isinstance(node, (ast.List, ast.Tuple, ast.Set)):
        names: set[str] = set()
        bad: list[str] = []
        for elt in node.elts:
            sub_names, sub_bad = _validate_marker_expr(
                elt,
                mod_env=mod_env,
                local_lists=local_lists,
                allow_refs=allow_refs,
                seen=seen,
            )
            names.update(sub_names)
            bad.extend(sub_bad)
        return (names, bad)
    if allow_refs and isinstance(node, ast.IfExp):
        b_names, b_bad = _validate_marker_expr(
            node.body,
            mod_env=mod_env,
            local_lists=local_lists,
            allow_refs=True,
            seen=seen,
        )
        o_names, o_bad = _validate_marker_expr(
            node.orelse,
            mod_env=mod_env,
            local_lists=local_lists,
            allow_refs=True,
            seen=seen,
        )
        return (b_names | o_names, b_bad + o_bad)
    if allow_refs and isinstance(node, ast.Name) and node.id not in seen:
        next_seen = seen | {node.id}
        if node.id in local_lists:
            names = set()
            bad = []
            for item_expr in local_lists[node.id]:
                sub_names, sub_bad = _validate_marker_expr(
                    item_expr,
                    mod_env=mod_env,
                    local_lists=local_lists,
                    allow_refs=True,
                    seen=next_seen,
                )
                names.update(sub_names)
                bad.extend(sub_bad)
            return (names, bad)
        if node.id in mod_env:
            return _validate_marker_expr(
                mod_env[node.id],
                mod_env=mod_env,
                local_lists={},
                allow_refs=True,
                seen=next_seen,
            )
    return (set(), [type(node).__name__])


def _validate_pytestmark_decl(node: ast.AST) -> tuple[set[str], list[str]]:
    """Validate strict pytest ``pytestmark`` grammar: one mark or a flat list of marks."""
    direct = _direct_pytest_marker_name(node)
    if direct is not None:
        return ({direct}, [])
    if isinstance(node, ast.List):
        names: set[str] = set()
        bad: list[str] = []
        for elt in node.elts:
            elt_name = _direct_pytest_marker_name(elt)
            if elt_name is not None:
                names.add(elt_name)
            else:
                bad.append(type(elt).__name__)
        return (names, bad)
    return (set(), [type(node).__name__])


def _extract_scope_pytestmark(stmts: list[ast.stmt]) -> tuple[set[str], list[str]]:
    """Validate and extract the single effective ``pytestmark`` in ``stmts``."""
    markers: set[str] = set()
    unsupported: list[str] = []
    decl_count = 0

    for stmt in stmts:
        if isinstance(stmt, ast.AugAssign) and _stmt_targets_name(stmt, "pytestmark"):
            unsupported.append("augassign-pytestmark")
        elif _stmt_targets_name(stmt, "pytestmark"):
            decl_count += 1
            if isinstance(stmt, ast.Assign) and not all(
                isinstance(t, ast.Name) for t in stmt.targets
            ):
                unsupported.append("destructured-pytestmark")
            else:
                value = (
                    stmt.value
                    if isinstance(stmt, (ast.Assign, ast.AnnAssign))
                    else None
                )
                if value is None:
                    unsupported.append("empty-pytestmark-annotation")
                else:
                    names, bad = _validate_pytestmark_decl(value)
                    markers.update(names)
                    unsupported.extend(bad)
        elif not isinstance(stmt, ast.ClassDef):
            for nested in ast.walk(stmt):
                if isinstance(nested, ast.stmt) and _stmt_targets_name(
                    nested, "pytestmark"
                ):
                    unsupported.append("conditional-pytestmark")

    if decl_count > 1:
        unsupported.append("reassigned-pytestmark")
    return (markers, unsupported)


def _has_pytestmark_mutation_or_escape(tree: ast.AST) -> bool:
    """Return True if ``pytestmark`` is read, aliased, mutated in place, or deleted."""
    for node in ast.walk(tree):
        if (
            isinstance(node, ast.Name)
            and node.id == "pytestmark"
            and isinstance(node.ctx, (ast.Load, ast.Del))
        ):
            return True
    return False


def _inspect_decorator_expr(
    decorator: ast.AST,
    mod_env: Mapping[str, ast.AST],
) -> tuple[set[str], list[str]]:
    """Extract applied marker name(s) or reject dynamic marker-producing decorators."""
    direct = _direct_pytest_marker_name(decorator)
    if direct is not None:
        return ({direct}, [])
    if isinstance(decorator, ast.Name) and decorator.id in mod_env:
        bound = _direct_pytest_marker_name(mod_env[decorator.id])
        return ({bound}, []) if bound is not None else (set(), [])

    # Inspect chained call/attribute heads such as `@getattr(pytest.mark, "physics")()`
    # or `@pytest.mark.physics.with_args()` without walking ordinary test-data arguments.
    cur: ast.AST = decorator
    while isinstance(cur, (ast.Call, ast.Attribute)):
        if _is_dynamic_mark_call(cur):
            return (set(), ["dynamic-mark-decorator"])
        head_mark = _direct_pytest_marker_name(cur)
        if head_mark is not None:
            return ({head_mark}, [])
        cur = cur.func if isinstance(cur, ast.Call) else cur.value
    return (set(), [])


def _inspect_param_marks_in_scope(
    root: ast.AST,
    *,
    mod_env: Mapping[str, ast.AST],
    local_lists: Mapping[str, list[ast.AST]],
    dec_markers: set[str],
    unsupported: list[str],
) -> None:
    """Inspect ``pytest.param(..., marks=...)`` calls directly inside ``root``."""
    stack: list[ast.AST] = list(ast.iter_child_nodes(root))
    while stack:
        node = stack.pop()
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            # Recurse into nested functions with their own local list bindings.
            nested_lists = _collect_local_mark_lists(node)
            _inspect_param_marks_in_scope(
                node,
                mod_env=mod_env,
                local_lists=nested_lists,
                dec_markers=dec_markers,
                unsupported=unsupported,
            )
            continue
        if isinstance(node, ast.Call) and _is_pytest_param_call(node):
            for kw in node.keywords:
                if kw.arg == "marks":
                    names, bad = _validate_marker_expr(
                        kw.value,
                        mod_env=mod_env,
                        local_lists=local_lists,
                        allow_refs=True,
                    )
                    dec_markers.update(names)
                    if bad:
                        unsupported.append(f"dynamic-param-marks({','.join(bad)})")
        stack.extend(ast.iter_child_nodes(node))


def inspect_file_axis_markers(
    source: str,
) -> tuple[set[str], set[str], bool, bool, str | None, list[str]]:
    """Inspect module-level and nested axis/legacy markers in Python source.

    Returns
    -------
    tuple[set[str], set[str], bool, bool, str | None, list[str]]
        ``(mod_axes, dec_axes, mod_has_api, dec_has_api, syntax_error,
        unsupported_forms)`` where ``mod_axes`` are primary axes referenced in
        module-level ``pytestmark``, ``dec_axes`` are primary axes referenced in
        class-level ``pytestmark``, class/function decorators, or
        ``pytest.param(..., marks=...)`` calls, ``mod_has_api`` and
        ``dec_has_api`` report retired ``api`` usage, ``syntax_error`` holds any
        ``SyntaxError`` message, and ``unsupported_forms`` records dynamic,
        reassigned, mutated, aliased, or conditional marker declarations.
    """
    try:
        tree = ast.parse(source)
    except SyntaxError as exc:
        return (set(), set(), False, False, exc.msg or "invalid syntax", [])

    mod_env = _build_single_assign_env(tree)
    mod_markers, unsupported = _extract_scope_pytestmark(tree.body)
    if _has_pytestmark_mutation_or_escape(tree):
        unsupported.append("mutated-or-aliased-pytestmark")

    dec_markers: set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            for decorator in node.decorator_list:
                d_names, d_bad = _inspect_decorator_expr(decorator, mod_env)
                dec_markers.update(d_names)
                unsupported.extend(d_bad)
        if isinstance(node, ast.ClassDef):
            cls_markers, cls_bad = _extract_scope_pytestmark(node.body)
            dec_markers.update(cls_markers)
            unsupported.extend(cls_bad)

    _inspect_param_marks_in_scope(
        tree,
        mod_env=mod_env,
        local_lists={},
        dec_markers=dec_markers,
        unsupported=unsupported,
    )

    return (
        mod_markers & AXIS_MARKERS,
        dec_markers & AXIS_MARKERS,
        LEGACY_SHIM_MARKER in mod_markers,
        LEGACY_SHIM_MARKER in dec_markers,
        None,
        unsupported,
    )


def _check_locked_file_axis(
    rel_posix: str,
    expected_locked_axis: str,
    mod_axes: set[str],
    dec_axes: set[str],
    mod_has_api: bool,
) -> str | None:
    """Validate a locked open-PR file against its mapped primary axis."""
    error: str | None = None
    if expected_locked_axis not in AXIS_MARKERS:
        error = (
            f"{rel_posix}: invalid LOCKED_PR_FILE_AXES mapping {expected_locked_axis!r}"
        )
    elif len(mod_axes) > 1:
        error = (
            f"{rel_posix}: declares multiple primary axis markers "
            f"({', '.join(sorted(mod_axes))}); declare exactly one"
        )
    elif not mod_axes and not mod_has_api:
        error = (
            f"{rel_posix}: missing primary axis marker or legacy "
            f"`{LEGACY_SHIM_MARKER}` marker (locked open-PR file)"
        )
    elif len(mod_axes) == 1:
        declared = next(iter(mod_axes))
        extra_dec = sorted(dec_axes - mod_axes)
        if mod_has_api and declared != expected_locked_axis:
            error = (
                f"{rel_posix}: locked file declares `{declared}` alongside "
                f"legacy `{LEGACY_SHIM_MARKER}` but maps to `{expected_locked_axis}`"
            )
        elif extra_dec:
            error = (
                f"{rel_posix}: declares conflicting decorator axis marker(s) "
                f"({', '.join(extra_dec)}); module axis is `{declared}`"
            )
    else:
        extra_dec = sorted(dec_axes - {expected_locked_axis})
        if extra_dec:
            error = (
                f"{rel_posix}: locked file declares conflicting decorator axis "
                f"marker(s) ({', '.join(extra_dec)}); mapped axis is "
                f"`{expected_locked_axis}`"
            )
    return error


def _check_regular_file_axis(
    rel_posix: str,
    mod_axes: set[str],
    dec_axes: set[str],
    mod_has_api: bool,
    dec_has_api: bool,
) -> str | None:
    """Validate a non-locked, non-auto-applied test file's primary axis."""
    error: str | None = None
    if mod_has_api or dec_has_api:
        error = (
            f"{rel_posix}: uses retired `{LEGACY_SHIM_MARKER}` marker directly; "
            f"declare exactly one primary axis marker ({', '.join(sorted(AXIS_MARKERS))})"
        )
    elif not mod_axes:
        error = (
            f"{rel_posix}: missing primary axis marker; declare exactly one of "
            f"{', '.join(sorted(AXIS_MARKERS))} at module level"
        )
    elif len(mod_axes) > 1:
        error = (
            f"{rel_posix}: declares multiple primary axis markers "
            f"({', '.join(sorted(mod_axes))}); declare exactly one"
        )
    else:
        declared = next(iter(mod_axes))
        extra_dec = sorted(dec_axes - mod_axes)
        if extra_dec:
            error = (
                f"{rel_posix}: declares conflicting decorator axis marker(s) "
                f"({', '.join(extra_dec)}); module axis is `{declared}`"
            )
    return error


def check_file_axis_markers(
    rel_posix: str,
    source: str,
    *,
    locked_axes: Mapping[str, str] = LOCKED_PR_FILE_AXES,
    auto_applied: bool = False,
) -> str | None:
    """Validate a single test file's primary dependency-axis marker declaration.

    Returns ``None`` when the file satisfies the Schema-2 single-axis contract
    (or the ``LOCKED_PR_FILE_AXES`` compatibility rule for open held PRs), or an
    ASCII-safe error string explaining the violation.
    """
    mod_axes, dec_axes, mod_has_api, dec_has_api, syntax_error, unsupported = (
        inspect_file_axis_markers(source)
    )
    if syntax_error is not None:
        return f"{rel_posix}: syntax error ({syntax_error})"
    if unsupported:
        forms = ", ".join(sorted(set(unsupported)))
        return (
            f"{rel_posix}: unsupported dynamic marker expression ({forms}); "
            "declare explicit pytest.mark.<name> literals"
        )

    if auto_applied:
        bad_auto = sorted(mod_axes | dec_axes)
        if bad_auto or mod_has_api or dec_has_api:
            found = [
                *bad_auto,
                *([LEGACY_SHIM_MARKER] if (mod_has_api or dec_has_api) else []),
            ]
            return (
                f"{rel_posix}: auto-applied directory file must not declare explicit "
                f"axis or legacy api markers ({', '.join(found)})"
            )
        return None

    expected_locked_axis = locked_axes.get(rel_posix)
    if expected_locked_axis is not None:
        return _check_locked_file_axis(
            rel_posix, expected_locked_axis, mod_axes, dec_axes, mod_has_api
        )

    return _check_regular_file_axis(
        rel_posix, mod_axes, dec_axes, mod_has_api, dec_has_api
    )


def file_has_valid_axis_marker(
    source: str,
    *,
    rel_posix: str = "",
    locked_axes: Mapping[str, str] = LOCKED_PR_FILE_AXES,
) -> bool:
    """Return ``True`` when ``source`` satisfies the single-axis marker rule."""
    return check_file_axis_markers(rel_posix, source, locked_axes=locked_axes) is None


def is_auto_applied(rel: Path) -> bool:
    """Return whether ``rel`` inherits its nature marker from a conftest hook.

    Parameters
    ----------
    rel : Path
        Repository-relative path to a test file.

    Returns
    -------
    bool
        ``True`` when ``rel`` starts with any prefix in ``AUTO_APPLIED_DIRS``.
    """
    rel_posix = rel.as_posix()
    return any(rel_posix.startswith(prefix) for prefix in AUTO_APPLIED_DIRS)


def _has_odd_trailing_backslashes(line: str) -> bool:
    """Return True when ``line`` ends with an odd count of backslashes."""
    trailing = len(line) - len(line.rstrip("\\"))
    return trailing % 2 == 1


def _split_make_words(value: str) -> list[str]:
    """Split on GNU Make ASCII blanks (space and tab) only."""
    return [token for token in re.split(r"[ \t]+", value) if token]


def _join_makefile_continuations(text: str) -> list[str]:
    """Join GNU Make physical-newline continuations respecting backslash parity."""
    logical_lines: list[str] = []
    buffer = ""
    in_continuation = False
    raw_parts = text.split("\n")
    last_idx = len(raw_parts) - 1
    for idx, raw_part in enumerate(raw_parts):
        has_newline = idx < last_idx
        raw_line = (
            raw_part[:-1] if has_newline and raw_part.endswith("\r") else raw_part
        )
        if has_newline and _has_odd_trailing_backslashes(raw_line):
            part = raw_line[:-1]
            if in_continuation:
                trimmed = part.lstrip(_MAKE_WHITESPACE)
                buffer = f"{buffer} {trimmed}" if buffer else trimmed
            else:
                buffer = part
            in_continuation = True
        elif in_continuation:
            trimmed = raw_line.lstrip(_MAKE_WHITESPACE)
            joined = f"{buffer} {trimmed}" if buffer else trimmed
            logical_lines.append(joined)
            buffer = ""
            in_continuation = False
        else:
            logical_lines.append(raw_line)
    return logical_lines


def _validate_relative_token(token: str, source_label: str) -> str:
    """Validate and normalise a portable relative POSIX path token."""
    if not _PORTABLE_TOKEN_RE.fullmatch(token):
        raise ValueError(
            f"{source_label} contains unsupported path characters: {token}"
        )
    if token.startswith("/") or Path(token).is_absolute():
        raise ValueError(f"{source_label} contains absolute path: {token}")
    path_token = Path(token)
    if ".." in path_token.parts:
        raise ValueError(f"{source_label} contains parent traversal: {token}")
    for part in path_token.parts:
        if (
            part.endswith(".")
            or part.split(".", 1)[0].upper() in _WINDOWS_RESERVED_NAMES
        ):
            raise ValueError(
                f"{source_label} contains Windows-reserved path component: {token}"
            )
    normalised = path_token.as_posix()
    if normalised in {"", "."}:
        raise ValueError(f"{source_label} contains invalid path token: {token}")
    return normalised


def _split_makefile_code_and_inline_recipe(
    stripped: str,
    *,
    is_assignment: bool,
) -> tuple[str, str | None]:
    """Split a non-recipe line at its first unescaped ``#`` or inline rule ``;``."""
    for idx, ch in enumerate(stripped):
        if ch == "#":
            backslashes = idx - len(stripped[:idx].rstrip("\\"))
            if backslashes % 2 == 0:
                return (stripped[:idx], None)
        elif ch == ";" and not is_assignment and _RULE_HEADER_RE.match(stripped[:idx]):
            return (stripped[:idx], stripped[idx + 1 :])
    return (stripped, None)


def _reject_unsupported_makefile_line(code: str, *, is_assignment: bool) -> None:
    """Reject top-level Make directives or expansions that can mutate variables."""
    if any((ord(ch) < 0x20 and ch != "\t") or ch == "\x7f" for ch in code):
        raise ValueError(
            "Makefile contains unsupported control character: "
            f"{code.strip(_MAKE_WHITESPACE)}"
        )
    if _INCLUDE_RE.match(code):
        raise ValueError(
            "Makefile include directives are unsupported when parsing "
            "TEST_DEFAULT_PATHS"
        )
    if _DEFINE_DIRECTIVE_RE.match(code):
        raise ValueError(
            "Makefile define blocks are unsupported when parsing TEST_DEFAULT_PATHS"
        )
    if _COND_DIRECTIVE_RE.match(code):
        raise ValueError(
            "Makefile conditionals are unsupported when parsing TEST_DEFAULT_PATHS"
        )
    directive_match = _UNSUPPORTED_DIRECTIVE_RE.match(code)
    if directive_match is not None:
        directive = directive_match.group("directive")
        raise ValueError(
            f"Makefile {directive} directives are unsupported when parsing "
            f"TEST_DEFAULT_PATHS: {code.strip(_MAKE_WHITESPACE)}"
        )
    if _RECIPEPREFIX_RE.search(code):
        raise ValueError(
            "Makefile .RECIPEPREFIX is unsupported when parsing TEST_DEFAULT_PATHS"
        )
    if "$" in code:
        raise ValueError(
            "Makefile variable reference or expansion is unsupported when parsing "
            f"TEST_DEFAULT_PATHS: {code.strip()}"
        )
    if not is_assignment and _RULE_HEADER_RE.match(code):
        _, after_colon = code.split(":", 1)
        if "=" in after_colon:
            raise ValueError(
                "Makefile target-specific variable assignments are unsupported "
                f"when parsing TEST_DEFAULT_PATHS: {code.strip(_MAKE_WHITESPACE)}"
            )


def _apply_makefile_target_line(
    raw_line: str,
    code: str,
    raw_tokens: list[str],
    found: bool,
) -> tuple[list[str], bool]:
    """Validate and apply one non-recipe line referencing ``TEST_DEFAULT_PATHS``."""
    if raw_line.startswith("\t"):
        raise ValueError(
            "TEST_DEFAULT_PATHS uses unsupported tab-indented construct outside a recipe"
        )
    match = _PLAIN_ASSIGN_RE.match(code)
    if match is None:
        raise ValueError(
            f"TEST_DEFAULT_PATHS uses unsupported Make construct: {code.strip()}"
        )
    value = match.group("value")
    if "$" in value:
        raise ValueError(
            "TEST_DEFAULT_PATHS contains unsupported Make variable reference: "
            f"{value.strip()}"
        )
    op = match.group("op")
    tokens = _split_make_words(value)
    if op in {":=", "::=", "="}:
        return (tokens, True)
    if op == "?=":
        return (raw_tokens, True) if found else (tokens, True)
    return ([*raw_tokens, *tokens], True)


def parse_makefile_test_default_paths(makefile_path: Path) -> list[str]:
    """Parse ``TEST_DEFAULT_PATHS`` directly from a ``Makefile``.

    Parameters
    ----------
    makefile_path : Path
        Path to the repository ``Makefile``.

    Returns
    -------
    list[str]
        Normalised repository-relative POSIX paths declared in
        ``TEST_DEFAULT_PATHS``.

    Raises
    ------
    ValueError
        If ``TEST_DEFAULT_PATHS`` is missing, empty, uses unsupported Make
        syntax (conditionals, variable references, ``include``, ``eval``,
        ``call``, ``define``, ``export``, ``unexport``, ``undefine``,
        ``vpath``, ``.RECIPEPREFIX``, directive prefixes, ``:::=``/``!=``,
        target-specific assignments, computed variable names, or tab-indented
        assignments outside a recipe), or contains an invalid path token.
    """
    with makefile_path.open("r", encoding="utf-8", newline="") as handle:
        text = handle.read()
    found = False
    raw_tokens: list[str] = []
    in_rule = False

    for line in _join_makefile_continuations(text):
        if line.startswith("\t") and in_rule:
            if _DYNAMIC_MAKE_CALL_RE.search(line):
                raise ValueError(
                    "Makefile eval/call inside recipe is unsupported when parsing "
                    f"TEST_DEFAULT_PATHS: {line.strip()}"
                )
            continue

        stripped = line.lstrip(_MAKE_WHITESPACE)
        if not stripped or stripped.startswith("#"):
            continue

        is_assignment = bool(_ANY_VAR_ASSIGN_RE.match(stripped))
        code, inline_recipe = _split_makefile_code_and_inline_recipe(
            stripped, is_assignment=is_assignment
        )
        if inline_recipe is not None and _DYNAMIC_MAKE_CALL_RE.search(inline_recipe):
            raise ValueError(
                "Makefile eval/call inside inline recipe is unsupported when "
                f"parsing TEST_DEFAULT_PATHS: {stripped}"
            )
        _reject_unsupported_makefile_line(code, is_assignment=is_assignment)

        if _MENTIONS_TARGET_RE.search(code):
            raw_tokens, found = _apply_makefile_target_line(
                line, code, raw_tokens, found
            )
            in_rule = False
            continue

        if is_assignment:
            in_rule = False
        elif _RULE_HEADER_RE.match(code) and not line.startswith("\t"):
            in_rule = True
        else:
            in_rule = False

    if not found:
        raise ValueError(
            f"TEST_DEFAULT_PATHS assignment not found in {makefile_path.name}"
        )
    if not raw_tokens:
        raise ValueError(f"TEST_DEFAULT_PATHS is empty in {makefile_path.name}")

    return [
        _validate_relative_token(token, "TEST_DEFAULT_PATHS") for token in raw_tokens
    ]


def _is_covered_by_lane(
    rel_path: Path,
    covered_files: set[Path],
    covered_dirs: set[Path],
) -> bool:
    """Return True if ``rel_path`` matches a covered file or directory."""
    if rel_path in covered_files:
        return True
    return any(rel_path.is_relative_to(directory) for directory in covered_dirs)


def _collect_covered_lanes(
    resolved_root: Path,
    resolved_test_root: Path,
    default_paths: list[str],
    peripheral_dirs: tuple[str, ...],
) -> tuple[list[str], set[Path], set[Path]]:
    """Validate default and peripheral lane entries and return covered sets."""
    errors: list[str] = []
    covered_files: set[Path] = set()
    covered_dirs: set[Path] = set()

    for entry in default_paths:
        raw_candidate = resolved_root / entry
        try:
            raw_stat = os.lstat(raw_candidate)
            is_link_or_junc = (
                stat.S_ISLNK(raw_stat.st_mode) or raw_candidate.is_junction()
            )
            candidate = raw_candidate.resolve(strict=True)
            target_stat = os.stat(candidate)
        except FileNotFoundError:
            errors.append(f"TEST_DEFAULT_PATHS entry does not exist: {entry}")
            continue
        except (OSError, RuntimeError) as exc:
            errors.append(
                f"TEST_DEFAULT_PATHS entry cannot be inspected ({entry}): {exc}"
            )
            continue
        if stat.S_ISDIR(target_stat.st_mode) and is_link_or_junc:
            errors.append(
                f"TEST_DEFAULT_PATHS entry is a directory symlink or junction: {entry}"
            )
            continue
        if not candidate.is_relative_to(resolved_root):
            errors.append(f"TEST_DEFAULT_PATHS entry escapes repository root: {entry}")
            continue
        if not candidate.is_relative_to(resolved_test_root):
            errors.append(f"TEST_DEFAULT_PATHS entry is outside test/: {entry}")
            continue
        rel_entry = Path(entry)
        if stat.S_ISREG(target_stat.st_mode):
            covered_files.add(rel_entry)
        elif stat.S_ISDIR(target_stat.st_mode):
            covered_dirs.add(rel_entry)
        else:
            errors.append(
                f"TEST_DEFAULT_PATHS entry is neither a file nor directory: {entry}"
            )

    for peripheral in peripheral_dirs:
        try:
            norm_peripheral = _validate_relative_token(
                peripheral, "PERIPHERAL_TEST_DIRS"
            )
        except ValueError as exc:
            errors.append(str(exc))
            continue
        covered_dirs.add(Path(norm_peripheral))

    return (errors, covered_files, covered_dirs)


def _check_lane_coverage(
    repo_root: Path,
    *,
    makefile_path: Path | None = None,
    test_root: Path | None = None,
    peripheral_dirs: tuple[str, ...] = PERIPHERAL_TEST_DIRS,
) -> tuple[list[Path], list[str], list[str]]:
    """Walk ``test_root`` once and return ``(test_files, errors, orphans)``."""
    resolved_root = repo_root.resolve()
    raw_test_root = test_root if test_root is not None else resolved_root / "test"
    if _is_link_or_junction(raw_test_root):
        rel_test_root = (
            raw_test_root.relative_to(resolved_root).as_posix()
            if raw_test_root.is_relative_to(resolved_root)
            else raw_test_root.name
        )
        return ([], [f"unsupported directory symlink or junction: {rel_test_root}"], [])

    resolved_test_root = raw_test_root.resolve()
    if not resolved_test_root.is_relative_to(resolved_root):
        return (
            [],
            [f"test root escapes repository root: {raw_test_root.as_posix()}"],
            [],
        )

    discovered_files, walk_errors = _walk_test_tree(resolved_root, resolved_test_root)

    makefile = (
        makefile_path if makefile_path is not None else resolved_root / "Makefile"
    )
    if not makefile.is_file():
        return (
            discovered_files,
            [*walk_errors, f"Makefile not found at {makefile.as_posix()}"],
            [],
        )

    try:
        default_paths = parse_makefile_test_default_paths(makefile)
    except (OSError, UnicodeError, ValueError) as exc:
        return (discovered_files, [*walk_errors, str(exc)], [])

    errors, covered_files, covered_dirs = _collect_covered_lanes(
        resolved_root, resolved_test_root, default_paths, peripheral_dirs
    )
    errors.extend(walk_errors)

    orphans: list[str] = []
    for path in discovered_files:
        rel = path.relative_to(resolved_root)
        if not _is_covered_by_lane(rel, covered_files, covered_dirs):
            orphans.append(rel.as_posix())

    return (discovered_files, errors, orphans)


def find_orphan_test_files(
    repo_root: Path,
    *,
    makefile_path: Path | None = None,
    test_root: Path | None = None,
    peripheral_dirs: tuple[str, ...] = PERIPHERAL_TEST_DIRS,
) -> tuple[list[str], list[str]]:
    """Find test files outside ``Makefile`` ``TEST_DEFAULT_PATHS`` and peripheral dirs.

    Parameters
    ----------
    repo_root : Path
        Repository root directory.
    makefile_path : Path | None, optional
        Explicit ``Makefile`` path; defaults to ``repo_root / "Makefile"``.
    test_root : Path | None, optional
        Explicit test directory to scan; defaults to ``repo_root / "test"``.
    peripheral_dirs : tuple[str, ...], optional
        Documented peripheral test directories intentionally omitted from
        ``TEST_DEFAULT_PATHS``.

    Returns
    -------
    tuple[list[str], list[str]]
        ``(errors, orphans)`` where ``errors`` describes ``Makefile`` parse,
        symlink/junction, traversal, or invalid-path failures and ``orphans``
        lists uncovered test files as POSIX paths relative to ``repo_root``.
    """
    _, errors, orphans = _check_lane_coverage(
        repo_root,
        makefile_path=makefile_path,
        test_root=test_root,
        peripheral_dirs=peripheral_dirs,
    )
    return (errors, orphans)


def _check_axis_markers(
    repo_root: Path,
    test_files: list[Path],
    *,
    locked_axes: Mapping[str, str] = LOCKED_PR_FILE_AXES,
) -> tuple[int, int, int, list[str], list[str]]:
    """Scan test files for single-axis markers after confirming in-repo containment."""
    violations: list[str] = []
    read_errors: list[str] = []
    checked = 0
    auto_applied = 0
    locked_count = 0
    for path in test_files:
        rel = path.relative_to(repo_root)
        rel_posix = rel.as_posix()
        try:
            target = path.resolve(strict=True)
        except (OSError, RuntimeError) as exc:
            read_errors.append(f"{rel_posix}: {exc}")
            continue
        if not target.is_relative_to(repo_root):
            read_errors.append(f"{rel_posix}: resolved path escapes repo root")
            continue
        try:
            source = path.read_text(encoding="utf-8")
        except (OSError, UnicodeError) as exc:
            read_errors.append(f"{rel_posix}: {exc}")
            continue
        is_auto = is_auto_applied(rel)
        if is_auto:
            auto_applied += 1
        else:
            checked += 1
            if rel_posix in locked_axes:
                locked_count += 1
        err = check_file_axis_markers(
            rel_posix,
            source,
            locked_axes=locked_axes,
            auto_applied=is_auto,
        )
        if err is not None:
            violations.append(err)
    return (checked, auto_applied, locked_count, violations, read_errors)


def main(argv: list[str]) -> int:
    """Run marker-axis and orphan-test lane-coverage checks on a repository tree.

    Parameters
    ----------
    argv : list[str]
        Command-line arguments; ``argv[1]``, if provided, sets the repository
        root directory (defaulting to the current working directory).

    Returns
    -------
    int
        0 when all checks pass, 1 otherwise.
    """
    repo_root = Path(argv[1]).resolve() if len(argv) > 1 else Path.cwd().resolve()
    test_root = repo_root / "test"
    if _is_link_or_junction(test_root):
        print(
            _to_ascii(
                "[X] gh#2349 lane-coverage lint: "
                "unsupported directory symlink or junction: test"
            ),
            file=sys.stderr,
        )
        return 1
    if not test_root.is_dir():
        print(_to_ascii(f"[X] test/ not found under {repo_root}"), file=sys.stderr)
        return 1

    test_files, lane_errors, orphans = _check_lane_coverage(
        repo_root, test_root=test_root
    )
    checked, auto_applied, locked_count, violations, read_errors = _check_axis_markers(
        repo_root, test_files
    )

    failed = False
    if read_errors:
        failed = True
        print(
            "[X] gh#2349 marker lint: unable to read test files as UTF-8:",
            file=sys.stderr,
        )
        for err in read_errors:
            print(_to_ascii(f"  - {err}"), file=sys.stderr)

    if violations:
        failed = True
        print(
            "[X] gh#2349 marker lint: test files violating the single primary-axis contract:",
            file=sys.stderr,
        )
        for err in violations:
            print(_to_ascii(f"  - {err}"), file=sys.stderr)
        print(
            "Declare exactly one primary axis at module level "
            "(`pytestmark = pytest.mark.<tooling|unit|native|physics>`), "
            "or auto-apply via a conftest.py hook.",
            file=sys.stderr,
        )

    if lane_errors:
        failed = True
        print(
            "[X] gh#2349 lane-coverage lint: invalid Makefile TEST_DEFAULT_PATHS "
            "or test-tree layout:",
            file=sys.stderr,
        )
        for err in lane_errors:
            print(_to_ascii(f"  - {err}"), file=sys.stderr)

    if orphans:
        failed = True
        print(
            "[X] gh#2349 lane-coverage lint: orphan test files outside "
            "Makefile TEST_DEFAULT_PATHS and peripheral surfaces:",
            file=sys.stderr,
        )
        for rel_str in orphans:
            print(_to_ascii(f"  - {rel_str}"), file=sys.stderr)
        print(
            "Move each file into a directory listed in Makefile "
            "TEST_DEFAULT_PATHS or a documented peripheral surface "
            f"({', '.join(PERIPHERAL_TEST_DIRS)}), or register the lane "
            "explicitly.",
            file=sys.stderr,
        )

    if failed:
        return 1

    print(
        f"[OK] {checked} test files satisfy the single-axis marker contract "
        f"({locked_count} locked open-PR compatibility files); "
        f"{auto_applied} files pick up a marker via conftest auto-apply; "
        f"all {len(test_files)} test files are covered by Makefile "
        "TEST_DEFAULT_PATHS or peripheral surfaces."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
