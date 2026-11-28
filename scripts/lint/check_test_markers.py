#!/usr/bin/env python3
"""Static lint for pytest dependency-axis markers, lane coverage, and AST guards (gh#1300, gh#2349).

Complements the pytest_collection_finish hook in test/conftest.py. The
hook is the authoritative runtime check, but it requires importing supy
(which requires a compiled build). This script is a supy-free static
pass so CI can lint marker presence, lane coverage, and AST governance
guards without building -- keep CI cheap for test-only PRs.

Checks four invariants under ``test/``:
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
3. No ``@pytest.mark.parametrize``-decorated test function (or test method on
   a parametrised class) directly calls ``SUEWSSimulation.from_sample_data(...)``
   or a simulation ``.run(...)`` inside its body; shared reference simulations
   belong in session- or module-scoped fixtures. Pre-existing legacy debt is
   ratcheted monotonically in ``scripts/lint/test_ast_guards_baseline.json``.
4. Every test invoking a SUEWS CLI executable (``suews``, ``suews-validate``,
   ``suews-mcp``, ``suews-engine``, ``suews-convert``, ``suews-run``,
   ``suews-schema``, ``suews-inspect``, or ``python -m supy...``) via
   ``subprocess.{run,Popen,check_output,check_call}`` carries
   ``@pytest.mark.e2e`` (at module, class, or function level), and the total
   statically counted ``@pytest.mark.e2e`` subprocess test nodes per CLI
   command stays within ``DEFAULT_E2E_COMMAND_NODE_CAP`` (with monotonic
   ratchet entries in ``scripts/lint/test_ast_guards_baseline.json`` for
   pre-existing debt).

The ``Makefile`` parser intentionally supports only direct literal
``TEST_DEFAULT_PATHS`` assignments (plus backslash continuations, inline
comments, and ``+=`` appends) and fails closed on dynamic Make constructs
(``include``, ``eval``, ``call``, ``define``, conditionals, variable
references, computed names, and unsupported operators).

Exits 0 when every check passes, 1 otherwise.
"""

from __future__ import annotations

import argparse
import ast
from collections.abc import Mapping
import json
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

# Static AST guard configuration (gh#2349 W1-8).
AST_GUARDS_BASELINE_PATH: Path = (
    Path("scripts") / "lint" / "test_ast_guards_baseline.json"
)
AST_GUARDS_BASELINE_SCHEMA_VERSION: int = 1
SUEWS_CLI_COMMANDS: tuple[str, ...] = (
    "suews",
    "suews-validate",
    "suews-mcp",
    "suews-engine",
    "suews-convert",
    "suews-run",
    "suews-schema",
    "suews-inspect",
)
PYTHON_M_SUPY_COMMAND: str = "python -m supy"
ALL_SUEWS_CLI_COMMANDS: frozenset[str] = frozenset(
    (*SUEWS_CLI_COMMANDS, PYTHON_M_SUPY_COMMAND)
)
# Documented cap on statically counted `@pytest.mark.e2e` subprocess test nodes
# per SUEWS CLI command (gh#2349 W1-8). Pre-existing commands above this cap
# are pinned monotonically in `test_ast_guards_baseline.json`.
DEFAULT_E2E_COMMAND_NODE_CAP: int = 10

_SUBPROCESS_CALL_METHODS: frozenset[str] = frozenset({
    "run",
    "Popen",
    "check_output",
    "check_call",
})
_NON_SIMULATION_RUN_MODULES: frozenset[str] = frozenset({
    "subprocess",
    "asyncio",
    "cProfile",
    "profile",
    "pdb",
    "bdb",
    "doctest",
    "sched",
    "unittest",
    "click",
})
_NON_SIMULATION_RUN_RECEIVERS: frozenset[str] = _NON_SIMULATION_RUN_MODULES | frozenset({
    "TextTestRunner",
    "CliRunner",
    "runner",
    "cli_runner",
    "loop",
    "context",
    "ctx",
})
_ENGINE_BIN_VAR_NAMES: frozenset[str] = frozenset({
    "engine_bin",
    "engine_exe",
    "engine_binary",
})
_GENERIC_ENGINE_RUN_VARS: frozenset[str] = frozenset({
    "engine",
    "binary",
    "bin_path",
    "engine_path",
})
_SUEWS_EXE_VAR_NAMES: frozenset[str] = frozenset({
    "suews_exe",
    "suews_bin",
    "suews_cmd",
    "suews_cli",
})
_SUEWS_CLI_SET: frozenset[str] = frozenset(SUEWS_CLI_COMMANDS)
_BASELINE_ALLOWED_KEYS: frozenset[str] = frozenset({
    "schema_version",
    "description",
    "e2e_default_cap",
    "parametrized_simulations",
    "unmarked_subprocess_e2e",
    "e2e_command_caps",
})

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


class AstGuardsSnapshot:
    """Aggregated static AST guard measurements across the test tree.

    Defined as a plain class rather than a ``@dataclass`` because
    ``test/conftest.py`` executes this module via ``exec_module`` without
    registering it in ``sys.modules``, which breaks ``@dataclass`` under
    ``from __future__ import annotations``.
    """

    def __init__(
        self,
        parametrized_simulations: dict[str, int] | None = None,
        unmarked_subprocess_e2e: dict[str, list[str]] | None = None,
        e2e_command_nodes: dict[str, int] | None = None,
    ) -> None:
        """Initialise an AST guards snapshot with optional pre-populated maps."""
        self.parametrized_simulations: dict[str, int] = (
            dict(parametrized_simulations) if parametrized_simulations else {}
        )
        self.unmarked_subprocess_e2e: dict[str, list[str]] = (
            {k: list(v) for k, v in unmarked_subprocess_e2e.items()}
            if unmarked_subprocess_e2e
            else {}
        )
        self.e2e_command_nodes: dict[str, int] = (
            dict(e2e_command_nodes) if e2e_command_nodes else {}
        )

    __hash__ = None  # Mutable snapshot mappings are unhashable.

    def __eq__(self, other: object) -> bool:
        """Compare two snapshots by their three guard mappings."""
        if not isinstance(other, AstGuardsSnapshot):
            return NotImplemented
        return (
            self.parametrized_simulations == other.parametrized_simulations
            and self.unmarked_subprocess_e2e == other.unmarked_subprocess_e2e
            and self.e2e_command_nodes == other.e2e_command_nodes
        )

    def __repr__(self) -> str:
        """Return a readable representation of the snapshot."""
        return (
            "AstGuardsSnapshot("
            f"parametrized_simulations={self.parametrized_simulations!r}, "
            f"unmarked_subprocess_e2e={self.unmarked_subprocess_e2e!r}, "
            f"e2e_command_nodes={self.e2e_command_nodes!r})"
        )


def _normalise_cli_executable(token: str) -> str | None:
    """Return canonical SUEWS CLI command name when ``token`` names one."""
    cleaned = token.strip().strip("\"'")
    if not cleaned:
        return None
    basename = cleaned.replace("\\", "/").rsplit("/", 1)[-1].lower()
    if basename.endswith(".exe"):
        basename = basename[:-4]
    return basename if basename in _SUEWS_CLI_SET else None


def _leading_str_constant(node: ast.AST) -> str | None:
    """Return string literal value, or leading constant prefix of an f-string."""
    if isinstance(node, ast.Constant) and isinstance(node.value, str):
        return node.value
    if isinstance(node, ast.JoinedStr) and node.values:
        first = node.values[0]
        if isinstance(first, ast.Constant) and isinstance(first.value, str):
            return first.value
    return None


def _is_fixture_decorator(decorator: ast.AST) -> bool:
    """Return True when ``decorator`` is ``@pytest.fixture`` or ``@fixture``."""
    target = decorator.func if isinstance(decorator, ast.Call) else decorator
    if isinstance(target, ast.Name) and target.id == "fixture":
        return True
    return isinstance(target, ast.Attribute) and target.attr == "fixture"


def _root_name_of_expr(node: ast.AST) -> str | None:
    """Return the root ``ast.Name.id`` of a dotted attribute chain, if any."""
    cur: ast.AST = node
    while isinstance(cur, ast.Attribute):
        cur = cur.value
    if isinstance(cur, ast.Name):
        return cur.id
    return None


def _is_non_simulation_run_receiver(
    recv: ast.AST, non_sim_aliases: frozenset[str]
) -> bool:
    """Return True when ``<recv>.run(...)`` clearly targets a non-simulation runner."""
    if isinstance(recv, ast.Name):
        return recv.id in non_sim_aliases
    if isinstance(recv, ast.Attribute):
        root = _root_name_of_expr(recv)
        return root is not None and root in non_sim_aliases
    if isinstance(recv, ast.Call):
        func = recv.func
        if isinstance(func, ast.Name) and func.id in _NON_SIMULATION_RUN_RECEIVERS:
            return True
        if isinstance(func, ast.Attribute):
            if func.attr in _NON_SIMULATION_RUN_RECEIVERS:
                return True
            root = _root_name_of_expr(func)
            return root is not None and root in non_sim_aliases
    return False


def _collect_import_aliases(
    tree: ast.Module,
) -> tuple[set[str], set[str], frozenset[str]]:
    """Collect subprocess aliases, direct subprocess imports, and non-sim `.run` aliases."""
    subproc_aliases: set[str] = {"subprocess"}
    subproc_funcs: set[str] = set()
    non_sim_aliases: set[str] = set(_NON_SIMULATION_RUN_RECEIVERS)
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                local_name = alias.asname or alias.name.split(".", 1)[0]
                root_mod = alias.name.split(".", 1)[0]
                if alias.name == "subprocess":
                    subproc_aliases.add(local_name)
                if root_mod in _NON_SIMULATION_RUN_MODULES:
                    non_sim_aliases.add(local_name)
        elif isinstance(node, ast.ImportFrom):
            if node.module == "subprocess":
                for alias in node.names:
                    if alias.name in _SUBPROCESS_CALL_METHODS:
                        subproc_funcs.add(alias.asname or alias.name)
            elif node.module and node.module.split(".", 1)[0] in _NON_SIMULATION_RUN_MODULES:
                for alias in node.names:
                    if alias.name != "*":
                        non_sim_aliases.add(alias.asname or alias.name)
    return (subproc_aliases, subproc_funcs, frozenset(non_sim_aliases))


def _flatten_argv_expr(
    node: ast.AST,
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str] = frozenset(),
    depth: int = 0,
) -> list[ast.AST]:
    """Flatten a literal/concatenated/starred argv list expression."""
    if depth > 6:
        return [node]
    if isinstance(node, ast.Name) and node.id in env and node.id not in seen:
        return _flatten_argv_expr(
            env[node.id], env, seen=seen | {node.id}, depth=depth + 1
        )
    if isinstance(node, (ast.List, ast.Tuple)):
        out: list[ast.AST] = []
        for elt in node.elts:
            if isinstance(elt, ast.Starred):
                out.extend(_flatten_argv_expr(elt.value, env, seen=seen, depth=depth + 1))
            else:
                out.append(elt)
        return out
    if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Add):
        return _flatten_argv_expr(
            node.left, env, seen=seen, depth=depth + 1
        ) + _flatten_argv_expr(node.right, env, seen=seen, depth=depth + 1)
    return [node]


def _unwrap_path_conversion(node: ast.AST) -> ast.AST:
    """Unwrap ``str(x)``, ``os.fspath(x)``, or ``Path(x)`` around an executable expr."""
    cur = node
    for _ in range(3):
        if isinstance(cur, ast.Call) and cur.args:
            func = cur.func
            if isinstance(func, ast.Name) and func.id in {"str", "fspath", "Path"}:
                cur = cur.args[0]
                continue
            if (
                isinstance(func, ast.Attribute)
                and func.attr in {"fspath", "Path", "as_posix"}
            ):
                cur = cur.args[0] if func.attr != "as_posix" else func.value
                continue
        break
    return cur


def _detect_cli_from_helper_args(
    call: ast.Call,
    elems: list[ast.AST],
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str],
    depth: int,
) -> str:
    """Resolve CLI command from arguments to a SUEWS CLI helper call."""
    for arg in call.args:
        arg_s = _leading_str_constant(arg)
        if arg_s is not None:
            exe = _normalise_cli_executable(arg_s)
            if exe is not None:
                return exe
        if isinstance(arg, ast.Name) and arg.id in env and arg.id not in seen:
            resolved = _detect_suews_cli_in_expr(
                env[arg.id], env, seen=seen | {arg.id}, depth=depth + 1
            )
            if resolved is not None:
                return resolved
    if len(elems) >= 2 and _leading_str_constant(elems[1]) == "validate":
        return "suews-validate"
    return "suews"


def _detect_cli_from_call_expr(
    call: ast.Call,
    elems: list[ast.AST],
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str],
    depth: int,
) -> str | None:
    """Identify a SUEWS CLI executable returned by a helper call expression."""
    func = call.func
    fname = (
        func.id
        if isinstance(func, ast.Name)
        else (func.attr if isinstance(func, ast.Attribute) else "")
    )
    fl = fname.lower()
    result: str | None = None
    if fl == "which":
        arg_s = _leading_str_constant(call.args[0]) if call.args else None
        result = _normalise_cli_executable(arg_s) if arg_s is not None else None
    elif "suews_engine" in fl or fl in {"_locate_engine", "locate_engine"}:
        result = "suews-engine"
    elif "suews_validate" in fl:
        result = "suews-validate"
    elif "suews_mcp" in fl:
        result = "suews-mcp"
    elif "suews_cli" in fl or "entry_point" in fl or "suews_cmd" in fl:
        result = _detect_cli_from_helper_args(
            call, elems, env, seen=seen, depth=depth
        )
    return result


def _detect_cli_from_string(text: str) -> str | None:
    """Detect a SUEWS CLI or ``python -m supy...`` invocation in a command string."""
    tokens = text.split()
    if not tokens:
        return None
    exe = _normalise_cli_executable(tokens[0])
    if exe is not None:
        return exe
    for idx, tok in enumerate(tokens[:-1]):
        if tok == "-m" and (
            tokens[idx + 1] == "supy" or tokens[idx + 1].startswith("supy.")
        ):
            return PYTHON_M_SUPY_COMMAND
    return None


_CLI_NAME_SUBSTRINGS: tuple[tuple[str, str], ...] = (
    ("suews_engine", "suews-engine"),
    ("suews_validate", "suews-validate"),
    ("suews_mcp", "suews-mcp"),
    ("suews_convert", "suews-convert"),
    ("suews_schema", "suews-schema"),
    ("suews_inspect", "suews-inspect"),
)


def _detect_cli_from_name(
    first: ast.Name,
    elems: list[ast.AST],
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str],
    depth: int,
) -> str | None:
    """Detect a SUEWS CLI from a variable or fixture name in ``argv[0]``."""
    if first.id in env and first.id not in seen:
        resolved = _detect_suews_cli_in_expr(
            env[first.id], env, seen=seen | {first.id}, depth=depth + 1
        )
        if resolved is not None:
            return resolved
    low = first.id.lower()
    for needle, command in _CLI_NAME_SUBSTRINGS:
        if needle in low:
            return command
    if low in _ENGINE_BIN_VAR_NAMES or (
        low in _GENERIC_ENGINE_RUN_VARS
        and len(elems) >= 2
        and _leading_str_constant(elems[1]) == "run"
    ):
        return "suews-engine"
    if low in _SUEWS_EXE_VAR_NAMES:
        if len(elems) >= 2 and _leading_str_constant(elems[1]) == "validate":
            return "suews-validate"
        return "suews"
    return None


def _has_python_m_supy(elems: list[ast.AST]) -> bool:
    """Return True when argv elements include ``-m supy`` or ``-m supy.<...>``."""
    for idx, elem in enumerate(elems[:-1]):
        if _leading_str_constant(elem) == "-m":
            mod_str = _leading_str_constant(elems[idx + 1])
            if mod_str is not None and (
                mod_str == "supy" or mod_str.startswith("supy.")
            ):
                return True
    return False


def _detect_suews_cli_in_expr(
    expr: ast.AST,
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str] = frozenset(),
    depth: int = 0,
) -> str | None:
    """Return canonical SUEWS CLI command invoked by ``expr``, or ``None``."""
    if depth > 6:
        return None
    direct_str = _leading_str_constant(expr)
    if direct_str is not None:
        return _detect_cli_from_string(direct_str)

    elems = _flatten_argv_expr(expr, env, seen=seen, depth=depth + 1)
    if _has_python_m_supy(elems):
        return PYTHON_M_SUPY_COMMAND
    if not elems:
        return None

    first = _unwrap_path_conversion(elems[0])
    result: str | None = None
    first_str = _leading_str_constant(first)
    if first_str is not None:
        result = _normalise_cli_executable(first_str)
    elif isinstance(first, ast.BinOp) and isinstance(first.op, ast.Div):
        rhs = _leading_str_constant(first.right)
        result = _normalise_cli_executable(rhs) if rhs is not None else None
    elif isinstance(first, ast.Name):
        result = _detect_cli_from_name(
            first, elems, env, seen=seen, depth=depth
        )
    elif isinstance(first, ast.Call):
        result = _detect_cli_from_call_expr(
            first, elems, env, seen=seen, depth=depth + 1
        )
    return result


def _build_local_expr_env(
    fn: ast.FunctionDef | ast.AsyncFunctionDef,
    mod_env: Mapping[str, ast.AST],
) -> dict[str, ast.AST]:
    """Build a best-effort local variable -> expression map for a function body."""
    env: dict[str, ast.AST] = dict(mod_env)
    for stmt in fn.body:
        for node in ast.walk(stmt):
            if isinstance(node, ast.Assign):
                for target in node.targets:
                    if isinstance(target, ast.Name):
                        env[target.id] = node.value
            elif (
                isinstance(node, ast.AnnAssign)
                and isinstance(node.target, ast.Name)
                and node.value is not None
            ):
                env[node.target.id] = node.value
            elif (
                isinstance(node, ast.AugAssign)
                and isinstance(node.op, ast.Add)
                and isinstance(node.target, ast.Name)
                and node.target.id in env
            ):
                env[node.target.id] = ast.BinOp(
                    left=env[node.target.id], op=ast.Add(), right=node.value
                )
    return env


def _is_subprocess_call(
    node: ast.Call,
    subproc_aliases: set[str],
    subproc_funcs: set[str],
) -> bool:
    """Return True when ``node`` calls subprocess.{run,Popen,check_output,check_call}."""
    func = node.func
    if (
        isinstance(func, ast.Attribute)
        and func.attr in _SUBPROCESS_CALL_METHODS
        and isinstance(func.value, ast.Name)
        and func.value.id in subproc_aliases
    ):
        return True
    return isinstance(func, ast.Name) and func.id in subproc_funcs


def _extract_subprocess_cmd_arg(call: ast.Call) -> ast.AST | None:
    """Return the command expression passed to a ``subprocess.*`` call."""
    if call.args:
        return call.args[0]
    for kw in call.keywords:
        if kw.arg == "args":
            return kw.value
    return None


def _extract_forwarded_param_names(
    cmd_arg: ast.AST,
    env: Mapping[str, ast.AST],
    param_names: set[str],
) -> set[str]:
    """Return parameter names of a helper that flow into ``subprocess`` ``argv[0]``."""
    forwarded: set[str] = set()
    elems = _flatten_argv_expr(cmd_arg, env)
    if not elems:
        return forwarded
    first = _unwrap_path_conversion(elems[0])
    if isinstance(first, ast.Name) and first.id in param_names:
        forwarded.add(first.id)
    elif isinstance(first, ast.Call):
        for arg in first.args:
            unwrapped = _unwrap_path_conversion(arg)
            if isinstance(unwrapped, ast.Name) and unwrapped.id in param_names:
                forwarded.add(unwrapped.id)
    return forwarded


def _collect_helper_return_exprs(tree: ast.Module) -> dict[str, ast.AST]:
    """Return single-return expressions of non-test top-level helper functions."""
    return_exprs: dict[str, ast.AST] = {}
    for stmt in tree.body:
        if isinstance(
            stmt, (ast.FunctionDef, ast.AsyncFunctionDef)
        ) and not stmt.name.startswith("test_"):
            returns = [
                n.value
                for n in ast.walk(stmt)
                if isinstance(n, ast.Return) and n.value is not None
            ]
            if len(returns) == 1:
                return_exprs[stmt.name] = returns[0]
    return return_exprs


def _resolve_subprocess_cli(
    cmd_arg: ast.AST,
    env: Mapping[str, ast.AST],
    mod_env: Mapping[str, ast.AST],
    helper_return_exprs: Mapping[str, ast.AST],
) -> str | None:
    """Resolve the canonical SUEWS CLI invoked by ``cmd_arg``."""
    cli = _detect_suews_cli_in_expr(cmd_arg, env)
    if cli is not None:
        return cli
    if (
        isinstance(cmd_arg, ast.Call)
        and isinstance(cmd_arg.func, ast.Name)
        and cmd_arg.func.id in helper_return_exprs
    ):
        return _detect_suews_cli_in_expr(helper_return_exprs[cmd_arg.func.id], mod_env)
    return None


def _scan_single_helper(
    stmt: ast.FunctionDef | ast.AsyncFunctionDef,
    mod_env: Mapping[str, ast.AST],
    subproc_aliases: set[str],
    subproc_funcs: set[str],
    helper_return_exprs: Mapping[str, ast.AST],
) -> tuple[dict[str, int], set[str], set[str], list[ast.Call]]:
    """Inspect one module-level helper for direct CLI calls and forwarded params."""
    pos_args = [
        *(arg.arg for arg in stmt.args.posonlyargs),
        *(arg.arg for arg in stmt.args.args),
    ]
    param_pos = {name: idx for idx, name in enumerate(pos_args)}
    all_params = set(param_pos) | {arg.arg for arg in stmt.args.kwonlyargs}
    env = _build_local_expr_env(stmt, mod_env)
    clis: set[str] = set()
    forwarded: set[str] = set()
    calls: list[ast.Call] = []

    for body_stmt in stmt.body:
        for node in ast.walk(body_stmt):
            if not isinstance(node, ast.Call):
                continue
            if _is_subprocess_call(node, subproc_aliases, subproc_funcs):
                cmd_arg = _extract_subprocess_cmd_arg(node)
                if cmd_arg is None:
                    continue
                cli = _resolve_subprocess_cli(
                    cmd_arg, env, mod_env, helper_return_exprs
                )
                if cli is not None:
                    clis.add(cli)
                forwarded.update(
                    _extract_forwarded_param_names(cmd_arg, env, all_params)
                )
            elif isinstance(node.func, ast.Name):
                calls.append(node)
    return (param_pos, clis, forwarded, calls)


def _propagate_helper_clis(
    helper_clis: dict[str, set[str]],
    helper_calls: Mapping[str, list[ast.Call]],
) -> None:
    """Propagate SUEWS CLI sets transitively across intra-module helper calls."""
    changed = True
    while changed:
        changed = False
        for helper_name, calls in helper_calls.items():
            for call in calls:
                callee = call.func.id if isinstance(call.func, ast.Name) else ""
                if callee not in helper_clis:
                    continue
                before = len(helper_clis[helper_name])
                helper_clis[helper_name].update(helper_clis[callee])
                if len(helper_clis[helper_name]) > before:
                    changed = True


def _analyse_module_helpers(
    tree: ast.Module,
    mod_env: Mapping[str, ast.AST],
    subproc_aliases: set[str],
    subproc_funcs: set[str],
    helper_return_exprs: Mapping[str, ast.AST],
) -> tuple[
    dict[str, set[str]],
    dict[str, dict[str, int]],
    dict[str, set[str]],
]:
    """Analyse module-level non-test helpers for SUEWS CLI subprocess calls.

    Returns ``(helper_direct_clis, helper_param_positions, helper_forwarded_params)``.
    """
    helper_clis: dict[str, set[str]] = {}
    helper_calls: dict[str, list[ast.Call]] = {}
    helper_param_positions: dict[str, dict[str, int]] = {}
    helper_forwarded: dict[str, set[str]] = {}

    for stmt in tree.body:
        if isinstance(
            stmt, (ast.FunctionDef, ast.AsyncFunctionDef)
        ) and not stmt.name.startswith("test_"):
            param_pos, clis, forwarded, calls = _scan_single_helper(
                stmt, mod_env, subproc_aliases, subproc_funcs, helper_return_exprs
            )
            helper_param_positions[stmt.name] = param_pos
            helper_clis[stmt.name] = clis
            helper_forwarded[stmt.name] = forwarded
            helper_calls[stmt.name] = calls

    _propagate_helper_clis(helper_clis, helper_calls)
    return (helper_clis, helper_param_positions, helper_forwarded)


def _count_parametrize_nodes(
    decorators: list[ast.AST],
    mod_env: Mapping[str, ast.AST],
) -> int:
    """Return statically counted node expansion factor for ``@pytest.mark.parametrize``."""
    total = 1
    for dec in decorators:
        resolved = dec
        if isinstance(resolved, ast.Name) and resolved.id in mod_env:
            resolved = mod_env[resolved.id]
        if not isinstance(resolved, ast.Call):
            continue
        if _direct_pytest_marker_name(resolved) != "parametrize":
            continue
        argvalues: ast.AST | None = None
        if len(resolved.args) >= 2:
            argvalues = resolved.args[1]
        else:
            for kw in resolved.keywords:
                if kw.arg == "argvalues":
                    argvalues = kw.value
                    break
        if isinstance(argvalues, ast.Name) and argvalues.id in mod_env:
            argvalues = mod_env[argvalues.id]
        if isinstance(argvalues, (ast.List, ast.Tuple, ast.Set)) and argvalues.elts:
            total *= len(argvalues.elts)
        elif (
            isinstance(argvalues, ast.Call)
            and isinstance(argvalues.func, ast.Name)
            and argvalues.func.id == "range"
            and 1 <= len(argvalues.args) <= 3
            and all(
                isinstance(a, ast.Constant)
                and isinstance(a.value, int)
                and not isinstance(a.value, bool)
                for a in argvalues.args
            )
        ):
            r_args = [int(a.value) for a in argvalues.args]  # type: ignore[union-attr]
            if len(r_args) < 3 or r_args[2] != 0:
                length = len(range(*r_args))
                if length > 0:
                    total *= length
    return total


def _count_param_simulation_calls(
    fn: ast.FunctionDef | ast.AsyncFunctionDef,
    non_sim_aliases: frozenset[str],
) -> int:
    """Count direct ``from_sample_data(...)`` and simulation ``.run(...)`` calls in ``fn``."""
    count = 0
    for stmt in fn.body:
        for node in ast.walk(stmt):
            if not isinstance(node, ast.Call) or not isinstance(
                node.func, ast.Attribute
            ):
                continue
            if node.func.attr == "from_sample_data" or (
                node.func.attr == "run"
                and not _is_non_simulation_run_receiver(
                    node.func.value, non_sim_aliases
                )
            ):
                count += 1
    return count


def _resolve_forwarded_helper_clis(
    call: ast.Call,
    forwarded_params: set[str],
    param_pos: Mapping[str, int],
    env: Mapping[str, ast.AST],
) -> set[str]:
    """Detect SUEWS CLI commands passed through forwarded helper parameters."""
    found: set[str] = set()
    kw_map = {kw.arg: kw.value for kw in call.keywords if kw.arg is not None}
    for param_name in forwarded_params:
        idx = param_pos.get(param_name)
        arg_expr = (
            call.args[idx]
            if idx is not None and idx < len(call.args)
            else kw_map.get(param_name)
        )
        if arg_expr is not None:
            cli = _detect_suews_cli_in_expr(arg_expr, env)
            if cli is not None:
                found.add(cli)
    return found


def _collect_test_subprocess_clis(
    fn: ast.FunctionDef | ast.AsyncFunctionDef,
    mod_env: Mapping[str, ast.AST],
    subproc_aliases: set[str],
    subproc_funcs: set[str],
    helper_clis: Mapping[str, set[str]],
    helper_param_positions: Mapping[str, dict[str, int]],
    helper_forwarded: Mapping[str, set[str]],
    helper_return_exprs: Mapping[str, ast.AST],
) -> set[str]:
    """Collect canonical SUEWS CLI commands invoked via subprocess by ``fn``."""
    env = _build_local_expr_env(fn, mod_env)
    found: set[str] = set()
    for stmt in fn.body:
        for node in ast.walk(stmt):
            if not isinstance(node, ast.Call):
                continue
            if _is_subprocess_call(node, subproc_aliases, subproc_funcs):
                cmd_arg = _extract_subprocess_cmd_arg(node)
                if cmd_arg is not None:
                    cli = _resolve_subprocess_cli(
                        cmd_arg, env, mod_env, helper_return_exprs
                    )
                    if cli is not None:
                        found.add(cli)
            elif isinstance(node.func, ast.Name) and node.func.id in helper_clis:
                callee = node.func.id
                found.update(helper_clis[callee])
                forwarded = helper_forwarded.get(callee)
                if forwarded:
                    found.update(
                        _resolve_forwarded_helper_clis(
                            node,
                            forwarded,
                            helper_param_positions.get(callee, {}),
                            env,
                        )
                    )
    return found


def inspect_file_ast_guards(
    rel_posix: str,
    source: str,
    *,
    extra_markers: Mapping[str, tuple[str, ...]] = LOCKED_PR_FILE_EXTRA_MARKERS,
) -> AstGuardsSnapshot | None:
    """Inspect a single test file for parametrised-simulation and subprocess ``e2e`` guards.

    Parameters
    ----------
    rel_posix : str
        Repository-relative POSIX path of the test file.
    source : str
        Python source text of the test file.
    extra_markers : Mapping[str, tuple[str, ...]], optional
        Runtime-attached file-level extra markers (defaults to
        ``LOCKED_PR_FILE_EXTRA_MARKERS``).

    Returns
    -------
    AstGuardsSnapshot | None
        Snapshot for this file, or ``None`` if ``source`` has a ``SyntaxError``.
    """
    try:
        tree = ast.parse(source)
    except SyntaxError:
        return None

    mod_env = _build_single_assign_env(tree)
    mod_markers, _ = _extract_scope_pytestmark(tree.body)
    file_e2e = ("e2e" in mod_markers) or ("e2e" in extra_markers.get(rel_posix, ()))
    file_param = "parametrize" in mod_markers
    subproc_aliases, subproc_funcs, non_sim_aliases = _collect_import_aliases(tree)
    helper_return_exprs = _collect_helper_return_exprs(tree)
    helper_clis, helper_param_positions, helper_forwarded = _analyse_module_helpers(
        tree, mod_env, subproc_aliases, subproc_funcs, helper_return_exprs
    )
    snapshot = AstGuardsSnapshot()

    def _visit_function(
        fn: ast.FunctionDef | ast.AsyncFunctionDef,
        class_chain: tuple[str, ...],
        enclosing_decs: list[ast.AST],
        inherited_param: bool,
        inherited_e2e: bool,
    ) -> None:
        if not fn.name.startswith("test_") or any(
            _is_fixture_decorator(dec) for dec in fn.decorator_list
        ):
            return
        fn_marks: set[str] = set()
        for dec in fn.decorator_list:
            d_names, _ = _inspect_decorator_expr(dec, mod_env)
            fn_marks.update(d_names)
        is_param = file_param or inherited_param or ("parametrize" in fn_marks)
        is_e2e = file_e2e or inherited_e2e or ("e2e" in fn_marks)
        node_id = "::".join((rel_posix, *class_chain, fn.name))
        if is_param:
            sim_calls = _count_param_simulation_calls(fn, non_sim_aliases)
            if sim_calls > 0:
                snapshot.parametrized_simulations[node_id] = sim_calls
        clis = _collect_test_subprocess_clis(
            fn,
            mod_env,
            subproc_aliases,
            subproc_funcs,
            helper_clis,
            helper_param_positions,
            helper_forwarded,
            helper_return_exprs,
        )
        if not clis:
            return
        if not is_e2e:
            snapshot.unmarked_subprocess_e2e[node_id] = sorted(clis)
        else:
            nodes = _count_parametrize_nodes(
                [*enclosing_decs, *fn.decorator_list], mod_env
            )
            for cli in clis:
                snapshot.e2e_command_nodes[cli] = (
                    snapshot.e2e_command_nodes.get(cli, 0) + nodes
                )

    def _visit_class(
        cls: ast.ClassDef,
        class_chain: tuple[str, ...],
        enclosing_decs: list[ast.AST],
        inherited_param: bool,
        inherited_e2e: bool,
    ) -> None:
        cls_marks: set[str] = set()
        for dec in cls.decorator_list:
            d_names, _ = _inspect_decorator_expr(dec, mod_env)
            cls_marks.update(d_names)
        cls_pm, _ = _extract_scope_pytestmark(cls.body)
        cls_marks.update(cls_pm)
        next_chain = (*class_chain, cls.name)
        next_decs = [*enclosing_decs, *cls.decorator_list]
        next_param = inherited_param or ("parametrize" in cls_marks)
        next_e2e = inherited_e2e or ("e2e" in cls_marks)
        for item in cls.body:
            if isinstance(item, (ast.FunctionDef, ast.AsyncFunctionDef)):
                _visit_function(item, next_chain, next_decs, next_param, next_e2e)
            elif isinstance(item, ast.ClassDef):
                _visit_class(item, next_chain, next_decs, next_param, next_e2e)

    for stmt in tree.body:
        if isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef)):
            _visit_function(stmt, (), [], False, False)
        elif isinstance(stmt, ast.ClassDef):
            _visit_class(stmt, (), [], False, False)
    return snapshot


def scan_repo_ast_guards(
    repo_root: Path,
    test_files: list[Path],
    *,
    extra_markers: Mapping[str, tuple[str, ...]] = LOCKED_PR_FILE_EXTRA_MARKERS,
) -> tuple[AstGuardsSnapshot, frozenset[str]]:
    """Run static AST guards across ``test_files`` and aggregate results."""
    combined = AstGuardsSnapshot()
    skipped: set[str] = set()
    for path in test_files:
        rel_posix = path.relative_to(repo_root).as_posix()
        try:
            target = path.resolve(strict=True)
            if not target.is_relative_to(repo_root):
                skipped.add(rel_posix)
                continue
            source = path.read_text(encoding="utf-8")
        except (OSError, RuntimeError, UnicodeError):
            skipped.add(rel_posix)
            continue
        file_snap = inspect_file_ast_guards(
            rel_posix, source, extra_markers=extra_markers
        )
        if file_snap is None:
            skipped.add(rel_posix)
            continue
        combined.parametrized_simulations.update(file_snap.parametrized_simulations)
        combined.unmarked_subprocess_e2e.update(file_snap.unmarked_subprocess_e2e)
        for cmd, count in file_snap.e2e_command_nodes.items():
            combined.e2e_command_nodes[cmd] = (
                combined.e2e_command_nodes.get(cmd, 0) + count
            )

    combined.parametrized_simulations = dict(
        sorted(combined.parametrized_simulations.items())
    )
    combined.unmarked_subprocess_e2e = dict(
        sorted(combined.unmarked_subprocess_e2e.items())
    )
    combined.e2e_command_nodes = dict(sorted(combined.e2e_command_nodes.items()))
    return (combined, frozenset(skipped))


def _empty_ast_guards_baseline(
    *, default_cap: int = DEFAULT_E2E_COMMAND_NODE_CAP
) -> dict[str, object]:
    """Return an empty validated baseline payload."""
    return {
        "schema_version": AST_GUARDS_BASELINE_SCHEMA_VERSION,
        "e2e_default_cap": default_cap,
        "parametrized_simulations": {},
        "unmarked_subprocess_e2e": {},
        "e2e_command_caps": {},
    }


def _is_positive_int(value: object) -> bool:
    """Return True when ``value`` is a positive ``int`` (and not a ``bool``)."""
    return isinstance(value, int) and not isinstance(value, bool) and value > 0


def _validate_baseline_sections(
    raw: Mapping[str, object],
    *,
    default_cap: int,
) -> tuple[dict[str, int], dict[str, list[str]], dict[str, int], list[str]]:
    """Validate the three ratchet maps inside an AST guards baseline payload."""
    errors: list[str] = []
    param_clean: dict[str, int] = {}
    unmarked_clean: dict[str, list[str]] = {}
    caps_clean: dict[str, int] = {}

    param_raw = raw.get("parametrized_simulations", {})
    if not isinstance(param_raw, dict):
        errors.append("baseline parametrized_simulations must be an object")
    else:
        for node_id, count in sorted(param_raw.items()):
            if isinstance(node_id, str) and _is_positive_int(count):
                param_clean[node_id] = int(count)
            else:
                errors.append(
                    f"invalid parametrized_simulations entry: {node_id!r}: {count!r}"
                )

    unmarked_raw = raw.get("unmarked_subprocess_e2e", {})
    if not isinstance(unmarked_raw, dict):
        errors.append("baseline unmarked_subprocess_e2e must be an object")
    else:
        for node_id, cmds in sorted(unmarked_raw.items()):
            if (
                isinstance(node_id, str)
                and isinstance(cmds, list)
                and cmds
                and all(
                    isinstance(c, str) and c in ALL_SUEWS_CLI_COMMANDS for c in cmds
                )
            ):
                unmarked_clean[node_id] = sorted(set(cmds))
            else:
                errors.append(
                    f"invalid unmarked_subprocess_e2e entry: {node_id!r}: {cmds!r}"
                )

    caps_raw = raw.get("e2e_command_caps", {})
    if not isinstance(caps_raw, dict):
        errors.append("baseline e2e_command_caps must be an object")
    else:
        for cmd, cap in sorted(caps_raw.items()):
            if (
                isinstance(cmd, str)
                and cmd in ALL_SUEWS_CLI_COMMANDS
                and _is_positive_int(cap)
                and int(cap) > default_cap
            ):
                caps_clean[cmd] = int(cap)
            else:
                errors.append(
                    f"invalid e2e_command_caps entry: {cmd!r}: {cap!r} "
                    f"(must be a recognised command with cap > {default_cap})"
                )

    return (param_clean, unmarked_clean, caps_clean, errors)


def load_ast_guards_baseline(
    baseline_path: Path,
    *,
    default_cap: int = DEFAULT_E2E_COMMAND_NODE_CAP,
) -> tuple[dict[str, object] | None, list[str]]:
    """Load and validate an AST guards baseline JSON file."""
    errors: list[str] = []
    try:
        raw = json.loads(baseline_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        return (None, [f"unable to load AST guards baseline {baseline_path.name}: {exc}"])

    if not isinstance(raw, dict):
        return (None, ["AST guards baseline must be a JSON object"])

    unknown_keys = sorted(set(raw) - _BASELINE_ALLOWED_KEYS)
    if unknown_keys:
        errors.append(f"unknown baseline keys: {', '.join(unknown_keys)}")

    version = raw.get("schema_version")
    if version != AST_GUARDS_BASELINE_SCHEMA_VERSION:
        errors.append(
            f"unsupported baseline schema_version {version!r} "
            f"(expected {AST_GUARDS_BASELINE_SCHEMA_VERSION})"
        )

    recorded_cap = raw.get("e2e_default_cap", default_cap)
    if (
        not isinstance(recorded_cap, int)
        or isinstance(recorded_cap, bool)
        or recorded_cap != default_cap
    ):
        errors.append(
            f"baseline e2e_default_cap {recorded_cap!r} does not match "
            f"configured cap {default_cap}"
        )

    param_clean, unmarked_clean, caps_clean, section_errors = _validate_baseline_sections(
        raw, default_cap=default_cap
    )
    errors.extend(section_errors)
    if errors:
        return (None, errors)

    return (
        {
            "schema_version": AST_GUARDS_BASELINE_SCHEMA_VERSION,
            "e2e_default_cap": default_cap,
            "parametrized_simulations": param_clean,
            "unmarked_subprocess_e2e": unmarked_clean,
            "e2e_command_caps": caps_clean,
        },
        [],
    )


def write_ast_guards_baseline(
    baseline_path: Path,
    snapshot: AstGuardsSnapshot,
    *,
    default_cap: int = DEFAULT_E2E_COMMAND_NODE_CAP,
) -> None:
    """Write ``snapshot`` as a canonical monotonic AST guards baseline JSON file."""
    caps_above_default = {
        cmd: count
        for cmd, count in sorted(snapshot.e2e_command_nodes.items())
        if count > default_cap
    }
    payload = {
        "schema_version": AST_GUARDS_BASELINE_SCHEMA_VERSION,
        "description": (
            "Monotonic legacy baseline for static AST guards in "
            "scripts/lint/check_test_markers.py (gh#2349 W1-8). "
            "Entries may only decrease or be removed."
        ),
        "e2e_default_cap": default_cap,
        "parametrized_simulations": dict(
            sorted(snapshot.parametrized_simulations.items())
        ),
        "unmarked_subprocess_e2e": {
            k: sorted(set(v))
            for k, v in sorted(snapshot.unmarked_subprocess_e2e.items())
        },
        "e2e_command_caps": caps_above_default,
    }
    baseline_path.parent.mkdir(parents=True, exist_ok=True)
    baseline_path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def _node_belongs_to_skipped(node_id: str, skipped_files: frozenset[str]) -> bool:
    """Return True if ``node_id`` belongs to a file in ``skipped_files``."""
    return node_id.split("::", 1)[0] in skipped_files


def compare_ast_guards_baseline(
    snapshot: AstGuardsSnapshot,
    baseline: Mapping[str, object],
    *,
    default_cap: int = DEFAULT_E2E_COMMAND_NODE_CAP,
    skipped_files: frozenset[str] = frozenset(),
) -> tuple[list[str], list[str]]:
    """Compare live ``snapshot`` against monotonic ``baseline``.

    Returns
    -------
    tuple[list[str], list[str]]
        ``(violations, stale_entries)`` where ``violations`` are new or
        increased AST guard breaches and ``stale_entries`` are baseline entries
        whose legacy debt has decreased or been eliminated.
    """
    violations: list[str] = []
    stale: list[str] = []

    base_param: Mapping[str, int] = baseline.get("parametrized_simulations", {})  # type: ignore[assignment]
    for node_id in sorted(set(snapshot.parametrized_simulations) | set(base_param)):
        now = snapshot.parametrized_simulations.get(node_id, 0)
        then = base_param.get(node_id, 0)
        if now > then:
            violations.append(
                f"parametrized-simulation: {node_id} calls from_sample_data()/.run() "
                f"directly inside @pytest.mark.parametrize ({then} -> {now}); "
                "inject a session/module fixture instead"
            )
        elif now < then and not _node_belongs_to_skipped(node_id, skipped_files):
            stale.append(
                f"parametrized-simulation baseline stale: {node_id} ({then} -> {now})"
            )

    base_unmarked: Mapping[str, list[str]] = baseline.get("unmarked_subprocess_e2e", {})  # type: ignore[assignment]
    for node_id in sorted(set(snapshot.unmarked_subprocess_e2e) | set(base_unmarked)):
        now_cmds = set(snapshot.unmarked_subprocess_e2e.get(node_id, ()))
        then_cmds = set(base_unmarked.get(node_id, ()))
        added = sorted(now_cmds - then_cmds)
        removed = sorted(then_cmds - now_cmds)
        if added:
            violations.append(
                f"missing-e2e-marker: {node_id} invokes SUEWS CLI subprocess "
                f"({', '.join(added)}) without @pytest.mark.e2e"
            )
        if removed and not _node_belongs_to_skipped(node_id, skipped_files):
            stale.append(
                f"unmarked-subprocess-e2e baseline stale: {node_id} "
                f"(removed {', '.join(removed)})"
            )

    base_caps: Mapping[str, int] = baseline.get("e2e_command_caps", {})  # type: ignore[assignment]
    for cmd in sorted(set(snapshot.e2e_command_nodes) | set(base_caps)):
        now_nodes = snapshot.e2e_command_nodes.get(cmd, 0)
        pinned = base_caps.get(cmd)
        effective_cap = pinned if pinned is not None else default_cap
        if now_nodes > effective_cap:
            violations.append(
                f"e2e-subprocess-cap: `{cmd}` has {now_nodes} @pytest.mark.e2e "
                f"subprocess test node(s), exceeding cap {effective_cap} "
                f"(default cap {default_cap})"
            )
        elif pinned is not None and not skipped_files:
            if now_nodes <= default_cap:
                stale.append(
                    f"e2e-subprocess-cap baseline stale: `{cmd}` is now within "
                    f"default cap ({now_nodes} <= {default_cap}; remove baseline {pinned})"
                )
            elif now_nodes < pinned:
                stale.append(
                    f"e2e-subprocess-cap baseline stale: `{cmd}` decreased "
                    f"({pinned} -> {now_nodes})"
                )

    return (violations, stale)


def _resolve_ast_baseline(
    repo_root: Path,
    explicit_baseline: Path | None,
    *,
    default_cap: int = DEFAULT_E2E_COMMAND_NODE_CAP,
) -> tuple[dict[str, object] | None, list[str]]:
    """Resolve and load the AST guards baseline for ``repo_root``."""
    if explicit_baseline is not None:
        if not explicit_baseline.is_file():
            return (
                None,
                [
                    "AST guards baseline not found at "
                    f"{explicit_baseline.as_posix()}"
                ],
            )
        return load_ast_guards_baseline(explicit_baseline, default_cap=default_cap)

    default_path = repo_root / AST_GUARDS_BASELINE_PATH
    link_error = _check_baseline_symlink(default_path, repo_root)
    if link_error is not None:
        return (None, [link_error])
    if default_path.is_file():
        return load_ast_guards_baseline(default_path, default_cap=default_cap)
    if (repo_root / "scripts" / "lint").is_dir():
        return (
            None,
            [
                "missing AST guards baseline "
                f"{AST_GUARDS_BASELINE_PATH.as_posix()}; regenerate with "
                "`python scripts/lint/check_test_markers.py --write-ast-baseline`"
            ],
        )
    return (_empty_ast_guards_baseline(default_cap=default_cap), [])


def _check_baseline_symlink(default_path: Path, repo_root: Path) -> str | None:
    """Reject a broken or escaping symlink at the default baseline path."""
    if not _is_link_or_junction(default_path):
        return None
    try:
        target = default_path.resolve(strict=True)
    except (OSError, RuntimeError) as exc:
        return f"invalid AST guards baseline symlink: {exc}"
    if not target.is_relative_to(repo_root):
        return (
            "AST guards baseline symlink escapes repository root: "
            f"{AST_GUARDS_BASELINE_PATH.as_posix()}"
        )
    return None


def _print_error_block(header: str, items: list[str], footer: str | None = None) -> bool:
    """Print an ASCII error section to stderr when ``items`` is non-empty."""
    if not items:
        return False
    print(_to_ascii(header), file=sys.stderr)
    for item in items:
        print(_to_ascii(f"  - {item}"), file=sys.stderr)
    if footer is not None:
        print(_to_ascii(footer), file=sys.stderr)
    return True


def _emit_lint_failures(
    *,
    read_errors: list[str],
    violations: list[str],
    lane_errors: list[str],
    orphans: list[str],
    baseline_errors: list[str],
    ast_violations: list[str],
    ast_stale: list[str],
) -> bool:
    """Emit all collected diagnostic sections to stderr; return True if any failed."""
    blocks = (
        (
            "[X] gh#2349 marker lint: unable to read test files as UTF-8:",
            read_errors,
            None,
        ),
        (
            "[X] gh#2349 marker lint: test files violating the single primary-axis contract:",
            violations,
            (
                "Declare exactly one primary axis at module level "
                "(`pytestmark = pytest.mark.<tooling|unit|native|physics>`), "
                "or auto-apply via a conftest.py hook."
            ),
        ),
        (
            "[X] gh#2349 lane-coverage lint: invalid Makefile TEST_DEFAULT_PATHS "
            "or test-tree layout:",
            lane_errors,
            None,
        ),
        (
            "[X] gh#2349 lane-coverage lint: orphan test files outside "
            "Makefile TEST_DEFAULT_PATHS and peripheral surfaces:",
            orphans,
            (
                "Move each file into a directory listed in Makefile "
                "TEST_DEFAULT_PATHS or a documented peripheral surface "
                f"({', '.join(PERIPHERAL_TEST_DIRS)}), or register the lane "
                "explicitly."
            ),
        ),
        (
            "[X] gh#2349 AST guard lint: invalid or missing AST guards baseline:",
            baseline_errors,
            None,
        ),
        (
            "[X] gh#2349 AST guard lint: new or increased AST guard violations:",
            ast_violations,
            None,
        ),
        (
            "[X] gh#2349 AST guard lint: baseline entries are stale (debt went down):",
            ast_stale,
            (
                "Lower the baseline in this diff with: "
                "python scripts/lint/check_test_markers.py --write-ast-baseline"
            ),
        ),
    )
    failed = False
    for header, items, footer in blocks:
        if _print_error_block(header, items, footer):
            failed = True
    return failed


def _write_baseline_mode(
    repo_root: Path,
    args: argparse.Namespace,
    ast_snapshot: AstGuardsSnapshot,
    *,
    has_parse_errors: bool,
) -> int:
    """Execute ``--write-ast-baseline`` mode and return exit code."""
    if has_parse_errors:
        print(
            "[X] gh#2349 AST guard lint: cannot write baseline while test "
            "files fail to read or parse.",
            file=sys.stderr,
        )
        return 1
    target_baseline = (
        args.ast_baseline
        if args.ast_baseline is not None
        else repo_root / AST_GUARDS_BASELINE_PATH
    )
    write_ast_guards_baseline(target_baseline, ast_snapshot, default_cap=args.e2e_cap)
    overrides = sum(
        1 for n in ast_snapshot.e2e_command_nodes.values() if n > args.e2e_cap
    )
    print(
        _to_ascii(
            f"[OK] wrote AST guards baseline to {target_baseline.as_posix()}: "
            f"{len(ast_snapshot.parametrized_simulations)} parametrised-sim "
            f"function(s), {len(ast_snapshot.unmarked_subprocess_e2e)} "
            f"unmarked e2e test(s), {overrides} command cap override(s)."
        )
    )
    return 0


def _parse_cli_args(argv: list[str]) -> argparse.Namespace:
    """Parse CLI arguments while keeping positional ``repo_root`` backward-compatible."""
    parser = argparse.ArgumentParser(
        prog=argv[0] if argv else "check_test_markers.py",
        description=(
            "Static lint for pytest dependency-axis markers, lane coverage, "
            "and AST governance guards (gh#1300, gh#2349)."
        ),
    )
    parser.add_argument(
        "repo_root",
        nargs="?",
        default=None,
        help="Repository root directory (defaults to current working directory).",
    )
    parser.add_argument(
        "--ast-baseline",
        type=Path,
        default=None,
        help=(
            "Path to the AST guards baseline JSON file "
            f"(defaults to {AST_GUARDS_BASELINE_PATH.as_posix()})."
        ),
    )
    parser.add_argument(
        "--write-ast-baseline",
        action="store_true",
        help="Rewrite the AST guards baseline JSON from the current tree and exit 0.",
    )
    parser.add_argument(
        "--e2e-cap",
        type=int,
        default=DEFAULT_E2E_COMMAND_NODE_CAP,
        help=(
            "Default per-command cap on @pytest.mark.e2e subprocess test nodes "
            f"(default {DEFAULT_E2E_COMMAND_NODE_CAP})."
        ),
    )
    return parser.parse_args(argv[1:])


def _evaluate_ast_guards(
    repo_root: Path,
    args: argparse.Namespace,
    ast_snapshot: AstGuardsSnapshot,
    ast_skipped: frozenset[str],
) -> tuple[list[str], list[str], list[str]]:
    """Resolve the AST guards baseline and return ``(baseline_errors, violations, stale)``."""
    baseline, baseline_errors = _resolve_ast_baseline(
        repo_root, args.ast_baseline, default_cap=args.e2e_cap
    )
    if baseline is None:
        return (baseline_errors, [], [])
    ast_violations, ast_stale = compare_ast_guards_baseline(
        ast_snapshot,
        baseline,
        default_cap=args.e2e_cap,
        skipped_files=ast_skipped,
    )
    return (baseline_errors, ast_violations, ast_stale)


def main(argv: list[str]) -> int:
    """Run marker-axis, lane-coverage, and static AST guard checks on a repo tree.

    Parameters
    ----------
    argv : list[str]
        Command-line arguments; optional positional ``repo_root`` sets the
        repository root directory (defaulting to the current working directory).

    Returns
    -------
    int
        0 when all checks pass, 1 otherwise.
    """
    args = _parse_cli_args(argv)
    if args.e2e_cap < 0:
        print("[X] --e2e-cap must be non-negative", file=sys.stderr)
        return 1

    repo_root = (
        Path(args.repo_root).resolve()
        if args.repo_root is not None
        else Path.cwd().resolve()
    )
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
    ast_snapshot, ast_skipped = scan_repo_ast_guards(repo_root, test_files)

    if args.write_ast_baseline:
        return _write_baseline_mode(
            repo_root,
            args,
            ast_snapshot,
            has_parse_errors=bool(read_errors or ast_skipped),
        )

    ast_checks = _evaluate_ast_guards(repo_root, args, ast_snapshot, ast_skipped)

    if _emit_lint_failures(
        read_errors=read_errors,
        violations=violations,
        lane_errors=lane_errors,
        orphans=orphans,
        baseline_errors=ast_checks[0],
        ast_violations=ast_checks[1],
        ast_stale=ast_checks[2],
    ):
        return 1

    print(
        f"[OK] {checked} test files satisfy the single-axis marker contract "
        f"({locked_count} locked open-PR compatibility files); "
        f"{auto_applied} files pick up a marker via conftest auto-apply; "
        f"all {len(test_files)} test files are covered by Makefile "
        "TEST_DEFAULT_PATHS or peripheral surfaces; "
        "AST guards match the baseline "
        f"({len(ast_snapshot.parametrized_simulations)} legacy parametrised-sim "
        f"function(s), {len(ast_snapshot.unmarked_subprocess_e2e)} legacy unmarked "
        f"e2e test(s), {sum(ast_snapshot.e2e_command_nodes.values())} e2e subprocess "
        "test node(s))."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
