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
import shlex
import stat
import subprocess
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
    "contextvars",
})
_NON_SIMULATION_RUN_CLASSES: frozenset[str] = frozenset({
    "TextTestRunner",
    "CliRunner",
    "Runner",
    "Context",
    "Profile",
    "Pdb",
    "Bdb",
    "scheduler",
})
_PYTHON_EXE_RE = re.compile(r"^(?:python(?:\d+(?:\.\d+)*)?|pypy3?|py)(?:\.exe)?$")
_PYTHON_EXE_VAR_TOKENS: tuple[str, ...] = (
    "python",
    "py_exe",
    "env_exe",
    "executable",
)
_PYTHON_TWO_ARG_FLAGS: frozenset[str] = frozenset({"-W", "-X"})
_MAX_BRANCH_BINDINGS: int = 64
_MAX_EXPANDED_CASES: int = 2048
_BASELINE_ALLOWED_KEYS: frozenset[str] = frozenset({
    "schema_version",
    "description",
    "e2e_default_cap",
    "parametrized_simulations",
    "unmarked_subprocess_e2e",
    "e2e_command_caps",
    "unresolvable_e2e_parametrize",
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
_DARWIN_SYSTEM_LINK_ROOTS: frozenset[Path] = frozenset({
    Path("/tmp"),
    Path("/var"),
    Path("/etc"),
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
        unresolvable_e2e_parametrize: dict[str, list[str]] | None = None,
        subprocess_analysis_overflow: dict[str, list[str]] | None = None,
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
        self.unresolvable_e2e_parametrize: dict[str, list[str]] = (
            {k: list(v) for k, v in unresolvable_e2e_parametrize.items()}
            if unresolvable_e2e_parametrize
            else {}
        )
        self.subprocess_analysis_overflow: dict[str, list[str]] = (
            {k: list(v) for k, v in subprocess_analysis_overflow.items()}
            if subprocess_analysis_overflow
            else {}
        )

    __hash__ = None  # Mutable snapshot mappings are unhashable.

    def __eq__(self, other: object) -> bool:
        """Compare two snapshots by their guard mappings."""
        if not isinstance(other, AstGuardsSnapshot):
            return NotImplemented
        return (
            self.parametrized_simulations == other.parametrized_simulations
            and self.unmarked_subprocess_e2e == other.unmarked_subprocess_e2e
            and self.e2e_command_nodes == other.e2e_command_nodes
            and self.unresolvable_e2e_parametrize
            == other.unresolvable_e2e_parametrize
            and self.subprocess_analysis_overflow
            == other.subprocess_analysis_overflow
        )

    def __repr__(self) -> str:
        """Return a readable representation of the snapshot."""
        return (
            "AstGuardsSnapshot("
            f"parametrized_simulations={self.parametrized_simulations!r}, "
            f"unmarked_subprocess_e2e={self.unmarked_subprocess_e2e!r}, "
            f"e2e_command_nodes={self.e2e_command_nodes!r}, "
            f"unresolvable_e2e_parametrize={self.unresolvable_e2e_parametrize!r}, "
            f"subprocess_analysis_overflow={self.subprocess_analysis_overflow!r})"
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


def _collect_mutated_ast_names(tree: ast.Module) -> set[str]:
    """Return variable names mutated, deleted, or reassigned outside top-level decls."""
    top_stmts = set(tree.body)
    mutated: set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Assign) and node not in top_stmts:
            for target in node.targets:
                mutated.update(_iter_assigned_names(target))
        elif (
            isinstance(node, ast.AnnAssign) and node not in top_stmts
        ) or isinstance(node, ast.AugAssign):
            mutated.update(_iter_assigned_names(node.target))
        elif isinstance(node, ast.Delete):
            for target in node.targets:
                mutated.update(_iter_assigned_names(target))
                if isinstance(target, ast.Subscript) and isinstance(
                    target.value, ast.Name
                ):
                    mutated.add(target.value.id)
        elif (
            isinstance(node, ast.Subscript)
            and isinstance(node.ctx, ast.Store)
            and isinstance(node.value, ast.Name)
        ):
            mutated.add(node.value.id)
        elif (
            isinstance(node, ast.Call)
            and isinstance(node.func, ast.Attribute)
            and isinstance(node.func.value, ast.Name)
            and node.func.attr in _MUTATING_LIST_METHODS
        ):
            mutated.add(node.func.value.id)
    return mutated


def _build_ast_value_env(tree: ast.Module) -> dict[str, ast.AST]:
    """Map single-assigned, unmutated top-level constants to their RHS expressions.

    Unlike ``_build_single_assign_env``, ordinary read-only references in
    ``subprocess.run(ARGV)`` or ``@pytest.mark.parametrize('x', CASES)`` are
    retained rather than treated as escapes.
    """
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
    mutated = _collect_mutated_ast_names(tree)
    return {
        name: values[name]
        for name, count in counts.items()
        if count == 1 and name not in mutated
    }


def _extract_scope_pytestmark_exprs(stmts: list[ast.stmt]) -> list[ast.AST]:
    """Return marker AST expressions declared by ``pytestmark`` in ``stmts``."""
    exprs: list[ast.AST] = []
    for stmt in stmts:
        if not isinstance(stmt, (ast.Assign, ast.AnnAssign)):
            continue
        if not _stmt_targets_name(stmt, "pytestmark") or stmt.value is None:
            continue
        if isinstance(stmt.value, (ast.List, ast.Tuple)):
            exprs.extend(stmt.value.elts)
        else:
            exprs.append(stmt.value)
    return exprs


def _is_non_sim_constructor_call(
    node: ast.AST,
    non_sim_mods: frozenset[str],
    non_sim_classes: frozenset[str],
    shadowed: frozenset[str],
) -> bool:
    """Return True when ``node`` instantiates a known non-simulation runner class."""
    if not isinstance(node, ast.Call):
        return False
    func = node.func
    if isinstance(func, ast.Name):
        return func.id in non_sim_classes and func.id not in shadowed
    if isinstance(func, ast.Attribute):
        root = _root_name_of_expr(func)
        return (
            func.attr in _NON_SIMULATION_RUN_CLASSES
            and root is not None
            and root in non_sim_mods
            and root not in shadowed
        )
    return False


def _is_non_simulation_run_receiver(
    recv: ast.AST,
    non_sim_mods: frozenset[str],
    non_sim_classes: frozenset[str],
    local_non_sim_vars: frozenset[str],
    shadowed: frozenset[str],
) -> bool:
    """Return True only when ``<recv>.run(...)`` is provably bound to a non-sim runner."""
    if isinstance(recv, ast.Name):
        if recv.id in local_non_sim_vars:
            return True
        return recv.id in non_sim_mods and recv.id not in shadowed
    if isinstance(recv, ast.Attribute):
        root = _root_name_of_expr(recv)
        return (
            root is not None
            and root in non_sim_mods
            and root not in shadowed
        )
    return _is_non_sim_constructor_call(
        recv, non_sim_mods, non_sim_classes, shadowed
    )


def _collect_subprocess_imports(
    tree: ast.Module,
) -> tuple[set[str], set[str]]:
    """Collect module aliases and direct imports for ``subprocess`` calls."""
    subproc_aliases: set[str] = {"subprocess"}
    subproc_funcs: set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                if alias.name == "subprocess":
                    subproc_aliases.add(alias.asname or "subprocess")
        elif isinstance(node, ast.ImportFrom) and node.module == "subprocess":
            for alias in node.names:
                if alias.name in _SUBPROCESS_CALL_METHODS:
                    subproc_funcs.add(alias.asname or alias.name)
    return (subproc_aliases, subproc_funcs)


def _kill_non_sim_names(
    names: set[str],
    non_sim_mods: set[str],
    non_sim_classes: set[str],
    non_sim_vars: set[str],
) -> None:
    """Remove ``names`` from all proven module-level non-simulation binding sets."""
    non_sim_mods.difference_update(names)
    non_sim_classes.difference_update(names)
    non_sim_vars.difference_update(names)


def _apply_module_non_sim_import(
    stmt: ast.Import | ast.ImportFrom,
    non_sim_mods: set[str],
    non_sim_classes: set[str],
    non_sim_vars: set[str],
) -> None:
    """Update module non-sim binding sets for an import statement."""
    if isinstance(stmt, ast.Import):
        for alias in stmt.names:
            local_name = alias.asname or alias.name.split(".", 1)[0]
            root_mod = alias.name.split(".", 1)[0]
            _kill_non_sim_names({local_name}, non_sim_mods, non_sim_classes, non_sim_vars)
            if root_mod in _NON_SIMULATION_RUN_MODULES:
                non_sim_mods.add(local_name)
    else:
        root_mod = stmt.module.split(".", 1)[0] if stmt.module else ""
        is_non_sim_mod = root_mod in _NON_SIMULATION_RUN_MODULES
        for alias in stmt.names:
            if alias.name == "*":
                continue
            local_name = alias.asname or alias.name
            _kill_non_sim_names({local_name}, non_sim_mods, non_sim_classes, non_sim_vars)
            if is_non_sim_mod and alias.name in _NON_SIMULATION_RUN_CLASSES:
                non_sim_classes.add(local_name)


def _apply_module_non_sim_assign(
    target: ast.AST,
    value: ast.AST | None,
    non_sim_mods: set[str],
    non_sim_classes: set[str],
    non_sim_vars: set[str],
) -> None:
    """Update module non-sim binding sets for an assignment target."""
    assigned = set(_iter_assigned_names(target))
    is_ctor = (
        value is not None
        and isinstance(target, ast.Name)
        and _is_non_sim_constructor_call(
            value,
            frozenset(non_sim_mods),
            frozenset(non_sim_classes),
            frozenset(),
        )
    )
    _kill_non_sim_names(assigned, non_sim_mods, non_sim_classes, non_sim_vars)
    if is_ctor and isinstance(target, ast.Name):
        non_sim_vars.add(target.id)


def _walk_module_non_sim_stmts(
    stmts: list[ast.stmt],
    non_sim_mods: set[str],
    non_sim_classes: set[str],
    non_sim_vars: set[str],
) -> None:
    """Walk module-level statements in order, preserving only must-hold non-sim bindings."""
    for stmt in stmts:
        if isinstance(stmt, (ast.Import, ast.ImportFrom)):
            _apply_module_non_sim_import(stmt, non_sim_mods, non_sim_classes, non_sim_vars)
        elif isinstance(stmt, ast.Assign):
            for target in stmt.targets:
                _apply_module_non_sim_assign(
                    target, stmt.value, non_sim_mods, non_sim_classes, non_sim_vars
                )
        elif isinstance(stmt, ast.AnnAssign):
            if stmt.value is not None:
                _apply_module_non_sim_assign(
                    stmt.target, stmt.value, non_sim_mods, non_sim_classes, non_sim_vars
                )
        elif isinstance(stmt, ast.AugAssign):
            _kill_non_sim_names(
                set(_iter_assigned_names(stmt.target)),
                non_sim_mods,
                non_sim_classes,
                non_sim_vars,
            )
        elif isinstance(stmt, ast.Delete):
            for target in stmt.targets:
                _kill_non_sim_names(
                    set(_iter_assigned_names(target)),
                    non_sim_mods,
                    non_sim_classes,
                    non_sim_vars,
                )
        elif isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            _kill_non_sim_names({stmt.name}, non_sim_mods, non_sim_classes, non_sim_vars)
        elif isinstance(stmt, ast.If):
            t_m, t_c, t_v = set(non_sim_mods), set(non_sim_classes), set(non_sim_vars)
            e_m, e_c, e_v = set(non_sim_mods), set(non_sim_classes), set(non_sim_vars)
            _walk_module_non_sim_stmts(stmt.body, t_m, t_c, t_v)
            if stmt.orelse:
                _walk_module_non_sim_stmts(stmt.orelse, e_m, e_c, e_v)
            non_sim_mods.clear()
            non_sim_mods.update(t_m & e_m)
            non_sim_classes.clear()
            non_sim_classes.update(t_c & e_c)
            non_sim_vars.clear()
            non_sim_vars.update(t_v & e_v)
        else:
            # Conservative kill for loops/try/with/match at module level: any name
            # written inside cannot be assumed to retain its pre-block non-sim binding.
            written = _collect_written_names_in_stmt(stmt)
            _kill_non_sim_names(written, non_sim_mods, non_sim_classes, non_sim_vars)


def _collect_pattern_capture_names(pattern: ast.AST) -> set[str]:
    """Return variable names bound by a ``match`` case pattern."""
    names: set[str] = set()
    for node in ast.walk(pattern):
        if isinstance(node, (ast.MatchAs, ast.MatchStar)) and node.name:
            names.add(node.name)
        elif isinstance(node, ast.MatchMapping) and node.rest:
            names.add(node.rest)
    return names


def _collect_written_names_in_stmt(root: ast.AST) -> set[str]:
    """Return variable/import/def names bound inside ``root`` (excluding nested defs' bodies)."""
    written: set[str] = set()
    stack: list[ast.AST] = list(ast.iter_child_nodes(root))
    while stack:
        node = stack.pop()
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            written.add(node.name)
            continue
        if isinstance(node, ast.Import):
            written.update(a.asname or a.name.split(".", 1)[0] for a in node.names)
        elif isinstance(node, ast.ImportFrom):
            written.update(a.asname or a.name for a in node.names if a.name != "*")
        elif isinstance(node, ast.Assign):
            for target in node.targets:
                written.update(_iter_assigned_names(target))
        elif isinstance(node, (ast.AnnAssign, ast.AugAssign)):
            written.update(_iter_assigned_names(node.target))
        elif isinstance(node, ast.Delete):
            for target in node.targets:
                written.update(_iter_assigned_names(target))
        elif isinstance(node, (ast.For, ast.AsyncFor)):
            written.update(_iter_assigned_names(node.target))
        elif isinstance(node, ast.NamedExpr):
            written.add(node.target.id)
        elif isinstance(node, ast.ExceptHandler) and node.name:
            written.add(node.name)
        elif isinstance(node, ast.withitem) and node.optional_vars is not None:
            written.update(_iter_assigned_names(node.optional_vars))
        elif isinstance(node, (ast.MatchAs, ast.MatchStar)) and node.name:
            written.add(node.name)
        elif isinstance(node, ast.MatchMapping) and node.rest:
            written.add(node.rest)
        stack.extend(ast.iter_child_nodes(node))
    return written


def _collect_module_non_sim_bindings(
    tree: ast.Module,
) -> tuple[frozenset[str], frozenset[str], frozenset[str]]:
    """Collect module-level bindings provably holding non-simulation runners on all paths."""
    non_sim_mods: set[str] = set()
    non_sim_classes: set[str] = set()
    non_sim_vars: set[str] = set()
    _walk_module_non_sim_stmts(tree.body, non_sim_mods, non_sim_classes, non_sim_vars)
    global_mutated: set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Global):
            global_mutated.update(node.names)
    _kill_non_sim_names(global_mutated, non_sim_mods, non_sim_classes, non_sim_vars)
    return (
        frozenset(non_sim_mods),
        frozenset(non_sim_classes),
        frozenset(non_sim_vars),
    )


def _is_python_token(token: str) -> bool:
    """Return True when executable token names a Python interpreter."""
    cleaned = token.strip().strip("\"'")
    if not cleaned:
        return False
    basename = cleaned.replace("\\", "/").rsplit("/", 1)[-1].lower()
    return bool(_PYTHON_EXE_RE.fullmatch(basename))


def _is_python_executable_expr(
    node: ast.AST,
    env: Mapping[str, list[ast.AST]],
    *,
    seen: frozenset[str] = frozenset(),
    depth: int = 0,
) -> bool:
    """Return True when ``argv[0]`` represents a Python interpreter executable."""
    if depth > 6:
        return False
    unwrapped = _unwrap_path_conversion(node)
    text = _leading_str_constant(unwrapped)
    matched = False
    if text is not None:
        matched = _is_python_token(text)
    elif isinstance(unwrapped, ast.Attribute):
        matched = (
            isinstance(unwrapped.value, ast.Name)
            and unwrapped.value.id == "sys"
            and unwrapped.attr == "executable"
        ) or any(tok in unwrapped.attr.lower() for tok in _PYTHON_EXE_VAR_TOKENS)
    elif isinstance(unwrapped, ast.BinOp) and isinstance(unwrapped.op, ast.Div):
        rhs = _leading_str_constant(unwrapped.right)
        matched = rhs is not None and _is_python_token(rhs)
    elif isinstance(unwrapped, ast.Name):
        if unwrapped.id in env:
            if unwrapped.id not in seen:
                matched = any(
                    _is_python_executable_expr(
                        cand, env, seen=seen | {unwrapped.id}, depth=depth + 1
                    )
                    for cand in env[unwrapped.id]
                )
        else:
            matched = any(
                tok in unwrapped.id.lower() for tok in _PYTHON_EXE_VAR_TOKENS
            )
    return matched


def _flatten_argv_variants(
    node: ast.AST,
    env: Mapping[str, list[ast.AST]],
    *,
    seen: frozenset[str] = frozenset(),
    depth: int = 0,
    overflow_flag: list[bool] | None = None,
) -> list[list[ast.AST]]:
    """Expand an argv expression into candidate flattened element lists."""
    if depth > 6:
        return [[node]]
    if isinstance(node, ast.Name) and node.id in env and node.id not in seen:
        variants: list[list[ast.AST]] = []
        for cand in env[node.id]:
            variants.extend(
                _flatten_argv_variants(
                    cand,
                    env,
                    seen=seen | {node.id},
                    depth=depth + 1,
                    overflow_flag=overflow_flag,
                )
            )
        if len(variants) > _MAX_BRANCH_BINDINGS:
            if overflow_flag is not None:
                overflow_flag[0] = True
            return variants[:_MAX_BRANCH_BINDINGS]
        return variants
    if isinstance(node, (ast.List, ast.Tuple)):
        variants = [[]]
        for elt in node.elts:
            if isinstance(elt, ast.Starred):
                part_variants = _flatten_argv_variants(
                    elt.value,
                    env,
                    seen=seen,
                    depth=depth + 1,
                    overflow_flag=overflow_flag,
                )
                combined = [
                    [*base, *part] for base in variants for part in part_variants
                ]
                if len(combined) > _MAX_BRANCH_BINDINGS and overflow_flag is not None:
                    overflow_flag[0] = True
                variants = combined[:_MAX_BRANCH_BINDINGS]
            else:
                for base in variants:
                    base.append(elt)
        return variants
    if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Add):
        left_variants = _flatten_argv_variants(
            node.left, env, seen=seen, depth=depth + 1, overflow_flag=overflow_flag
        )
        right_variants = _flatten_argv_variants(
            node.right, env, seen=seen, depth=depth + 1, overflow_flag=overflow_flag
        )
        combined = [
            [*left, *right] for left in left_variants for right in right_variants
        ]
        if len(combined) > _MAX_BRANCH_BINDINGS and overflow_flag is not None:
            overflow_flag[0] = True
        return combined[:_MAX_BRANCH_BINDINGS]
    return [[node]]


def _unwrap_path_conversion(node: ast.AST) -> ast.AST:
    """Unwrap ``str(x)``, ``os.fspath(x)``, ``Path(x)``, or zero-arg path methods."""
    cur = node
    for _ in range(5):
        if not isinstance(cur, ast.Call):
            break
        func = cur.func
        if (
            isinstance(func, ast.Attribute)
            and func.attr in {"as_posix", "resolve", "expanduser", "absolute"}
            and not cur.args
        ):
            cur = func.value
            continue
        if cur.args:
            if isinstance(func, ast.Name) and func.id in {"str", "fspath", "Path"}:
                cur = cur.args[0]
                continue
            if isinstance(func, ast.Attribute) and func.attr in {"fspath", "Path"}:
                cur = cur.args[0]
                continue
        break
    return cur


def _detect_cli_from_helper_args(
    call: ast.Call,
    elems: list[ast.AST],
    env: Mapping[str, list[ast.AST]],
    *,
    seen: frozenset[str],
    depth: int,
) -> set[str]:
    """Resolve CLI command(s) from arguments to a SUEWS CLI helper call."""
    found: set[str] = set()
    for arg in call.args:
        arg_s = _leading_str_constant(arg)
        if arg_s is not None:
            exe = _normalise_cli_executable(arg_s)
            if exe is not None:
                found.add(exe)
        elif isinstance(arg, ast.Name) and arg.id in env and arg.id not in seen:
            for cand in env[arg.id]:
                found.update(
                    _detect_suews_clis_in_expr(
                        cand, env, seen=seen | {arg.id}, depth=depth + 1
                    )
                )
    if found:
        return found
    if len(elems) >= 2 and _leading_str_constant(elems[1]) == "validate":
        return {"suews-validate"}
    return {"suews"}


def _detect_cli_from_call_expr(
    call: ast.Call,
    elems: list[ast.AST],
    env: Mapping[str, list[ast.AST]],
    *,
    seen: frozenset[str],
    depth: int,
) -> set[str]:
    """Identify SUEWS CLI executable(s) returned by a helper call expression."""
    func = call.func
    fname = (
        func.id
        if isinstance(func, ast.Name)
        else (func.attr if isinstance(func, ast.Attribute) else "")
    )
    fl = fname.lower()
    result: set[str] = set()
    if fl == "which":
        arg_s = _leading_str_constant(call.args[0]) if call.args else None
        exe = _normalise_cli_executable(arg_s) if arg_s is not None else None
        if exe is not None:
            result.add(exe)
    elif "suews_engine" in fl or fl in {"_locate_engine", "locate_engine"}:
        result.add("suews-engine")
    elif "suews_validate" in fl:
        result.add("suews-validate")
    elif "suews_mcp" in fl:
        result.add("suews-mcp")
    elif "suews_cli" in fl or "entry_point" in fl or "suews_cmd" in fl:
        result.update(
            _detect_cli_from_helper_args(
                call, elems, env, seen=seen, depth=depth
            )
        )
    return result


def _is_supy_module_name(mod_name: str | None) -> bool:
    """Return True when ``mod_name`` is ``supy`` or a submodule of ``supy``."""
    return mod_name is not None and (mod_name == "supy" or mod_name.startswith("supy."))


def _tokens_invoke_python_m_supy(args: list[str | None]) -> bool:
    """Return True if Python interpreter ``args`` invoke ``-m supy...`` before option end."""
    idx = 0
    while idx < len(args):
        tok = args[idx]
        if tok is None or not tok.startswith("-") or tok in {"-", "--", "-c"} or tok.startswith("-c"):
            return False
        if tok == "-m":
            return idx + 1 < len(args) and _is_supy_module_name(args[idx + 1])
        if tok.startswith("-m") and len(tok) > 2:
            return _is_supy_module_name(tok[2:])
        if tok in _PYTHON_TWO_ARG_FLAGS or tok == "--check-hash-based-pycs":
            idx += 2
            continue
        idx += 1
    return False


def _detect_cli_from_string(text: str) -> set[str]:
    """Detect a SUEWS CLI or ``python -m supy...`` invocation in a command string."""
    try:
        tokens = shlex.split(text)
    except ValueError:
        tokens = text.split()
    if not tokens:
        return set()
    exe = _normalise_cli_executable(tokens[0])
    if exe is not None:
        return {exe}
    if _is_python_token(tokens[0]) and _tokens_invoke_python_m_supy(
        list(tokens[1:])
    ):
        return {PYTHON_M_SUPY_COMMAND}
    return set()


_CLI_NAME_SUBSTRINGS: tuple[tuple[str, str], ...] = (
    ("suews_engine", "suews-engine"),
    ("suews_validate", "suews-validate"),
    ("suews_mcp", "suews-mcp"),
    ("suews_convert", "suews-convert"),
    ("suews_schema", "suews-schema"),
    ("suews_inspect", "suews-inspect"),
)


def _is_resolved_literal_executable(node: ast.AST) -> bool:
    """Return True when ``node`` is a concrete literal/path executable expression."""
    unwrapped = _unwrap_path_conversion(node)
    if _leading_str_constant(unwrapped) is not None:
        return True
    if isinstance(unwrapped, ast.BinOp) and isinstance(unwrapped.op, ast.Div):
        return _leading_str_constant(unwrapped.right) is not None
    if isinstance(unwrapped, (ast.List, ast.Tuple)) and unwrapped.elts:
        return _is_resolved_literal_executable(unwrapped.elts[0])
    if isinstance(unwrapped, ast.Call):
        func = unwrapped.func
        fname = (
            func.id
            if isinstance(func, ast.Name)
            else (func.attr if isinstance(func, ast.Attribute) else "")
        )
        if fname.lower() == "which" and unwrapped.args:
            return _leading_str_constant(unwrapped.args[0]) is not None
    return False


def _detect_cli_from_name(
    first: ast.Name,
    elems: list[ast.AST],
    env: Mapping[str, list[ast.AST]],
    *,
    seen: frozenset[str],
    depth: int,
) -> set[str]:
    """Detect SUEWS CLI command(s) from a variable or fixture name in ``argv[0]``."""
    if first.id in env:
        found: set[str] = set()
        if first.id not in seen:
            for cand in env[first.id]:
                found.update(
                    _detect_suews_clis_in_expr(
                        cand, env, seen=seen | {first.id}, depth=depth + 1
                    )
                )
        if found or (
            env[first.id]
            and all(_is_resolved_literal_executable(c) for c in env[first.id])
        ):
            return found
    low = first.id.lower()
    result: set[str] = set()
    for needle, command in _CLI_NAME_SUBSTRINGS:
        if needle in low:
            result = {command}
            break
    if not result and (
        low in _ENGINE_BIN_VAR_NAMES
        or (
            low in _GENERIC_ENGINE_RUN_VARS
            and len(elems) >= 2
            and _leading_str_constant(elems[1]) == "run"
        )
    ):
        result = {"suews-engine"}
    elif not result and low in _SUEWS_EXE_VAR_NAMES:
        sub = _leading_str_constant(elems[1]) if len(elems) >= 2 else None
        result = {"suews-validate"} if sub == "validate" else {"suews"}
    return result


def _elem_string_variants(
    elem: ast.AST,
    env: Mapping[str, list[ast.AST]],
) -> list[str | None]:
    """Return candidate constant string values for one argv element."""
    direct = _leading_str_constant(elem)
    if direct is not None:
        return [direct]
    if isinstance(elem, ast.Name) and elem.id in env:
        vals = [_leading_str_constant(cand) for cand in env[elem.id]]
        return vals or [None]
    return [None]


def _has_python_m_supy(
    elems: list[ast.AST], env: Mapping[str, list[ast.AST]]
) -> bool:
    """Return True when ``elems`` invokes a Python executable with ``-m supy...``."""
    if len(elems) < 2 or not _is_python_executable_expr(elems[0], env):
        return False
    # Check across candidate constant token choices (usually 1 per slot).
    token_lists: list[list[str | None]] = [[]]
    for elem in elems[1:]:
        choices = _elem_string_variants(elem, env)
        token_lists = [
            [*prefix, choice]
            for prefix in token_lists
            for choice in choices
        ][:_MAX_BRANCH_BINDINGS]
    return any(_tokens_invoke_python_m_supy(tokens) for tokens in token_lists)


def _detect_suews_clis_in_argv(
    elems: list[ast.AST],
    env: Mapping[str, list[ast.AST]],
    *,
    seen: frozenset[str],
    depth: int,
) -> set[str]:
    """Classify one flattened argv candidate list into SUEWS CLI command name(s)."""
    if not elems:
        return set()
    if _has_python_m_supy(elems, env):
        return {PYTHON_M_SUPY_COMMAND}
    first = _unwrap_path_conversion(elems[0])
    result: set[str] = set()
    first_str = _leading_str_constant(first)
    if first_str is not None:
        exe = _normalise_cli_executable(first_str)
        if exe is not None:
            result.add(exe)
    elif isinstance(first, ast.BinOp) and isinstance(first.op, ast.Div):
        rhs = _leading_str_constant(first.right)
        exe = _normalise_cli_executable(rhs) if rhs is not None else None
        if exe is not None:
            result.add(exe)
    elif isinstance(first, ast.Name):
        result = _detect_cli_from_name(first, elems, env, seen=seen, depth=depth)
    elif isinstance(first, ast.Call):
        result = _detect_cli_from_call_expr(
            first, elems, env, seen=seen, depth=depth + 1
        )
    return result


def _detect_suews_clis_in_expr(
    expr: ast.AST,
    env: Mapping[str, list[ast.AST]],
    *,
    seen: frozenset[str] = frozenset(),
    depth: int = 0,
    overflow_flag: list[bool] | None = None,
) -> set[str]:
    """Return canonical SUEWS CLI command(s) invoked by ``expr``."""
    if depth > 6:
        return set()
    direct_str = _leading_str_constant(expr)
    if direct_str is not None:
        return _detect_cli_from_string(direct_str)
    found: set[str] = set()
    for elems in _flatten_argv_variants(
        expr, env, seen=seen, depth=depth + 1, overflow_flag=overflow_flag
    ):
        found.update(_detect_suews_clis_in_argv(elems, env, seen=seen, depth=depth))
    return found


def _is_cli_relevant_candidate(node: ast.AST) -> bool:
    """Return True if ``node`` directly resembles a SUEWS CLI or python-m-supy piece."""
    if _detect_suews_clis_in_expr(node, {}):
        return True
    text = _leading_str_constant(_unwrap_path_conversion(node))
    if text is not None:
        return (
            _normalise_cli_executable(text) is not None
            or _is_python_token(text)
            or text == "-m"
            or _is_supy_module_name(text)
        )
    return False


def _compact_ast_candidates(
    candidates: list[ast.AST],
) -> tuple[list[ast.AST], bool]:
    """Deduplicate AST candidates by structure, preserving CLI-relevant items on cap."""
    deduped: list[ast.AST] = []
    seen_dumps: set[str] = set()
    for node in candidates:
        dump = ast.dump(node)
        if dump not in seen_dumps:
            seen_dumps.add(dump)
            deduped.append(node)
    if len(deduped) <= _MAX_BRANCH_BINDINGS:
        return (deduped, False)
    priority = [node for node in deduped if _is_cli_relevant_candidate(node)]
    rest = [node for node in deduped if not _is_cli_relevant_candidate(node)]
    kept = (priority + rest)[:_MAX_BRANCH_BINDINGS]
    return (kept, True)


class _BindingState:
    """Point-in-time variable bindings plus explicit overflow tracking for subprocess AST."""

    def __init__(
        self,
        env: Mapping[str, list[ast.AST]] | None = None,
        overflowed: set[str] | frozenset[str] | None = None,
    ) -> None:
        self.env: dict[str, list[ast.AST]] = (
            {k: list(v) for k, v in env.items()} if env else {}
        )
        self.overflowed: set[str] = set(overflowed) if overflowed else set()

    def clone(self) -> _BindingState:
        """Return a shallow copy of this binding state."""
        return _BindingState(self.env, self.overflowed)


def _merge_binding_states(
    first: _BindingState,
    second: _BindingState,
) -> _BindingState:
    """Conservatively union candidate bindings from two control-flow branches."""
    merged_env: dict[str, list[ast.AST]] = {}
    merged_overflow: set[str] = set(first.overflowed) | set(second.overflowed)
    for key in set(first.env) | set(second.env):
        combined = [*first.env.get(key, ()), *second.env.get(key, ())]
        kept, overflowed = _compact_ast_candidates(combined)
        merged_env[key] = kept
        if overflowed:
            merged_overflow.add(key)
    return _BindingState(merged_env, merged_overflow)


def _substitute_seq_elements(
    elts: list[ast.AST],
    state: _BindingState,
    *,
    depth: int,
) -> tuple[list[list[ast.AST]], bool]:
    """Eagerly substitute element expressions into Cartesian-product element lists."""
    lists: list[list[ast.AST]] = [[]]
    overflowed = False
    for elt in elts:
        if isinstance(elt, ast.Starred):
            sub_vals, sub_ov = _substitute_expr_values(
                elt.value, state, depth=depth + 1
            )
            choices: list[ast.AST] = [
                ast.Starred(value=val, ctx=ast.Load()) for val in sub_vals
            ]
        else:
            choices, sub_ov = _substitute_expr_values(elt, state, depth=depth + 1)
        overflowed = overflowed or sub_ov
        expanded = [[*base, choice] for base in lists for choice in choices]
        if len(expanded) > _MAX_BRANCH_BINDINGS:
            overflowed = True
            lists = expanded[:_MAX_BRANCH_BINDINGS]
        else:
            lists = expanded
    return (lists, overflowed)


def _substitute_keyword_lists(
    keywords: list[ast.keyword],
    state: _BindingState,
    *,
    depth: int,
) -> tuple[list[list[ast.keyword]], bool]:
    """Eagerly substitute keyword argument values with bounded branch combinations."""
    kw_lists: list[list[ast.keyword]] = [[]]
    overflowed = False
    for kw in keywords:
        choices, kw_ov = _substitute_expr_values(kw.value, state, depth=depth + 1)
        overflowed = (
            overflowed
            or kw_ov
            or _expr_references_overflowed(kw.value, state.overflowed)
        )
        expanded = [
            [*base, ast.keyword(arg=kw.arg, value=choice)]
            for base in kw_lists
            for choice in choices
        ]
        if len(expanded) > _MAX_BRANCH_BINDINGS:
            overflowed = True
            kw_lists = expanded[:_MAX_BRANCH_BINDINGS]
        else:
            kw_lists = expanded
    return (kw_lists, overflowed)


def _substitute_call_values(
    expr: ast.Call,
    state: _BindingState,
    *,
    depth: int,
) -> tuple[list[ast.AST], bool]:
    """Eagerly substitute bound variables inside a call's receiver, args, and keywords."""
    func_choices: list[ast.AST] = [expr.func]
    overflowed = False
    if isinstance(expr.func, ast.Attribute):
        recv_vals, recv_ov = _substitute_expr_values(
            expr.func.value, state, depth=depth + 1
        )
        overflowed = overflowed or recv_ov
        func_choices = [
            ast.Attribute(value=rv, attr=expr.func.attr, ctx=ast.Load())
            for rv in recv_vals
        ]
    arg_lists, arg_ov = _substitute_seq_elements(expr.args, state, depth=depth)
    kw_lists, kw_ov = _substitute_keyword_lists(expr.keywords, state, depth=depth)
    overflowed = overflowed or arg_ov or kw_ov
    results = [
        ast.Call(func=fn, args=args, keywords=kws)
        for fn in func_choices
        for args in arg_lists
        for kws in kw_lists
    ]
    kept, cap_ov = _compact_ast_candidates(results)
    return (kept, overflowed or cap_ov)


def _substitute_expr_values(
    expr: ast.AST,
    state: _BindingState,
    *,
    depth: int = 0,
) -> tuple[list[ast.AST], bool]:
    """Eagerly replace active variable references with their assignment-time AST values."""
    result: tuple[list[ast.AST], bool] = ([expr], False)
    if depth <= 6:
        if isinstance(expr, ast.Name) and isinstance(expr.ctx, ast.Load):
            if expr.id in state.env:
                result = (list(state.env[expr.id]), expr.id in state.overflowed)
        elif isinstance(expr, (ast.List, ast.Tuple)):
            elt_lists, overflowed = _substitute_seq_elements(
                expr.elts, state, depth=depth
            )
            cls = ast.List if isinstance(expr, ast.List) else ast.Tuple
            rebuilt = [cls(elts=elts, ctx=ast.Load()) for elts in elt_lists]
            kept, cap_ov = _compact_ast_candidates(rebuilt)
            result = (kept, overflowed or cap_ov)
        elif isinstance(expr, ast.BinOp):
            l_vals, l_ov = _substitute_expr_values(expr.left, state, depth=depth + 1)
            r_vals, r_ov = _substitute_expr_values(expr.right, state, depth=depth + 1)
            rebuilt = [
                ast.BinOp(left=lv, op=expr.op, right=rv)
                for lv in l_vals
                for rv in r_vals
            ]
            kept, cap_ov = _compact_ast_candidates(rebuilt)
            result = (kept, l_ov or r_ov or cap_ov)
        elif isinstance(expr, ast.IfExp):
            b_vals, b_ov = _substitute_expr_values(expr.body, state, depth=depth + 1)
            o_vals, o_ov = _substitute_expr_values(expr.orelse, state, depth=depth + 1)
            kept, cap_ov = _compact_ast_candidates([*b_vals, *o_vals])
            result = (kept, b_ov or o_ov or cap_ov)
        elif isinstance(expr, ast.Call):
            result = _substitute_call_values(expr, state, depth=depth)
    return result


def _expr_references_overflowed(
    expr: ast.AST,
    overflowed: set[str],
) -> bool:
    """Return True if ``expr`` directly loads any variable marked overflowed."""
    if not overflowed:
        return False
    return any(
        isinstance(node, ast.Name)
        and isinstance(node.ctx, ast.Load)
        and node.id in overflowed
        for node in ast.walk(expr)
    )


def _eval_int_constant(node: ast.AST) -> int | None:
    """Evaluate a literal integer (including unary ``-``/``+``), excluding ``bool``."""
    if isinstance(node, ast.Constant) and isinstance(node.value, int) and not isinstance(
        node.value, bool
    ):
        return int(node.value)
    if isinstance(node, ast.UnaryOp) and isinstance(node.op, (ast.USub, ast.UAdd)):
        inner = _eval_int_constant(node.operand)
        if inner is not None:
            return -inner if isinstance(node.op, ast.USub) else inner
    return None


def _range_length(start: int, stop: int, step: int) -> int:
    """Compute exact Python ``range(start, stop, step)`` length without C ssize_t overflow."""
    if step > 0:
        return max(0, (stop - start + step - 1) // step)
    return max(0, (start - stop - step - 1) // (-step))


def _expand_range_call(call: ast.Call) -> list[ast.AST] | None:
    """Expand a static ``range(...)`` call into constant int AST elements."""
    if not (1 <= len(call.args) <= 3) or call.keywords:
        return None
    int_args = [_eval_int_constant(arg) for arg in call.args]
    if any(val is None for val in int_args):
        return None
    r_args = [int(val) for val in int_args if val is not None]
    if len(r_args) == 1:
        start, stop, step = 0, r_args[0], 1
    elif len(r_args) == 2:
        start, stop, step = r_args[0], r_args[1], 1
    else:
        start, stop, step = r_args[0], r_args[1], r_args[2]
    if step == 0:
        return None
    count = _range_length(start, stop, step)
    if count > _MAX_EXPANDED_CASES:
        return None
    return [ast.Constant(value=start + idx * step) for idx in range(count)]


def _dedup_constant_elements(elts: list[ast.AST]) -> list[ast.AST] | None:
    """Deduplicate a set's elements using Python set equality and hash semantics."""
    out: list[ast.AST] = []
    seen_vals: set[object] = set()
    for elt in elts:
        if not isinstance(elt, ast.Constant):
            return None
        try:
            if elt.value not in seen_vals:
                seen_vals.add(elt.value)
                out.append(elt)
        except TypeError:
            return None
    return out


def _expand_dict_view_call(
    call: ast.Call,
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str],
    depth: int,
) -> list[ast.AST] | None:
    """Expand literal ``dict.keys()`` / ``dict.values()`` / ``dict.items()`` calls."""
    del depth
    if call.args or call.keywords or not isinstance(call.func, ast.Attribute):
        return None
    method = call.func.attr
    target: ast.AST = call.func.value
    if isinstance(target, ast.Name) and target.id in env and target.id not in seen:
        target = env[target.id]
    if (
        method not in {"keys", "values", "items"}
        or not isinstance(target, ast.Dict)
        or any(k is None for k in target.keys)
        or len(target.keys) > _MAX_EXPANDED_CASES
    ):
        return None
    if method == "keys":
        return [k for k in target.keys if k is not None]
    if method == "values":
        return list(target.values)
    return [
        ast.Tuple(elts=[k, v], ctx=ast.Load())
        for k, v in zip(target.keys, target.values)
        if k is not None
    ]


def _expand_collection_ctor(
    fname: str,
    node: ast.Call,
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str],
    depth: int,
) -> list[ast.AST] | None:
    """Expand ``list/tuple/set/sorted/reversed(...)`` calls."""
    if not node.args and not node.keywords and fname in {"list", "tuple", "set"}:
        return []
    if len(node.args) != 1 or (fname != "sorted" and node.keywords):
        return None
    inner = _expand_static_sequence(node.args[0], env, seen=seen, depth=depth + 1)
    if inner is None:
        return None
    if fname == "reversed":
        return list(reversed(inner))
    if fname == "set":
        return _dedup_constant_elements(inner)
    return list(inner)


def _expand_call_sequence(
    node: ast.Call,
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str],
    depth: int,
) -> list[ast.AST] | None:
    """Expand supported built-in sequence constructor calls."""
    if isinstance(node.func, ast.Attribute):
        return _expand_dict_view_call(node, env, seen=seen, depth=depth)
    if not isinstance(node.func, ast.Name):
        return None
    fname = node.func.id
    result: list[ast.AST] | None = None
    if fname == "range":
        result = _expand_range_call(node)
    elif fname in {"list", "tuple", "set", "sorted", "reversed"}:
        result = _expand_collection_ctor(fname, node, env, seen=seen, depth=depth)
    elif fname == "enumerate" and len(node.args) == 1 and not node.keywords:
        inner = _expand_static_sequence(node.args[0], env, seen=seen, depth=depth + 1)
        if inner is not None:
            result = [
                ast.Tuple(elts=[ast.Constant(value=i), elt], ctx=ast.Load())
                for i, elt in enumerate(inner)
            ]
    elif fname == "zip" and not node.keywords:
        parts = [
            _expand_static_sequence(arg, env, seen=seen, depth=depth + 1)
            for arg in node.args
        ]
        if all(p is not None for p in parts):
            result = [
                ast.Tuple(elts=list(items), ctx=ast.Load())
                for items in zip(*(p for p in parts if p is not None))
            ]
    return result


def _bind_comp_target(
    target: ast.AST,
    item: ast.AST,
    out: dict[str, ast.AST],
) -> bool:
    """Bind comprehension target variable(s) to ``item``; return False if unsupported."""
    if isinstance(target, ast.Name):
        out[target.id] = item
        return True
    if isinstance(target, (ast.Tuple, ast.List)) and isinstance(
        item, (ast.Tuple, ast.List)
    ):
        if len(target.elts) != len(item.elts):
            return False
        return all(
            _bind_comp_target(sub_t, sub_v, out)
            for sub_t, sub_v in zip(target.elts, item.elts)
        )
    return False


def _expand_comprehension(
    node: ast.ListComp | ast.GeneratorExp,
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str],
    depth: int,
) -> list[ast.AST] | None:
    """Expand filter-free list/generator comprehensions into element AST expressions."""
    if any(gen.ifs or gen.is_async for gen in node.generators):
        return None
    bindings_list: list[dict[str, ast.AST]] = [{}]
    for gen in node.generators:
        next_bindings: list[dict[str, ast.AST]] = []
        for cur_bind in bindings_list:
            sub_env = {**env, **cur_bind}
            items = _expand_static_sequence(
                gen.iter, sub_env, seen=seen, depth=depth + 1
            )
            if items is None:
                return None
            for item in items:
                merged = dict(cur_bind)
                if not _bind_comp_target(gen.target, item, merged):
                    return None
                next_bindings.append(merged)
                if len(next_bindings) > _MAX_EXPANDED_CASES:
                    return None
        bindings_list = next_bindings
    out: list[ast.AST] = []
    for cur_bind in bindings_list:
        bstate = _BindingState({k: [v] for k, v in cur_bind.items()})
        substituted, _ = _substitute_expr_values(node.elt, bstate)
        out.append(substituted[0] if substituted else node.elt)
    return out


def _expand_binop_sequence(
    node: ast.BinOp,
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str],
    depth: int,
) -> list[ast.AST] | None:
    """Expand sequence concatenation ``A + B`` and repetition ``A * n`` / ``n * A``."""
    result: list[ast.AST] | None = None
    if isinstance(node.op, ast.Add):
        left = _expand_static_sequence(node.left, env, seen=seen, depth=depth + 1)
        right = _expand_static_sequence(node.right, env, seen=seen, depth=depth + 1)
        if left is not None and right is not None and len(left) + len(right) <= _MAX_EXPANDED_CASES:
            result = [*left, *right]
    elif isinstance(node.op, ast.Mult):
        l_int = _eval_int_constant(node.left)
        r_int = _eval_int_constant(node.right)
        seq: list[ast.AST] | None = None
        mult = 0
        if l_int is not None and r_int is None:
            seq = _expand_static_sequence(node.right, env, seen=seen, depth=depth + 1)
            mult = l_int
        elif r_int is not None and l_int is None:
            seq = _expand_static_sequence(node.left, env, seen=seen, depth=depth + 1)
            mult = r_int
        if seq is not None:
            if mult <= 0:
                result = []
            elif len(seq) * mult <= _MAX_EXPANDED_CASES:
                result = seq * mult
    return result


def _expand_literal_container(
    node: ast.List | ast.Tuple | ast.Set,
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str],
    depth: int,
) -> list[ast.AST] | None:
    """Expand a literal list/tuple/set (including starred sub-sequences)."""
    items: list[ast.AST] = []
    for elt in node.elts:
        if isinstance(elt, ast.Starred):
            part = _expand_static_sequence(
                elt.value, env, seen=seen, depth=depth + 1
            )
            if part is None or len(items) + len(part) > _MAX_EXPANDED_CASES:
                return None
            items.extend(part)
        else:
            if len(items) + 1 > _MAX_EXPANDED_CASES:
                return None
            items.append(elt)
    return _dedup_constant_elements(items) if isinstance(node, ast.Set) else items


def _expand_static_sequence(
    node: ast.AST,
    env: Mapping[str, ast.AST],
    *,
    seen: frozenset[str] = frozenset(),
    depth: int = 0,
) -> list[ast.AST] | None:
    """Return statically expanded sequence elements, or ``None`` when cardinality is unknown."""
    result: list[ast.AST] | None = None
    if depth <= 8:
        if isinstance(node, ast.Name):
            if node.id in env and node.id not in seen:
                result = _expand_static_sequence(
                    env[node.id], env, seen=seen | {node.id}, depth=depth + 1
                )
        elif isinstance(node, (ast.List, ast.Tuple, ast.Set)):
            result = _expand_literal_container(node, env, seen=seen, depth=depth)
        elif isinstance(node, ast.Call):
            result = _expand_call_sequence(node, env, seen=seen, depth=depth)
        elif isinstance(node, ast.BinOp):
            result = _expand_binop_sequence(node, env, seen=seen, depth=depth)
        elif isinstance(node, (ast.ListComp, ast.GeneratorExp)):
            result = _expand_comprehension(node, env, seen=seen, depth=depth)
        elif (
            isinstance(node, ast.Constant)
            and isinstance(node.value, str)
            and len(node.value) <= _MAX_EXPANDED_CASES
        ):
            result = [ast.Constant(value=ch) for ch in node.value]
    return result


def _expand_iterable_in_state(
    iter_expr: ast.AST,
    state: _BindingState,
) -> tuple[list[ast.AST], bool]:
    """Expand a ``for`` loop iterable using current eager bindings."""
    subst_iters, overflowed = _substitute_expr_values(iter_expr, state)
    single_env = {k: v[0] for k, v in state.env.items() if len(v) == 1}
    collected: list[ast.AST] = []
    for cand in subst_iters:
        expanded = _expand_static_sequence(cand, single_env)
        if expanded is not None:
            collected.extend(expanded)
    return (collected, overflowed)


def _clear_for_target(
    target: ast.AST,
    state: _BindingState,
    *,
    iter_overflowed: bool,
) -> None:
    """Clear outer bindings shadowed by a ``for`` or comprehension target."""
    for name in _iter_assigned_names(target):
        state.env.pop(name, None)
        if iter_overflowed:
            state.overflowed.add(name)
        else:
            state.overflowed.discard(name)


def _bind_for_target(
    target: ast.AST,
    elements: list[ast.AST],
    state: _BindingState,
    *,
    iter_overflowed: bool,
) -> None:
    """Bind ``for`` loop target variable(s) to statically expanded iterable elements."""
    if not elements:
        _clear_for_target(target, state, iter_overflowed=iter_overflowed)
        return
    if isinstance(target, ast.Name):
        kept, cap_ov = _compact_ast_candidates(elements)
        state.env[target.id] = kept
        if iter_overflowed or cap_ov:
            state.overflowed.add(target.id)
        else:
            state.overflowed.discard(target.id)
        return
    if isinstance(target, (ast.Tuple, ast.List)):
        ncols = len(target.elts)
        columns: list[list[ast.AST]] = [[] for _ in range(ncols)]
        for elt in elements:
            if isinstance(elt, (ast.Tuple, ast.List)) and len(elt.elts) == ncols:
                for col_idx, sub_val in enumerate(elt.elts):
                    columns[col_idx].append(sub_val)
        for sub_target, col_vals in zip(target.elts, columns):
            _bind_for_target(
                sub_target, col_vals, state, iter_overflowed=iter_overflowed
            )
        return
    _clear_for_target(target, state, iter_overflowed=iter_overflowed)


def _args_param_names(args: ast.arguments) -> set[str]:
    """Return all parameter names bound by ``args``."""
    names = {
        *(arg.arg for arg in args.posonlyargs),
        *(arg.arg for arg in args.args),
        *(arg.arg for arg in args.kwonlyargs),
    }
    if args.vararg is not None:
        names.add(args.vararg.arg)
    if args.kwarg is not None:
        names.add(args.kwarg.arg)
    return names


def _extract_args_defaults(
    args: ast.arguments,
    def_state: _BindingState,
) -> dict[str, list[ast.AST]]:
    """Extract eagerly substituted default-argument expressions for ``args``."""
    pos_args = [
        *(arg.arg for arg in args.posonlyargs),
        *(arg.arg for arg in args.args),
    ]
    defaults: dict[str, list[ast.AST]] = {}
    if args.defaults:
        offset = len(pos_args) - len(args.defaults)
        for idx, default_expr in enumerate(args.defaults):
            vals, _ = _substitute_expr_values(default_expr, def_state)
            defaults[pos_args[offset + idx]] = vals
    for kwarg, default_expr in zip(args.kwonlyargs, args.kw_defaults):
        if default_expr is not None:
            vals, _ = _substitute_expr_values(default_expr, def_state)
            defaults[kwarg.arg] = vals
    return defaults


def _record_calls_in_comp(
    comp: ast.ListComp | ast.SetComp | ast.GeneratorExp | ast.DictComp,
    state: _BindingState,
    recorded: list[tuple[ast.Call, _BindingState]],
    enclosing_state: _BindingState | None,
) -> None:
    """Record calls inside a comprehension with generator-scoped target bindings."""
    walrus_target = state if enclosing_state is None else enclosing_state
    inner = state.clone()
    for idx, gen in enumerate(comp.generators):
        iter_state = state if idx == 0 else inner
        _record_calls_in_expr(
            gen.iter, iter_state, recorded, enclosing_state=walrus_target
        )
        elements, iter_ov = _expand_iterable_in_state(gen.iter, iter_state)
        _bind_for_target(gen.target, elements, inner, iter_overflowed=iter_ov)
        for cond in gen.ifs:
            _record_calls_in_expr(
                cond, inner, recorded, enclosing_state=walrus_target
            )
    if isinstance(comp, ast.DictComp):
        _record_calls_in_expr(
            comp.key, inner, recorded, enclosing_state=walrus_target
        )
        _record_calls_in_expr(
            comp.value, inner, recorded, enclosing_state=walrus_target
        )
    else:
        _record_calls_in_expr(
            comp.elt, inner, recorded, enclosing_state=walrus_target
        )


def _record_calls_in_lambda(
    expr: ast.Lambda,
    state: _BindingState,
    recorded: list[tuple[ast.Call, _BindingState]],
    enclosing_state: _BindingState | None,
) -> None:
    """Record calls inside a ``lambda`` with parameter shadowing and defaults."""
    for default_expr in expr.args.defaults:
        _record_calls_in_expr(
            default_expr, state, recorded, enclosing_state=enclosing_state
        )
    for default_expr in expr.args.kw_defaults:
        if default_expr is not None:
            _record_calls_in_expr(
                default_expr, state, recorded, enclosing_state=enclosing_state
            )
    inner = state.clone()
    for name in _args_param_names(expr.args):
        inner.env.pop(name, None)
        inner.overflowed.discard(name)
    for p_name, p_vals in _extract_args_defaults(expr.args, state).items():
        inner.env[p_name] = p_vals
    _record_calls_in_expr(expr.body, inner, recorded, enclosing_state=None)


def _record_calls_in_expr(
    expr: ast.AST,
    state: _BindingState,
    recorded: list[tuple[ast.Call, _BindingState]],
    *,
    enclosing_state: _BindingState | None = None,
) -> None:
    """Record every ``ast.Call`` inside ``expr`` with expression-scoped bindings."""
    if isinstance(expr, (ast.ListComp, ast.SetComp, ast.GeneratorExp, ast.DictComp)):
        _record_calls_in_comp(expr, state, recorded, enclosing_state)
        return
    if isinstance(expr, ast.Lambda):
        _record_calls_in_lambda(expr, state, recorded, enclosing_state)
        return
    if isinstance(expr, ast.NamedExpr):
        _record_calls_in_expr(
            expr.value, state, recorded, enclosing_state=enclosing_state
        )
        _assign_eager_target(expr.target, expr.value, state)
        if enclosing_state is not None:
            _assign_eager_target(expr.target, expr.value, enclosing_state)
        return
    if isinstance(expr, ast.IfExp):
        _record_calls_in_expr(
            expr.test, state, recorded, enclosing_state=enclosing_state
        )
        body_state = state.clone()
        else_state = state.clone()
        _record_calls_in_expr(
            expr.body, body_state, recorded, enclosing_state=enclosing_state
        )
        _record_calls_in_expr(
            expr.orelse, else_state, recorded, enclosing_state=enclosing_state
        )
        merged = _merge_binding_states(body_state, else_state)
        state.env = merged.env
        state.overflowed = merged.overflowed
        return
    for child in ast.iter_child_nodes(expr):
        _record_calls_in_expr(child, state, recorded, enclosing_state=enclosing_state)
    if isinstance(expr, ast.Call):
        recorded.append((expr, state.clone()))


_MAX_LOOP_FIXPOINT_ITERS: int = 6


def _binding_state_signature(
    state: _BindingState,
) -> tuple[tuple[str, bool, tuple[str, ...]], ...]:
    """Return a hashable structural signature of ``state`` for loop convergence."""
    return tuple(
        (key, key in state.overflowed, tuple(ast.dump(n) for n in state.env[key]))
        for key in sorted(state.env)
    )


def _record_calls_in_stmt_order(
    stmts: list[ast.stmt],
    state: _BindingState,
    recorded: list[tuple[ast.Call, _BindingState]],
    *,
    exc_accum: list[_BindingState] | None = None,
    descend_defs: bool = True,
    returns: list[tuple[ast.AST, _BindingState]] | None = None,
) -> _BindingState:
    """Walk statements in execution order, recording calls with point-in-time bindings."""
    cur = state.clone()
    for stmt in stmts:
        cur = _step_single_stmt(
            stmt,
            cur,
            recorded,
            exc_accum=exc_accum,
            descend_defs=descend_defs,
            returns=returns,
        )
        if exc_accum is not None:
            exc_accum[0] = _merge_binding_states(exc_accum[0], cur)
    return cur


def _step_loop_stmt(
    stmt: ast.For | ast.AsyncFor | ast.While,
    state: _BindingState,
    recorded: list[tuple[ast.Call, _BindingState]],
    *,
    exc_accum: list[_BindingState] | None,
    descend_defs: bool,
    returns: list[tuple[ast.AST, _BindingState]] | None = None,
) -> _BindingState:
    """Compute bounded loop-entry convergence so loop-carried bindings reach calls."""
    header = stmt.iter if isinstance(stmt, (ast.For, ast.AsyncFor)) else stmt.test
    _record_calls_in_expr(header, state, recorded)
    cur_entry = state.clone()
    elements: list[ast.AST] = []
    iter_ov = False
    if isinstance(stmt, (ast.For, ast.AsyncFor)):
        elements, iter_ov = _expand_iterable_in_state(stmt.iter, state)
        _bind_for_target(stmt.target, elements, cur_entry, iter_overflowed=iter_ov)

    converged = False
    prev_sig = _binding_state_signature(cur_entry)
    for _ in range(_MAX_LOOP_FIXPOINT_ITERS):
        dry_recorded: list[tuple[ast.Call, _BindingState]] = []
        body_exit = _record_calls_in_stmt_order(
            stmt.body,
            cur_entry.clone(),
            dry_recorded,
            exc_accum=exc_accum,
            descend_defs=descend_defs,
        )
        next_entry = _merge_binding_states(cur_entry, body_exit)
        if isinstance(stmt, (ast.For, ast.AsyncFor)) and elements:
            _bind_for_target(stmt.target, elements, next_entry, iter_overflowed=iter_ov)
        next_sig = _binding_state_signature(next_entry)
        if next_sig == prev_sig:
            converged = True
            break
        cur_entry = next_entry
        prev_sig = next_sig

    if not converged:
        # Mark every non-converging variable as overflowed so subprocess checks fail closed.
        dry_recorded = []
        body_exit = _record_calls_in_stmt_order(
            stmt.body,
            cur_entry.clone(),
            dry_recorded,
            exc_accum=exc_accum,
            descend_defs=descend_defs,
        )
        for key in set(cur_entry.env) | set(body_exit.env):
            before_d = [ast.dump(n) for n in cur_entry.env.get(key, ())]
            after_d = [ast.dump(n) for n in body_exit.env.get(key, ())]
            if before_d != after_d:
                cur_entry.overflowed.add(key)

    body_state = _record_calls_in_stmt_order(
        stmt.body,
        cur_entry.clone(),
        recorded,
        exc_accum=exc_accum,
        descend_defs=descend_defs,
        returns=returns,
    )
    merged = _merge_binding_states(state, _merge_binding_states(cur_entry, body_state))
    if stmt.orelse:
        else_state = _record_calls_in_stmt_order(
            stmt.orelse,
            merged.clone(),
            recorded,
            exc_accum=exc_accum,
            descend_defs=descend_defs,
            returns=returns,
        )
        merged = _merge_binding_states(merged, else_state)
    return merged


_TRY_STMT_TYPES: tuple[type[ast.stmt], ...] = (
    (ast.Try, ast.TryStar) if hasattr(ast, "TryStar") else (ast.Try,)
)


def _step_try_stmt(
    stmt: ast.stmt,
    state: _BindingState,
    recorded: list[tuple[ast.Call, _BindingState]],
    *,
    exc_accum: list[_BindingState] | None,
    descend_defs: bool,
    returns: list[tuple[ast.AST, _BindingState]] | None = None,
) -> _BindingState:
    """Preserve intermediate bindings from ``try``/``try*`` body for handlers and ``finally``."""
    body: list[ast.stmt] = getattr(stmt, "body", [])
    orelse: list[ast.stmt] = getattr(stmt, "orelse", [])
    handlers: list[ast.ExceptHandler] = getattr(stmt, "handlers", [])
    finalbody: list[ast.stmt] = getattr(stmt, "finalbody", [])

    try_exc: list[_BindingState] = [state.clone()]
    body_end = _record_calls_in_stmt_order(
        body,
        state.clone(),
        recorded,
        exc_accum=try_exc,
        descend_defs=descend_defs,
        returns=returns,
    )
    handler_entry = try_exc[0]
    if exc_accum is not None:
        exc_accum[0] = _merge_binding_states(exc_accum[0], handler_entry)

    normal_exit = (
        _record_calls_in_stmt_order(
            orelse,
            body_end.clone(),
            recorded,
            exc_accum=exc_accum,
            descend_defs=descend_defs,
            returns=returns,
        )
        if orelse
        else body_end
    )
    merged = normal_exit
    for handler in handlers:
        h_state = _record_calls_in_stmt_order(
            handler.body,
            handler_entry.clone(),
            recorded,
            exc_accum=exc_accum,
            descend_defs=descend_defs,
            returns=returns,
        )
        merged = _merge_binding_states(merged, h_state)
    if finalbody:
        finally_entry = _merge_binding_states(merged, handler_entry)
        merged = _record_calls_in_stmt_order(
            finalbody,
            finally_entry,
            recorded,
            exc_accum=exc_accum,
            descend_defs=descend_defs,
            returns=returns,
        )
    return merged


def _step_compound_stmt(
    stmt: ast.stmt,
    state: _BindingState,
    recorded: list[tuple[ast.Call, _BindingState]],
    *,
    exc_accum: list[_BindingState] | None = None,
    descend_defs: bool = True,
    returns: list[tuple[ast.AST, _BindingState]] | None = None,
) -> _BindingState:
    """Handle control-flow compound statements while preserving branch bindings."""
    if isinstance(stmt, ast.If):
        _record_calls_in_expr(stmt.test, state, recorded)
        then_state = _record_calls_in_stmt_order(
            stmt.body,
            state.clone(),
            recorded,
            exc_accum=exc_accum,
            descend_defs=descend_defs,
            returns=returns,
        )
        else_state = (
            _record_calls_in_stmt_order(
                stmt.orelse,
                state.clone(),
                recorded,
                exc_accum=exc_accum,
                descend_defs=descend_defs,
                returns=returns,
            )
            if stmt.orelse
            else state
        )
        return _merge_binding_states(then_state, else_state)
    if isinstance(stmt, (ast.For, ast.AsyncFor, ast.While)):
        return _step_loop_stmt(
            stmt,
            state,
            recorded,
            exc_accum=exc_accum,
            descend_defs=descend_defs,
            returns=returns,
        )
    if isinstance(stmt, (ast.With, ast.AsyncWith)):
        body_entry = state.clone()
        for item in stmt.items:
            _record_calls_in_expr(item.context_expr, body_entry, recorded)
            if item.optional_vars is not None:
                _clear_for_target(
                    item.optional_vars, body_entry, iter_overflowed=False
                )
        with_exc: list[_BindingState] = [body_entry.clone()]
        body_end = _record_calls_in_stmt_order(
            stmt.body,
            body_entry.clone(),
            recorded,
            exc_accum=with_exc,
            descend_defs=descend_defs,
            returns=returns,
        )
        if exc_accum is not None:
            exc_accum[0] = _merge_binding_states(exc_accum[0], with_exc[0])
        return _merge_binding_states(body_end, with_exc[0])
    if isinstance(stmt, _TRY_STMT_TYPES):
        return _step_try_stmt(
            stmt,
            state,
            recorded,
            exc_accum=exc_accum,
            descend_defs=descend_defs,
            returns=returns,
        )
    if isinstance(stmt, ast.Match):
        _record_calls_in_expr(stmt.subject, state, recorded)
        merged = state.clone()
        for case in stmt.cases:
            c_state = _record_calls_in_stmt_order(
                case.body,
                state.clone(),
                recorded,
                exc_accum=exc_accum,
                descend_defs=descend_defs,
                returns=returns,
            )
            merged = _merge_binding_states(merged, c_state)
        return merged
    return state


def _bind_unpacked_candidates(
    target: ast.AST,
    candidates: list[ast.AST],
    state: _BindingState,
    *,
    overflowed: bool,
) -> None:
    """Bind ``target`` (name or tuple/list) to already-evaluated ``candidates``."""
    if isinstance(target, ast.Name):
        kept, cap_ov = _compact_ast_candidates(candidates)
        state.env[target.id] = kept
        if overflowed or cap_ov:
            state.overflowed.add(target.id)
        else:
            state.overflowed.discard(target.id)
        return
    if isinstance(target, (ast.Tuple, ast.List)) and not any(
        isinstance(e, ast.Starred) for e in target.elts
    ):
        ncols = len(target.elts)
        columns: list[list[ast.AST]] = [[] for _ in range(ncols)]
        all_unpacked = bool(candidates)
        for cand in candidates:
            if isinstance(cand, (ast.Tuple, ast.List)) and len(cand.elts) == ncols:
                for idx, item in enumerate(cand.elts):
                    columns[idx].append(item)
            else:
                all_unpacked = False
                break
        if all_unpacked:
            for sub_target, col_vals in zip(target.elts, columns):
                _bind_unpacked_candidates(
                    sub_target, col_vals, state, overflowed=overflowed
                )
            return
    for name in _iter_assigned_names(target):
        state.env.pop(name, None)
        if overflowed:
            state.overflowed.add(name)
        else:
            state.overflowed.discard(name)


def _assign_eager_target(
    target: ast.AST,
    value: ast.AST,
    state: _BindingState,
) -> None:
    """Eagerly substitute ``value`` in ``state`` and bind ``target`` (supporting unpacking)."""
    subst_vals, overflowed = _substitute_expr_values(value, state)
    _bind_unpacked_candidates(target, subst_vals, state, overflowed=overflowed)


def _step_single_stmt(
    stmt: ast.stmt,
    state: _BindingState,
    recorded: list[tuple[ast.Call, _BindingState]],
    *,
    exc_accum: list[_BindingState] | None = None,
    descend_defs: bool = True,
    returns: list[tuple[ast.AST, _BindingState]] | None = None,
) -> _BindingState:
    """Process one statement in order, eagerly substituting assignment-time values."""
    if isinstance(stmt, ast.Assign):
        _record_calls_in_expr(stmt.value, state, recorded)
        for target in stmt.targets:
            _assign_eager_target(target, stmt.value, state)
    elif isinstance(stmt, ast.AnnAssign):
        if stmt.value is not None:
            _record_calls_in_expr(stmt.value, state, recorded)
            _assign_eager_target(stmt.target, stmt.value, state)
    elif isinstance(stmt, ast.AugAssign):
        _record_calls_in_expr(stmt.value, state, recorded)
        if isinstance(stmt.op, ast.Add) and isinstance(stmt.target, ast.Name):
            prev = state.env.get(stmt.target.id)
            if prev:
                rhs_vals, rhs_ov = _substitute_expr_values(stmt.value, state)
                combined = [
                    ast.BinOp(left=p, op=ast.Add(), right=rv)
                    for p in prev
                    for rv in rhs_vals
                ]
                kept, cap_ov = _compact_ast_candidates(combined)
                state.env[stmt.target.id] = kept
                if rhs_ov or cap_ov:
                    state.overflowed.add(stmt.target.id)
    elif isinstance(stmt, ast.Return):
        if stmt.value is not None:
            _record_calls_in_expr(stmt.value, state, recorded)
            if returns is not None:
                returns.append((stmt.value, state.clone()))
    elif isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
        if descend_defs:
            _record_calls_in_stmt_order(stmt.body, state.clone(), recorded)
    elif isinstance(
        stmt,
        (
            ast.If,
            ast.For,
            ast.AsyncFor,
            ast.While,
            ast.With,
            ast.AsyncWith,
            *_TRY_STMT_TYPES,
            ast.Match,
        ),
    ):
        return _step_compound_stmt(
            stmt,
            state,
            recorded,
            exc_accum=exc_accum,
            descend_defs=descend_defs,
            returns=returns,
        )
    else:
        _record_calls_in_expr(stmt, state, recorded)
    return state


def _is_pytest_collected_function(fn: ast.FunctionDef | ast.AsyncFunctionDef) -> bool:
    """Return True when pytest's default ``python_functions = ('test',)`` collects ``fn``."""
    return fn.name.startswith("test") and not any(
        _is_fixture_decorator(dec) for dec in fn.decorator_list
    )


def _build_module_binding_state(
    tree: ast.Module,
) -> tuple[_BindingState, dict[str, _BindingState]]:
    """Evaluate module-level bindings in execution order and snapshot helper def states."""
    cur = _BindingState()
    helper_def_states: dict[str, _BindingState] = {}
    discarded_calls: list[tuple[ast.Call, _BindingState]] = []
    for stmt in tree.body:
        if isinstance(
            stmt, (ast.FunctionDef, ast.AsyncFunctionDef)
        ) and not _is_pytest_collected_function(stmt):
            helper_def_states[stmt.name] = cur.clone()
        cur = _step_single_stmt(stmt, cur, discarded_calls, descend_defs=False)
    return (cur, helper_def_states)


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
    env: Mapping[str, list[ast.AST]],
    param_names: set[str],
) -> set[str]:
    """Return parameter names of a helper that flow into ``subprocess`` ``argv[0]``."""
    forwarded: set[str] = set()
    for elems in _flatten_argv_variants(cmd_arg, env):
        if not elems:
            continue
        first = _unwrap_path_conversion(elems[0])
        if isinstance(first, ast.Name) and first.id in param_names:
            forwarded.add(first.id)
        elif isinstance(first, ast.Call):
            for arg in first.args:
                unwrapped = _unwrap_path_conversion(arg)
                if isinstance(unwrapped, ast.Name) and unwrapped.id in param_names:
                    forwarded.add(unwrapped.id)
    return forwarded


def _collect_helper_defs(
    tree: ast.Module,
) -> dict[str, ast.FunctionDef | ast.AsyncFunctionDef]:
    """Return top-level non-test helper function definitions in ``tree``."""
    helpers: dict[str, ast.FunctionDef | ast.AsyncFunctionDef] = {}
    for stmt in tree.body:
        if isinstance(
            stmt, (ast.FunctionDef, ast.AsyncFunctionDef)
        ) and not _is_pytest_collected_function(stmt):
            helpers[stmt.name] = stmt
    return helpers


def _bind_helper_positional_args(
    pos_args: list[str],
    call_args: list[ast.expr],
    caller_state: _BindingState,
    callee_state: _BindingState,
) -> tuple[set[str], bool, bool]:
    """Bind positional and starred call arguments into ``callee_state``."""
    bound: set[str] = set()
    has_unknown_starred = False
    starred_overflow = False
    pos_idx = 0
    for arg in call_args:
        if isinstance(arg, ast.Starred):
            items, star_ov = _expand_iterable_in_state(arg.value, caller_state)
            starred_overflow = (
                starred_overflow
                or star_ov
                or _expr_references_overflowed(arg.value, caller_state.overflowed)
            )
            if not items:
                has_unknown_starred = True
                break
            for item in items:
                if pos_idx >= len(pos_args):
                    break
                p_name = pos_args[pos_idx]
                callee_state.env[p_name] = [item]
                if starred_overflow:
                    callee_state.overflowed.add(p_name)
                bound.add(p_name)
                pos_idx += 1
            continue
        if pos_idx < len(pos_args):
            p_name = pos_args[pos_idx]
            vals, arg_ov = _substitute_expr_values(arg, caller_state)
            callee_state.env[p_name] = vals
            if arg_ov or _expr_references_overflowed(arg, caller_state.overflowed):
                callee_state.overflowed.add(p_name)
            bound.add(p_name)
            pos_idx += 1
    return (bound, has_unknown_starred, starred_overflow)


def _bind_helper_keyword_args(
    kw_bindable: set[str],
    call_keywords: list[ast.keyword],
    caller_state: _BindingState,
    callee_state: _BindingState,
) -> tuple[set[str], bool, bool]:
    """Bind keyword call arguments into ``callee_state``."""
    bound: set[str] = set()
    has_kwargs_unpack = False
    kwargs_overflow = False
    for kw in call_keywords:
        if kw.arg is None:
            has_kwargs_unpack = True
            kwargs_overflow = kwargs_overflow or _expr_references_overflowed(
                kw.value, caller_state.overflowed
            )
        elif kw.arg in kw_bindable:
            vals, kw_ov = _substitute_expr_values(kw.value, caller_state)
            callee_state.env[kw.arg] = vals
            if kw_ov or _expr_references_overflowed(kw.value, caller_state.overflowed):
                callee_state.overflowed.add(kw.arg)
            bound.add(kw.arg)
    return (bound, has_kwargs_unpack, kwargs_overflow)


def _bind_helper_call_state(
    fn: ast.FunctionDef | ast.AsyncFunctionDef,
    call: ast.Call,
    caller_state: _BindingState,
    mod_state: _BindingState,
    def_state: _BindingState,
) -> _BindingState:
    """Build callee binding state with parameters shadowed and bound from ``call``."""
    callee_state = mod_state.clone()
    pos_args = [
        *(arg.arg for arg in fn.args.posonlyargs),
        *(arg.arg for arg in fn.args.args),
    ]
    kw_bindable = {
        *(arg.arg for arg in fn.args.args),
        *(arg.arg for arg in fn.args.kwonlyargs),
    }
    for p_name in _args_param_names(fn.args):
        callee_state.env.pop(p_name, None)
        callee_state.overflowed.discard(p_name)

    pos_bound, has_unknown_star, star_ov = _bind_helper_positional_args(
        pos_args, call.args, caller_state, callee_state
    )
    kw_bound, has_kw_unpack, kw_ov = _bind_helper_keyword_args(
        kw_bindable, call.keywords, caller_state, callee_state
    )
    bound_params = pos_bound | kw_bound

    for p_name, def_vals in _extract_args_defaults(fn.args, def_state).items():
        if p_name in bound_params:
            continue
        blocked_star = has_unknown_star and p_name in pos_args
        blocked_kw = has_kw_unpack and p_name in kw_bindable
        if blocked_star or blocked_kw:
            if (blocked_star and star_ov) or (blocked_kw and kw_ov):
                callee_state.overflowed.add(p_name)
            continue
        callee_state.env[p_name] = list(def_vals)
    return callee_state


def _expand_bound_helper_args(
    param_names: set[str],
    callee_state: _BindingState,
    caller_state: _BindingState,
    mod_state: _BindingState,
    helper_defs: Mapping[str, ast.FunctionDef | ast.AsyncFunctionDef],
    helper_def_states: Mapping[str, _BindingState],
    *,
    seen_helpers: frozenset[str],
    depth: int,
) -> None:
    """Expand nested helper calls inside arguments already bound into ``callee_state``."""
    for p_name in param_names:
        p_vals = callee_state.env.get(p_name)
        if not p_vals:
            continue
        exp_arg_vals: list[ast.AST] = []
        arg_ov = p_name in callee_state.overflowed
        for val in p_vals:
            exp_v, v_ov = _expand_helper_returns_in_expr(
                val,
                caller_state,
                mod_state,
                helper_defs,
                helper_def_states,
                seen_helpers=seen_helpers,
                depth=depth + 1,
            )
            exp_arg_vals.extend(exp_v)
            arg_ov = arg_ov or v_ov
        kept_args, cap_ov = _compact_ast_candidates(exp_arg_vals)
        callee_state.env[p_name] = kept_args
        if arg_ov or cap_ov:
            callee_state.overflowed.add(p_name)


def _evaluate_helper_call_returns(
    call: ast.Call,
    caller_state: _BindingState,
    mod_state: _BindingState,
    helper_defs: Mapping[str, ast.FunctionDef | ast.AsyncFunctionDef],
    helper_def_states: Mapping[str, _BindingState],
    *,
    seen_helpers: frozenset[str],
    depth: int,
) -> tuple[list[ast.AST], bool]:
    """Evaluate return expressions of a module helper call in its callee scope."""
    callee = call.func.id if isinstance(call.func, ast.Name) else ""
    fn = helper_defs.get(callee)
    if fn is None or callee in seen_helpers or depth > 4:
        return ([call], False)

    callee_state = _bind_helper_call_state(
        fn, call, caller_state, mod_state, helper_def_states.get(callee, mod_state)
    )
    _expand_bound_helper_args(
        _args_param_names(fn.args),
        callee_state,
        caller_state,
        mod_state,
        helper_defs,
        helper_def_states,
        seen_helpers=seen_helpers,
        depth=depth,
    )

    collected_returns: list[tuple[ast.AST, _BindingState]] = []
    _record_calls_in_stmt_order(
        fn.body,
        callee_state,
        [],
        descend_defs=False,
        returns=collected_returns,
    )
    if not collected_returns:
        return ([call], False)

    expanded: list[ast.AST] = []
    ov = False
    next_seen = seen_helpers | {callee}
    for ret_expr, ret_state in collected_returns:
        sub_vals, sub_ov = _substitute_expr_values(ret_expr, ret_state)
        ov = ov or sub_ov or _expr_references_overflowed(ret_expr, ret_state.overflowed)
        for val in sub_vals:
            inner, inner_ov = _expand_helper_returns_in_expr(
                val,
                ret_state,
                mod_state,
                helper_defs,
                helper_def_states,
                seen_helpers=next_seen,
                depth=depth + 1,
            )
            expanded.extend(inner)
            ov = ov or inner_ov
    kept, cap_ov = _compact_ast_candidates(expanded)
    return (kept, ov or cap_ov)


def _expand_helper_seq_expr(
    expr: ast.List | ast.Tuple,
    caller_state: _BindingState,
    mod_state: _BindingState,
    helper_defs: Mapping[str, ast.FunctionDef | ast.AsyncFunctionDef],
    helper_def_states: Mapping[str, _BindingState],
    *,
    seen_helpers: frozenset[str],
    shadowed_names: frozenset[str],
    depth: int,
) -> tuple[list[ast.AST], bool]:
    """Expand module helper calls inside list/tuple elements with bounded combinations."""
    elt_lists: list[list[ast.AST]] = [[]]
    ov = False
    for elt in expr.elts:
        elt_vals, elt_ov = _expand_helper_returns_in_expr(
            elt,
            caller_state,
            mod_state,
            helper_defs,
            helper_def_states,
            seen_helpers=seen_helpers,
            shadowed_names=shadowed_names,
            depth=depth + 1,
        )
        ov = ov or elt_ov
        next_lists = [[*prefix, ev] for prefix in elt_lists for ev in elt_vals]
        if len(next_lists) > _MAX_BRANCH_BINDINGS:
            ov = True
            next_lists = next_lists[:_MAX_BRANCH_BINDINGS]
        elt_lists = next_lists
    cls = ast.List if isinstance(expr, ast.List) else ast.Tuple
    rebuilt = [cls(elts=elts, ctx=ast.Load()) for elts in elt_lists]
    kept, cap_ov = _compact_ast_candidates(rebuilt)
    return (kept, ov or cap_ov)


def _expand_helper_returns_in_expr(
    expr: ast.AST,
    caller_state: _BindingState,
    mod_state: _BindingState,
    helper_defs: Mapping[str, ast.FunctionDef | ast.AsyncFunctionDef],
    helper_def_states: Mapping[str, _BindingState],
    *,
    seen_helpers: frozenset[str] = frozenset(),
    shadowed_names: frozenset[str] = frozenset(),
    depth: int = 0,
) -> tuple[list[ast.AST], bool]:
    """Expand calls to module helpers inside ``expr`` using each callee's own scope."""
    if depth > 4:
        return ([expr], False)
    if (
        isinstance(expr, ast.Call)
        and isinstance(expr.func, ast.Name)
        and expr.func.id in helper_defs
        and expr.func.id not in seen_helpers
        and expr.func.id not in shadowed_names
    ):
        return _evaluate_helper_call_returns(
            expr,
            caller_state,
            mod_state,
            helper_defs,
            helper_def_states,
            seen_helpers=seen_helpers,
            depth=depth,
        )
    if isinstance(expr, (ast.List, ast.Tuple)) and expr.elts:
        return _expand_helper_seq_expr(
            expr,
            caller_state,
            mod_state,
            helper_defs,
            helper_def_states,
            seen_helpers=seen_helpers,
            shadowed_names=shadowed_names,
            depth=depth,
        )
    if isinstance(expr, ast.BinOp) and isinstance(expr.op, ast.Add):
        l_vals, l_ov = _expand_helper_returns_in_expr(
            expr.left,
            caller_state,
            mod_state,
            helper_defs,
            helper_def_states,
            seen_helpers=seen_helpers,
            shadowed_names=shadowed_names,
            depth=depth + 1,
        )
        r_vals, r_ov = _expand_helper_returns_in_expr(
            expr.right,
            caller_state,
            mod_state,
            helper_defs,
            helper_def_states,
            seen_helpers=seen_helpers,
            shadowed_names=shadowed_names,
            depth=depth + 1,
        )
        rebuilt_bin = [
            ast.BinOp(left=lv, op=expr.op, right=rv) for lv in l_vals for rv in r_vals
        ]
        kept_bin, cap_ov = _compact_ast_candidates(rebuilt_bin)
        return (kept_bin, l_ov or r_ov or cap_ov)
    if (
        isinstance(expr, ast.Call)
        and expr.args
        and _unwrap_path_conversion(expr) is not expr
    ):
        inner_vals, inner_ov = _expand_helper_returns_in_expr(
            expr.args[0],
            caller_state,
            mod_state,
            helper_defs,
            helper_def_states,
            seen_helpers=seen_helpers,
            shadowed_names=shadowed_names,
            depth=depth + 1,
        )
        rebuilt_wrap = [
            ast.Call(func=expr.func, args=[iv, *expr.args[1:]], keywords=expr.keywords)
            for iv in inner_vals
        ]
        kept_wrap, cap_ov = _compact_ast_candidates(rebuilt_wrap)
        return (kept_wrap, inner_ov or cap_ov)
    return ([expr], False)


def _expr_mentions_helper_call(
    expr: ast.AST,
    helper_defs: Mapping[str, ast.FunctionDef | ast.AsyncFunctionDef],
) -> bool:
    """Return True when ``expr`` contains a direct call to any helper in ``helper_defs``."""
    return any(
        isinstance(node, ast.Call)
        and isinstance(node.func, ast.Name)
        and node.func.id in helper_defs
        for node in ast.walk(expr)
    )


def _resolve_subprocess_clis(
    cmd_arg: ast.AST,
    call_state: _BindingState,
    mod_state: _BindingState,
    helper_defs: Mapping[str, ast.FunctionDef | ast.AsyncFunctionDef],
    helper_def_states: Mapping[str, _BindingState],
    *,
    shadowed_names: frozenset[str] = frozenset(),
) -> tuple[set[str], bool]:
    """Resolve canonical SUEWS CLI command(s) invoked by ``cmd_arg`` and overflow flag."""
    ov_flag = [_expr_references_overflowed(cmd_arg, call_state.overflowed)]
    clis = _detect_suews_clis_in_expr(
        cmd_arg, call_state.env, overflow_flag=ov_flag
    )
    if not helper_defs:
        return (clis, ov_flag[0])

    subst_cands, subst_ov = _substitute_expr_values(cmd_arg, call_state)
    ov = ov_flag[0] or subst_ov
    for cand in subst_cands:
        if not _expr_mentions_helper_call(cand, helper_defs):
            continue
        expanded_cands, exp_ov = _expand_helper_returns_in_expr(
            cand,
            call_state,
            mod_state,
            helper_defs,
            helper_def_states,
            shadowed_names=shadowed_names,
        )
        ov = ov or exp_ov
        for exp in expanded_cands:
            exp_flag = [False]
            clis.update(
                _detect_suews_clis_in_expr(exp, {}, overflow_flag=exp_flag)
            )
            ov = ov or exp_flag[0]
    return (clis, ov)


def _extract_param_defaults(
    fn: ast.FunctionDef | ast.AsyncFunctionDef,
    mod_state: _BindingState,
) -> dict[str, list[ast.AST]]:
    """Extract eagerly substituted default-argument expressions for ``fn``."""
    return _extract_args_defaults(fn.args, mod_state)


def _scan_single_helper(
    stmt: ast.FunctionDef | ast.AsyncFunctionDef,
    mod_state: _BindingState,
    def_state: _BindingState,
    subproc_aliases: set[str],
    subproc_funcs: set[str],
    helper_defs: Mapping[str, ast.FunctionDef | ast.AsyncFunctionDef],
    helper_def_states: Mapping[str, _BindingState],
) -> tuple[
    dict[str, int],
    dict[str, list[ast.AST]],
    set[str],
    set[str],
    set[str],
    bool,
    list[tuple[ast.Call, _BindingState]],
]:
    """Inspect one module-level helper in statement order for CLI calls, params, and overflow."""
    pos_args = [
        *(arg.arg for arg in stmt.args.posonlyargs),
        *(arg.arg for arg in stmt.args.args),
    ]
    param_pos = {name: idx for idx, name in enumerate(pos_args)}
    all_params = set(param_pos) | {arg.arg for arg in stmt.args.kwonlyargs}
    param_defaults = _extract_param_defaults(stmt, def_state)
    entry_state = mod_state.clone()
    for p_name in all_params:
        entry_state.env.pop(p_name, None)
        entry_state.overflowed.discard(p_name)

    recorded: list[tuple[ast.Call, _BindingState]] = []
    _record_calls_in_stmt_order(stmt.body, entry_state, recorded)

    clis: set[str] = set()
    forwarded: set[str] = set()
    helper_overflow = False
    helper_calls: list[tuple[ast.Call, _BindingState]] = []
    shadowed = frozenset(_args_param_names(stmt.args))
    for call, call_state in recorded:
        if _is_subprocess_call(call, subproc_aliases, subproc_funcs):
            cmd_arg = _extract_subprocess_cmd_arg(call)
            if cmd_arg is None:
                continue
            found_clis, ov = _resolve_subprocess_clis(
                cmd_arg,
                call_state,
                mod_state,
                helper_defs,
                helper_def_states,
                shadowed_names=shadowed,
            )
            clis.update(found_clis)
            helper_overflow = helper_overflow or ov
            forwarded.update(
                _extract_forwarded_param_names(cmd_arg, call_state.env, all_params)
            )
        elif isinstance(call.func, ast.Name):
            helper_calls.append((call, call_state))
    return (
        param_pos,
        param_defaults,
        all_params,
        clis,
        forwarded,
        helper_overflow,
        helper_calls,
    )


def _bound_call_arg_candidates(
    call: ast.Call,
    param_name: str,
    param_pos: Mapping[str, int],
    param_defaults: Mapping[str, list[ast.AST]],
) -> list[ast.AST]:
    """Return AST expression(s) bound to ``param_name`` at ``call`` (falling back to default)."""
    idx = param_pos.get(param_name)
    if idx is not None and idx < len(call.args):
        return [call.args[idx]]
    for kw in call.keywords:
        if kw.arg == param_name:
            return [kw.value]
    return list(param_defaults.get(param_name, ()))


def _propagate_single_helper_call(
    helper_name: str,
    call: ast.Call,
    call_state: _BindingState,
    helper_clis: dict[str, set[str]],
    helper_forwarded: dict[str, set[str]],
    helper_overflows: dict[str, bool],
    helper_param_positions: Mapping[str, dict[str, int]],
    helper_param_defaults: Mapping[str, dict[str, list[ast.AST]]],
    helper_all_params: Mapping[str, set[str]],
) -> bool:
    """Propagate callee CLIs, overflow, and forwarded params for one helper call."""
    callee = call.func.id if isinstance(call.func, ast.Name) else ""
    if callee not in helper_clis:
        return False
    before = (
        len(helper_clis[helper_name]),
        len(helper_forwarded[helper_name]),
        helper_overflows[helper_name],
    )
    helper_clis[helper_name].update(helper_clis[callee])
    if helper_overflows.get(callee):
        helper_overflows[helper_name] = True
    callee_pos = helper_param_positions.get(callee, {})
    callee_defs = helper_param_defaults.get(callee, {})
    for param_name in helper_forwarded.get(callee, ()):
        for arg_expr in _bound_call_arg_candidates(
            call, param_name, callee_pos, callee_defs
        ):
            ov_flag = [_expr_references_overflowed(arg_expr, call_state.overflowed)]
            helper_clis[helper_name].update(
                _detect_suews_clis_in_expr(
                    arg_expr, call_state.env, overflow_flag=ov_flag
                )
            )
            if ov_flag[0]:
                helper_overflows[helper_name] = True
            helper_forwarded[helper_name].update(
                _extract_forwarded_param_names(
                    arg_expr, call_state.env, helper_all_params[helper_name]
                )
            )
    after = (
        len(helper_clis[helper_name]),
        len(helper_forwarded[helper_name]),
        helper_overflows[helper_name],
    )
    return after != before


def _propagate_helper_clis(
    helper_clis: dict[str, set[str]],
    helper_forwarded: dict[str, set[str]],
    helper_overflows: dict[str, bool],
    helper_param_positions: Mapping[str, dict[str, int]],
    helper_param_defaults: Mapping[str, dict[str, list[ast.AST]]],
    helper_all_params: Mapping[str, set[str]],
    helper_calls: Mapping[str, list[tuple[ast.Call, _BindingState]]],
) -> None:
    """Propagate SUEWS CLI sets, overflow flags, and forwarded params across helper chains."""
    changed = True
    while changed:
        changed = False
        for helper_name, calls in helper_calls.items():
            for call, call_state in calls:
                if _propagate_single_helper_call(
                    helper_name,
                    call,
                    call_state,
                    helper_clis,
                    helper_forwarded,
                    helper_overflows,
                    helper_param_positions,
                    helper_param_defaults,
                    helper_all_params,
                ):
                    changed = True


def _analyse_module_helpers(
    tree: ast.Module,
    mod_state: _BindingState,
    helper_def_states: Mapping[str, _BindingState],
    subproc_aliases: set[str],
    subproc_funcs: set[str],
    helper_defs: Mapping[str, ast.FunctionDef | ast.AsyncFunctionDef],
) -> tuple[
    dict[str, set[str]],
    dict[str, dict[str, int]],
    dict[str, dict[str, list[ast.AST]]],
    dict[str, set[str]],
    dict[str, bool],
]:
    """Analyse module-level non-test helpers for SUEWS CLI subprocess calls and overflows.

    Returns ``(helper_direct_clis, helper_param_positions, helper_param_defaults,
    helper_forwarded_params, helper_overflows)``.
    """
    helper_clis: dict[str, set[str]] = {}
    helper_calls: dict[str, list[tuple[ast.Call, _BindingState]]] = {}
    helper_param_positions: dict[str, dict[str, int]] = {}
    helper_param_defaults: dict[str, dict[str, list[ast.AST]]] = {}
    helper_all_params: dict[str, set[str]] = {}
    helper_forwarded: dict[str, set[str]] = {}
    helper_overflows: dict[str, bool] = {}

    for stmt in tree.body:
        if isinstance(
            stmt, (ast.FunctionDef, ast.AsyncFunctionDef)
        ) and not _is_pytest_collected_function(stmt):
            (
                param_pos,
                param_defs,
                all_params,
                clis,
                forwarded,
                helper_ov,
                calls,
            ) = _scan_single_helper(
                stmt,
                mod_state,
                helper_def_states.get(stmt.name, mod_state),
                subproc_aliases,
                subproc_funcs,
                helper_defs,
                helper_def_states,
            )
            helper_param_positions[stmt.name] = param_pos
            helper_param_defaults[stmt.name] = param_defs
            helper_all_params[stmt.name] = all_params
            helper_clis[stmt.name] = clis
            helper_forwarded[stmt.name] = forwarded
            helper_overflows[stmt.name] = helper_ov
            helper_calls[stmt.name] = calls

    _propagate_helper_clis(
        helper_clis,
        helper_forwarded,
        helper_overflows,
        helper_param_positions,
        helper_param_defaults,
        helper_all_params,
        helper_calls,
    )
    return (
        helper_clis,
        helper_param_positions,
        helper_param_defaults,
        helper_forwarded,
        helper_overflows,
    )


def _extract_parametrize_argnames(node: ast.AST) -> list[str]:
    """Extract parameter names from the first argument of ``@pytest.mark.parametrize``."""
    if isinstance(node, ast.Constant) and isinstance(node.value, str):
        return [part.strip() for part in node.value.split(",") if part.strip()]
    if isinstance(node, (ast.List, ast.Tuple)):
        return [
            elt.value.strip()
            for elt in node.elts
            if isinstance(elt, ast.Constant)
            and isinstance(elt.value, str)
            and elt.value.strip()
        ]
    return []


def _eval_static_bool(node: ast.AST) -> bool | None:
    """Evaluate a literal boolean expression when statically known, else return ``None``."""
    result: bool | None = None
    if isinstance(node, ast.Constant):
        result = bool(node.value)
    elif isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.Not):
        inner = _eval_static_bool(node.operand)
        if inner is not None:
            result = not inner
    elif isinstance(node, ast.BoolOp):
        vals = [_eval_static_bool(v) for v in node.values]
        if isinstance(node.op, ast.And):
            if False in vals:
                result = False
            elif all(v is True for v in vals):
                result = True
        elif isinstance(node.op, ast.Or):
            if True in vals:
                result = True
            elif all(v is False for v in vals):
                result = False
    return result


def _extract_guaranteed_param_marks(
    node: ast.AST,
    *,
    mod_env: Mapping[str, ast.AST],
    seen: frozenset[str] = frozenset(),
) -> set[str]:
    """Return marker names guaranteed to apply on every reachable branch of ``node``."""
    direct = _direct_pytest_marker_name(node)
    result: set[str] = set()
    if direct is not None:
        result = {direct}
    elif isinstance(node, (ast.List, ast.Tuple, ast.Set)):
        for elt in node.elts:
            result.update(
                _extract_guaranteed_param_marks(elt, mod_env=mod_env, seen=seen)
            )
    elif isinstance(node, ast.IfExp):
        cond = _eval_static_bool(node.test)
        if cond is True:
            result = _extract_guaranteed_param_marks(
                node.body, mod_env=mod_env, seen=seen
            )
        elif cond is False:
            result = _extract_guaranteed_param_marks(
                node.orelse, mod_env=mod_env, seen=seen
            )
        else:
            result = _extract_guaranteed_param_marks(
                node.body, mod_env=mod_env, seen=seen
            ) & _extract_guaranteed_param_marks(
                node.orelse, mod_env=mod_env, seen=seen
            )
    elif isinstance(node, ast.Name) and node.id in mod_env and node.id not in seen:
        result = _extract_guaranteed_param_marks(
            mod_env[node.id], mod_env=mod_env, seen=seen | {node.id}
        )
    return result


def _parse_parametrize_case_item(
    raw_item: ast.AST,
    param_names: list[str],
    mod_env: Mapping[str, ast.AST],
) -> tuple[dict[str, ast.AST], set[str]]:
    """Parse one element of ``@pytest.mark.parametrize`` ``argvalues`` into ``(bindings, marks)``."""
    case_marks: set[str] = set()
    if isinstance(raw_item, ast.Call) and _is_pytest_param_call(raw_item):
        case_values = list(raw_item.args)
        for kw in raw_item.keywords:
            if kw.arg == "marks":
                case_marks.update(
                    _extract_guaranteed_param_marks(kw.value, mod_env=mod_env)
                )
    elif len(param_names) > 1 and isinstance(raw_item, (ast.Tuple, ast.List)):
        case_values = list(raw_item.elts)
    else:
        case_values = [raw_item]
    bindings: dict[str, ast.AST] = {}
    if len(param_names) == 1 and case_values:
        bindings[param_names[0]] = case_values[0]
    elif len(param_names) > 1 and len(case_values) == len(param_names):
        bindings = dict(zip(param_names, case_values))
    return (bindings, case_marks)


def _normalise_parametrize_call(
    dec: ast.AST,
    mod_env: Mapping[str, ast.AST],
    ast_value_env: Mapping[str, ast.AST],
) -> tuple[bool, ast.Call | None]:
    """Return ``(is_parametrize, call_node)`` handling ``.with_args(...)`` chains."""
    resolved = dec
    if isinstance(resolved, ast.Name) and resolved.id in ast_value_env:
        resolved = ast_value_env[resolved.id]
    if isinstance(resolved, ast.Call):
        if _direct_pytest_marker_name(resolved) == "parametrize":
            return (True, resolved)
        if (
            isinstance(resolved.func, ast.Attribute)
            and resolved.func.attr == "with_args"
            and _direct_pytest_marker_name(resolved.func.value) == "parametrize"
        ):
            return (True, resolved)
    dec_marks, _ = _inspect_decorator_expr(dec, mod_env)
    if "parametrize" in dec_marks:
        # Recognised chained/aliased parametrize form we cannot safely unpack: fail closed.
        return (True, None)
    return (False, None)


def _parse_single_parametrize_decorator(
    dec: ast.AST,
    mod_env: Mapping[str, ast.AST],
    ast_value_env: Mapping[str, ast.AST],
) -> tuple[bool, list[str], list[tuple[dict[str, ast.AST], set[str]]] | None]:
    """Parse one ``@pytest.mark.parametrize`` decorator into ``(is_param, names, cases)``."""
    is_param, resolved = _normalise_parametrize_call(dec, mod_env, ast_value_env)
    if not is_param:
        return (False, [], None)
    if resolved is None:
        return (True, [], None)

    argnames_node: ast.AST | None = resolved.args[0] if resolved.args else None
    argvalues_node: ast.AST | None = resolved.args[1] if len(resolved.args) >= 2 else None
    for kw in resolved.keywords:
        if kw.arg == "argnames" and argnames_node is None:
            argnames_node = kw.value
        elif kw.arg == "argvalues" and argvalues_node is None:
            argvalues_node = kw.value

    param_names = (
        _extract_parametrize_argnames(argnames_node) if argnames_node is not None else []
    )
    if argvalues_node is None:
        return (True, param_names, None)
    raw_items = _expand_static_sequence(argvalues_node, ast_value_env)
    if raw_items is None:
        return (True, param_names, None)
    cases = [
        _parse_parametrize_case_item(item, param_names, mod_env) for item in raw_items
    ]
    return (True, param_names, cases)


def _expand_parametrize_cases(
    decorators: list[ast.AST],
    mod_env: Mapping[str, ast.AST],
    ast_value_env: Mapping[str, ast.AST],
) -> tuple[list[tuple[dict[str, ast.AST], set[str]]], bool, set[str]]:
    """Expand ``@pytest.mark.parametrize`` into ``(cases, unresolvable, unresolvable_names)``."""
    cases: list[tuple[dict[str, ast.AST], set[str]]] = [({}, set())]
    unresolvable = False
    unresolvable_names: set[str] = set()
    for dec in decorators:
        is_param, param_names, dec_cases = _parse_single_parametrize_decorator(
            dec, mod_env, ast_value_env
        )
        if not is_param:
            continue
        if dec_cases is None:
            unresolvable = True
            unresolvable_names.update(param_names)
            continue
        if not dec_cases:
            return ([], False, set())
        if len(cases) * len(dec_cases) > _MAX_EXPANDED_CASES:
            unresolvable = True
            unresolvable_names.update(param_names)
            continue
        cases = [
            ({**base_bind, **new_bind}, base_marks | new_marks)
            for base_bind, base_marks in cases
            for new_bind, new_marks in dec_cases
        ]
    return (cases, unresolvable, unresolvable_names)


class _NonSimState:
    """Must-hold non-simulation receiver bindings along a control-flow path."""

    def __init__(
        self,
        mods: set[str] | frozenset[str],
        classes: set[str] | frozenset[str],
        local_vars: set[str] | frozenset[str],
        shadowed: set[str] | frozenset[str],
    ) -> None:
        self.mods: set[str] = set(mods)
        self.classes: set[str] = set(classes)
        self.local_vars: set[str] = set(local_vars)
        self.shadowed: set[str] = set(shadowed)

    def clone(self) -> _NonSimState:
        """Return a shallow copy of this must-hold state."""
        return _NonSimState(self.mods, self.classes, self.local_vars, self.shadowed)

    def kill(self, names: set[str]) -> None:
        """Invalidate non-sim provenance for ``names`` and mark them shadowed."""
        self.mods.difference_update(names)
        self.classes.difference_update(names)
        self.local_vars.difference_update(names)
        self.shadowed.update(names)

    def intersect_with(self, other: _NonSimState) -> None:
        """Keep only bindings proven non-sim on both ``self`` and ``other``."""
        self.mods.intersection_update(other.mods)
        self.classes.intersection_update(other.classes)
        self.local_vars.intersection_update(other.local_vars)
        self.shadowed.update(other.shadowed)


def _count_sim_calls_in_comp(
    comp: ast.ListComp | ast.SetComp | ast.GeneratorExp | ast.DictComp,
    state: _NonSimState,
    enclosing_state: _NonSimState | None,
) -> int:
    """Count simulation calls in a comprehension while shadowing generator targets."""
    walrus_target = state if enclosing_state is None else enclosing_state
    inner = state.clone()
    count = 0
    for idx, gen in enumerate(comp.generators):
        iter_state = state if idx == 0 else inner
        count += _count_sim_calls_in_expr(gen.iter, iter_state, walrus_target)
        inner.kill(set(_iter_assigned_names(gen.target)))
        for cond in gen.ifs:
            count += _count_sim_calls_in_expr(cond, inner, walrus_target)
    if isinstance(comp, ast.DictComp):
        count += _count_sim_calls_in_expr(comp.key, inner, walrus_target)
        count += _count_sim_calls_in_expr(comp.value, inner, walrus_target)
    else:
        count += _count_sim_calls_in_expr(comp.elt, inner, walrus_target)
    return count


def _count_sim_calls_in_expr(
    expr: ast.AST,
    state: _NonSimState,
    enclosing_state: _NonSimState | None = None,
) -> int:
    """Count simulation ``from_sample_data`` and ``.run`` calls in ``expr``."""
    if isinstance(expr, (ast.ListComp, ast.SetComp, ast.GeneratorExp, ast.DictComp)):
        return _count_sim_calls_in_comp(expr, state, enclosing_state)
    if isinstance(expr, ast.Lambda):
        count = sum(
            _count_sim_calls_in_expr(d, state, enclosing_state)
            for d in expr.args.defaults
        )
        count += sum(
            _count_sim_calls_in_expr(d, state, enclosing_state)
            for d in expr.args.kw_defaults
            if d is not None
        )
        inner = state.clone()
        inner.kill(_args_param_names(expr.args))
        return count + _count_sim_calls_in_expr(expr.body, inner, None)
    if isinstance(expr, ast.NamedExpr):
        count = _count_sim_calls_in_expr(expr.value, state, enclosing_state)
        state.kill({expr.target.id})
        if enclosing_state is not None:
            enclosing_state.kill({expr.target.id})
        return count
    if isinstance(expr, ast.IfExp):
        count = _count_sim_calls_in_expr(expr.test, state, enclosing_state)
        b_state = state.clone()
        e_state = state.clone()
        count += _count_sim_calls_in_expr(expr.body, b_state, enclosing_state)
        count += _count_sim_calls_in_expr(expr.orelse, e_state, enclosing_state)
        b_state.intersect_with(e_state)
        state.mods = b_state.mods
        state.classes = b_state.classes
        state.local_vars = b_state.local_vars
        state.shadowed = b_state.shadowed
        return count
    if isinstance(expr, ast.Call):
        count = _count_sim_calls_in_expr(expr.func, state, enclosing_state)
        if isinstance(expr.func, ast.Attribute) and (
            expr.func.attr == "from_sample_data"
            or (
                expr.func.attr == "run"
                and not _is_non_simulation_run_receiver(
                    expr.func.value,
                    frozenset(state.mods),
                    frozenset(state.classes),
                    frozenset(state.local_vars),
                    frozenset(state.shadowed),
                )
            )
        ):
            count += 1
        for arg in expr.args:
            count += _count_sim_calls_in_expr(arg, state, enclosing_state)
        for kw in expr.keywords:
            count += _count_sim_calls_in_expr(kw.value, state, enclosing_state)
        return count
    return sum(
        _count_sim_calls_in_expr(child, state, enclosing_state)
        for child in ast.iter_child_nodes(expr)
    )


def _apply_local_non_sim_import(
    stmt: ast.Import | ast.ImportFrom,
    state: _NonSimState,
) -> None:
    """Update local must-hold non-sim bindings for an in-function import."""
    if isinstance(stmt, ast.Import):
        for alias in stmt.names:
            local_name = alias.asname or alias.name.split(".", 1)[0]
            root_mod = alias.name.split(".", 1)[0]
            state.kill({local_name})
            if root_mod in _NON_SIMULATION_RUN_MODULES:
                state.mods.add(local_name)
                state.shadowed.discard(local_name)
    else:
        root_mod = stmt.module.split(".", 1)[0] if stmt.module else ""
        is_non_sim_mod = root_mod in _NON_SIMULATION_RUN_MODULES
        for alias in stmt.names:
            if alias.name == "*":
                continue
            local_name = alias.asname or alias.name
            state.kill({local_name})
            if is_non_sim_mod and alias.name in _NON_SIMULATION_RUN_CLASSES:
                state.classes.add(local_name)
                state.shadowed.discard(local_name)


def _apply_local_non_sim_assign(
    target: ast.AST,
    value: ast.AST,
    state: _NonSimState,
) -> None:
    """Update local must-hold non-sim bindings for an assignment target."""
    is_ctor = isinstance(target, ast.Name) and _is_non_sim_constructor_call(
        value,
        frozenset(state.mods),
        frozenset(state.classes),
        frozenset(state.shadowed),
    )
    assigned = set(_iter_assigned_names(target))
    state.kill(assigned)
    if is_ctor and isinstance(target, ast.Name):
        state.local_vars.add(target.id)
        state.shadowed.discard(target.id)


def _func_param_names(fn: ast.FunctionDef | ast.AsyncFunctionDef) -> set[str]:
    """Return all parameter names bound by ``fn``."""
    return _args_param_names(fn.args)


def _walk_sim_calls_in_block(
    stmts: list[ast.stmt],
    state: _NonSimState,
) -> int:
    """Count simulation calls in statement order with sound must-hold shadowing."""
    count = 0
    for stmt in stmts:
        if isinstance(stmt, (ast.Import, ast.ImportFrom)):
            _apply_local_non_sim_import(stmt, state)
        elif isinstance(stmt, ast.Assign):
            count += _count_sim_calls_in_expr(stmt.value, state)
            for target in stmt.targets:
                _apply_local_non_sim_assign(target, stmt.value, state)
        elif isinstance(stmt, ast.AnnAssign):
            if stmt.value is not None:
                count += _count_sim_calls_in_expr(stmt.value, state)
                _apply_local_non_sim_assign(stmt.target, stmt.value, state)
        elif isinstance(stmt, ast.AugAssign):
            count += _count_sim_calls_in_expr(stmt.value, state)
            state.kill(set(_iter_assigned_names(stmt.target)))
        elif isinstance(stmt, ast.Delete):
            for target in stmt.targets:
                state.kill(set(_iter_assigned_names(target)))
        elif isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef)):
            state.kill({stmt.name})
            nested = state.clone()
            nested.kill(_func_param_names(stmt))
            count += _walk_sim_calls_in_block(stmt.body, nested)
        elif isinstance(stmt, ast.ClassDef):
            state.kill({stmt.name})
            count += _walk_sim_calls_in_block(stmt.body, state.clone())
        else:
            count += _walk_sim_calls_in_compound(stmt, state)
    return count


def _walk_sim_calls_in_loop(
    stmt: ast.For | ast.AsyncFor | ast.While,
    state: _NonSimState,
) -> int:
    """Walk a loop soundly: zero-iteration paths cannot establish new non-sim vars."""
    header = stmt.iter if isinstance(stmt, (ast.For, ast.AsyncFor)) else stmt.test
    cnt = _count_sim_calls_in_expr(header, state)
    pre_state = state.clone()
    written_in_loop = _collect_written_names_in_stmt(stmt)
    if isinstance(stmt, (ast.For, ast.AsyncFor)):
        written_in_loop.update(_iter_assigned_names(stmt.target))

    # Any name overwritten inside the loop might not hold its pre-loop non-sim
    # binding on iteration 2+, so invalidate before walking the loop body.
    body_state = pre_state.clone()
    body_state.kill(written_in_loop)
    cnt += _walk_sim_calls_in_block(stmt.body, body_state)

    # Zero-iteration path means post-loop state cannot gain bindings created only in body.
    after_state = pre_state.clone()
    after_state.intersect_with(body_state)
    if stmt.orelse:
        else_state = after_state.clone()
        cnt += _walk_sim_calls_in_block(stmt.orelse, else_state)
        after_state.intersect_with(else_state)

    state.mods = after_state.mods
    state.classes = after_state.classes
    state.local_vars = after_state.local_vars
    state.shadowed = after_state.shadowed
    return cnt


def _collect_written_names_in_stmts(stmts: list[ast.stmt]) -> set[str]:
    """Return variable/import/def names bound inside ``stmts`` (excluding nested defs' bodies)."""
    return _collect_written_names_in_stmt(ast.Module(body=stmts, type_ignores=[]))


def _walk_sim_calls_in_try(
    stmt: ast.stmt,
    state: _NonSimState,
) -> int:
    """Walk a try/except/except*/else/finally block soundly across exception points."""
    body: list[ast.stmt] = getattr(stmt, "body", [])
    orelse: list[ast.stmt] = getattr(stmt, "orelse", [])
    handlers: list[ast.ExceptHandler] = getattr(stmt, "handlers", [])
    finalbody: list[ast.stmt] = getattr(stmt, "finalbody", [])

    pre_state = state.clone()
    try_state = pre_state.clone()
    cnt = _walk_sim_calls_in_block(body, try_state)

    # An exception may occur at any intermediate point inside ``body``, so
    # any binding written within the ``try`` block cannot be assumed non-sim on
    # exception entry (even if restored later in the ``try`` body).
    written_in_try = _collect_written_names_in_stmts(body)
    handler_entry = pre_state.clone()
    handler_entry.kill(written_in_try)
    exit_states: list[_NonSimState] = []

    if orelse:
        else_state = try_state.clone()
        cnt += _walk_sim_calls_in_block(orelse, else_state)
        exit_states.append(else_state)
    else:
        exit_states.append(try_state)

    for handler in handlers:
        h_state = handler_entry.clone()
        if handler.name:
            h_state.kill({handler.name})
        cnt += _walk_sim_calls_in_block(handler.body, h_state)
        exit_states.append(h_state)

    merged = exit_states[0].clone()
    for other in exit_states[1:]:
        merged.intersect_with(other)

    if finalbody:
        # An unhandled exception type can jump directly from try_body to finally.
        finally_state = merged.clone()
        finally_state.intersect_with(handler_entry)
        cnt += _walk_sim_calls_in_block(finalbody, finally_state)
        merged = finally_state

    state.mods = merged.mods
    state.classes = merged.classes
    state.local_vars = merged.local_vars
    state.shadowed = merged.shadowed
    return cnt


def _walk_sim_calls_in_compound(
    stmt: ast.stmt,
    state: _NonSimState,
) -> int:
    """Walk compound statements for simulation calls, requiring all-path non-sim proof."""
    if isinstance(stmt, ast.If):
        cnt = _count_sim_calls_in_expr(stmt.test, state)
        t_state = state.clone()
        e_state = state.clone()
        cnt += _walk_sim_calls_in_block(stmt.body, t_state)
        if stmt.orelse:
            cnt += _walk_sim_calls_in_block(stmt.orelse, e_state)
        t_state.intersect_with(e_state)
        state.mods = t_state.mods
        state.classes = t_state.classes
        state.local_vars = t_state.local_vars
        state.shadowed = t_state.shadowed
        return cnt
    if isinstance(stmt, (ast.For, ast.AsyncFor, ast.While)):
        return _walk_sim_calls_in_loop(stmt, state)
    if isinstance(stmt, (ast.With, ast.AsyncWith)):
        cnt = 0
        for item in stmt.items:
            cnt += _count_sim_calls_in_expr(item.context_expr, state)
            if item.optional_vars is not None:
                state.kill(set(_iter_assigned_names(item.optional_vars)))
        pre_body = state.clone()
        body_state = pre_body.clone()
        cnt += _walk_sim_calls_in_block(stmt.body, body_state)
        # A context manager's __exit__ may suppress an exception raised at any point in
        # the body, so post-with state must intersect normal completion with suppressed exits.
        suppressed_exit = pre_body.clone()
        suppressed_exit.kill(_collect_written_names_in_stmts(stmt.body))
        body_state.intersect_with(suppressed_exit)
        state.mods = body_state.mods
        state.classes = body_state.classes
        state.local_vars = body_state.local_vars
        state.shadowed = body_state.shadowed
        return cnt
    if isinstance(stmt, _TRY_STMT_TYPES):
        return _walk_sim_calls_in_try(stmt, state)
    if isinstance(stmt, ast.Match):
        cnt = _count_sim_calls_in_expr(stmt.subject, state)
        pre_state = state.clone()
        merged = pre_state.clone()
        for case in stmt.cases:
            c_state = pre_state.clone()
            c_state.kill(_collect_pattern_capture_names(case.pattern))
            if case.guard is not None:
                cnt += _count_sim_calls_in_expr(case.guard, c_state)
            cnt += _walk_sim_calls_in_block(case.body, c_state)
            merged.intersect_with(c_state)
        state.mods = merged.mods
        state.classes = merged.classes
        state.local_vars = merged.local_vars
        state.shadowed = merged.shadowed
        return cnt
    return _count_sim_calls_in_expr(stmt, state)


def _count_param_simulation_calls(
    fn: ast.FunctionDef | ast.AsyncFunctionDef,
    non_sim_mods: frozenset[str],
    non_sim_classes: frozenset[str],
    mod_non_sim_vars: frozenset[str] = frozenset(),
) -> int:
    """Count direct ``from_sample_data(...)`` and simulation ``.run(...)`` calls in ``fn``."""
    param_names = _func_param_names(fn)
    state = _NonSimState(non_sim_mods, non_sim_classes, mod_non_sim_vars, set())
    state.kill(param_names)
    return _walk_sim_calls_in_block(fn.body, state)


def _resolve_forwarded_helper_clis(
    call: ast.Call,
    forwarded_params: set[str],
    param_pos: Mapping[str, int],
    param_defaults: Mapping[str, list[ast.AST]],
    call_state: _BindingState,
) -> tuple[set[str], bool]:
    """Detect SUEWS CLI commands passed through forwarded helper parameters (or defaults)."""
    found: set[str] = set()
    overflowed = False
    for param_name in forwarded_params:
        for arg_expr in _bound_call_arg_candidates(
            call, param_name, param_pos, param_defaults
        ):
            ov_flag = [_expr_references_overflowed(arg_expr, call_state.overflowed)]
            found.update(
                _detect_suews_clis_in_expr(
                    arg_expr, call_state.env, overflow_flag=ov_flag
                )
            )
            overflowed = overflowed or ov_flag[0]
    return (found, overflowed)


def _build_test_entry_state(
    fn: ast.FunctionDef | ast.AsyncFunctionDef,
    mod_state: _BindingState,
    param_bindings: Mapping[str, list[ast.AST]] | None,
) -> _BindingState:
    """Create initial binding state for ``fn`` with parameters shadowed and seeded."""
    entry_state = mod_state.clone()
    for p_name in _func_param_names(fn):
        entry_state.env.pop(p_name, None)
        entry_state.overflowed.discard(p_name)
    if param_bindings:
        for p_name, p_vals in param_bindings.items():
            subst_list: list[ast.AST] = []
            ov = False
            for val in p_vals:
                sub_v, sub_ov = _substitute_expr_values(val, mod_state)
                subst_list.extend(sub_v)
                ov = ov or sub_ov
            kept, cap_ov = _compact_ast_candidates(subst_list)
            entry_state.env[p_name] = kept
            if ov or cap_ov:
                entry_state.overflowed.add(p_name)
    return entry_state


def _argv_head_references_names(
    cmd_arg: ast.AST,
    env: Mapping[str, list[ast.AST]],
    names: frozenset[str],
) -> bool:
    """Return True if an executable-bearing slot of ``cmd_arg`` references any name in ``names``."""
    if not names:
        return False
    for elems in _flatten_argv_variants(cmd_arg, env):
        if not elems:
            continue
        if _expr_references_overflowed(elems[0], set(names)):
            return True
        if _is_python_executable_expr(elems[0], env) and any(
            _expr_references_overflowed(el, set(names)) for el in elems[1:]
        ):
            return True
    return False


def _collect_test_subprocess_clis(
    fn: ast.FunctionDef | ast.AsyncFunctionDef,
    ctx: _FileAstGuardContext,
    param_bindings: Mapping[str, list[ast.AST]] | None = None,
    unresolvable_params: frozenset[str] = frozenset(),
) -> tuple[set[str], bool]:
    """Collect canonical SUEWS CLI commands invoked via subprocess by ``fn``."""
    entry_state = _build_test_entry_state(fn, ctx.mod_state, param_bindings)
    recorded: list[tuple[ast.Call, _BindingState]] = []
    _record_calls_in_stmt_order(fn.body, entry_state, recorded)

    found: set[str] = set()
    overflowed = False
    shadowed = frozenset(_func_param_names(fn))
    for call, call_state in recorded:
        if _is_subprocess_call(call, ctx.subproc_aliases, ctx.subproc_funcs):
            cmd_arg = _extract_subprocess_cmd_arg(call)
            if cmd_arg is not None:
                clis, ov = _resolve_subprocess_clis(
                    cmd_arg,
                    call_state,
                    ctx.mod_state,
                    ctx.helper_defs,
                    ctx.helper_def_states,
                    shadowed_names=shadowed,
                )
                found.update(clis)
                if ov or (
                    not clis
                    and _argv_head_references_names(
                        cmd_arg, call_state.env, unresolvable_params
                    )
                ):
                    overflowed = True
        elif isinstance(call.func, ast.Name) and call.func.id in ctx.helper_clis:
            callee = call.func.id
            found.update(ctx.helper_clis[callee])
            if ctx.helper_overflows.get(callee):
                overflowed = True
            forwarded = ctx.helper_forwarded.get(callee)
            if forwarded:
                fwd_clis, fwd_ov = _resolve_forwarded_helper_clis(
                    call,
                    forwarded,
                    ctx.helper_param_positions.get(callee, {}),
                    ctx.helper_param_defaults.get(callee, {}),
                    call_state,
                )
                found.update(fwd_clis)
                if fwd_ov or (
                    not fwd_clis
                    and unresolvable_params
                    and any(
                        _argv_head_references_names(
                            arg_expr, call_state.env, unresolvable_params
                        )
                        for p in forwarded
                        for arg_expr in _bound_call_arg_candidates(
                            call,
                            p,
                            ctx.helper_param_positions.get(callee, {}),
                            ctx.helper_param_defaults.get(callee, {}),
                        )
                    )
                ):
                    overflowed = True
    return (found, overflowed)


class _FileAstGuardContext:
    """Precomputed module-level analysis context for inspecting one test file."""

    def __init__(
        self,
        rel_posix: str,
        tree: ast.Module,
        extra_markers: Mapping[str, tuple[str, ...]],
    ) -> None:
        """Build module marker, alias, and helper tables from ``tree``."""
        self.rel_posix = rel_posix
        self.mod_env = _build_single_assign_env(tree)
        self.ast_value_env = _build_ast_value_env(tree)
        self.mod_state, self.helper_def_states = _build_module_binding_state(tree)
        mod_markers, _ = _extract_scope_pytestmark(tree.body)
        self.mod_pm_exprs = _extract_scope_pytestmark_exprs(tree.body)
        self.file_e2e = ("e2e" in mod_markers) or (
            "e2e" in extra_markers.get(rel_posix, ())
        )
        self.file_param = "parametrize" in mod_markers
        self.subproc_aliases, self.subproc_funcs = _collect_subprocess_imports(tree)
        (
            self.non_sim_mods,
            self.non_sim_classes,
            self.mod_non_sim_vars,
        ) = _collect_module_non_sim_bindings(tree)
        self.helper_defs = _collect_helper_defs(tree)
        (
            self.helper_clis,
            self.helper_param_positions,
            self.helper_param_defaults,
            self.helper_forwarded,
            self.helper_overflows,
        ) = _analyse_module_helpers(
            tree,
            self.mod_state,
            self.helper_def_states,
            self.subproc_aliases,
            self.subproc_funcs,
            self.helper_defs,
        )
        self.per_node_e2e_counts: dict[tuple[str, str], int] = {}


def _is_non_cli_scalar_constant(node: ast.AST) -> bool:
    """Return True when ``node`` is a scalar constant that can never name a CLI command."""
    return isinstance(node, ast.Constant) and not isinstance(node.value, str)


def _referenced_load_names(fn: ast.FunctionDef | ast.AsyncFunctionDef) -> set[str]:
    """Return variable names loaded anywhere inside ``fn.body``."""
    names: set[str] = set()
    for stmt in fn.body:
        for node in ast.walk(stmt):
            if isinstance(node, ast.Name) and isinstance(node.ctx, ast.Load):
                names.add(node.id)
    return names


def _group_parametrize_cases(
    cases: list[tuple[dict[str, ast.AST], set[str]]],
    base_is_e2e: bool,
    referenced_names: set[str],
) -> list[tuple[bool, dict[str, list[ast.AST]], int]]:
    """Group expanded cases into ``(case_is_e2e, param_bindings, weight)`` buckets."""
    grouped: dict[
        tuple[bool, tuple[tuple[str, str], ...]],
        tuple[bool, dict[str, list[ast.AST]], int],
    ] = {}
    for bindings, marks in cases:
        case_e2e = base_is_e2e or ("e2e" in marks)
        relevant = {
            k: v
            for k, v in bindings.items()
            if k in referenced_names and not _is_non_cli_scalar_constant(v)
        }
        key = (case_e2e, tuple((k, ast.dump(v)) for k, v in sorted(relevant.items())))
        if key in grouped:
            prev_e2e, prev_bind, prev_count = grouped[key]
            grouped[key] = (prev_e2e, prev_bind, prev_count + 1)
        else:
            grouped[key] = (case_e2e, {k: [v] for k, v in relevant.items()}, 1)

    groups = list(grouped.values())
    if len(groups) <= _MAX_BRANCH_BINDINGS:
        return groups

    # Merge large distinct-string parameter spaces into at most 2 buckets (e2e / non-e2e).
    merged_buckets: dict[bool, tuple[dict[str, list[ast.AST]], int]] = {}
    for case_e2e, bind_map, weight in groups:
        if case_e2e not in merged_buckets:
            merged_buckets[case_e2e] = ({k: list(v) for k, v in bind_map.items()}, weight)
        else:
            cur_map, cur_w = merged_buckets[case_e2e]
            for k, v in bind_map.items():
                cur_map.setdefault(k, []).extend(v)
            merged_buckets[case_e2e] = (cur_map, cur_w + weight)
    return [(e2e, bmap, w) for e2e, (bmap, w) in merged_buckets.items()]


def _record_function_subprocess_results(
    ctx: _FileAstGuardContext,
    snapshot: AstGuardsSnapshot,
    fn: ast.FunctionDef | ast.AsyncFunctionDef,
    node_id: str,
    all_decs: list[ast.AST],
    *,
    base_is_e2e: bool,
) -> None:
    """Evaluate subprocess CLI invocations per expanded case and update ``snapshot``."""
    cases, has_unresolvable, unresolvable_names = _expand_parametrize_cases(
        all_decs, ctx.mod_env, ctx.ast_value_env
    )
    if not cases:
        return
    groups = _group_parametrize_cases(
        cases, base_is_e2e, _referenced_load_names(fn)
    )

    unmarked_clis: set[str] = set()
    e2e_cli_counts: dict[str, int] = {}
    overflow_clis: set[str] = set()
    any_overflow = False
    unres_frozen = frozenset(unresolvable_names)

    for case_e2e, bind_map, weight in groups:
        clis, overflowed = _collect_test_subprocess_clis(
            fn, ctx, bind_map, unresolvable_params=unres_frozen
        )
        if overflowed:
            any_overflow = True
            overflow_clis.update(clis)
        if not clis:
            continue
        if case_e2e:
            for cli in clis:
                e2e_cli_counts[cli] = e2e_cli_counts.get(cli, 0) + weight
        else:
            unmarked_clis.update(clis)

    if any_overflow:
        existing_ov = set(snapshot.subprocess_analysis_overflow.get(node_id, ()))
        snapshot.subprocess_analysis_overflow[node_id] = sorted(
            existing_ov | overflow_clis | ({"<unknown>"} if not overflow_clis else set())
        )
    if unmarked_clis:
        prev_unmarked = set(snapshot.unmarked_subprocess_e2e.get(node_id, ()))
        snapshot.unmarked_subprocess_e2e[node_id] = sorted(prev_unmarked | unmarked_clis)
    if e2e_cli_counts:
        if has_unresolvable:
            prev_unres = set(snapshot.unresolvable_e2e_parametrize.get(node_id, ()))
            snapshot.unresolvable_e2e_parametrize[node_id] = sorted(
                prev_unres | set(e2e_cli_counts)
            )
        else:
            for cli, count in e2e_cli_counts.items():
                key = (node_id, cli)
                ctx.per_node_e2e_counts[key] = max(
                    ctx.per_node_e2e_counts.get(key, 0), count
                )


def _visit_ast_guard_function(
    ctx: _FileAstGuardContext,
    snapshot: AstGuardsSnapshot,
    fn: ast.FunctionDef | ast.AsyncFunctionDef,
    class_chain: tuple[str, ...],
    enclosing_decs: list[ast.AST],
    *,
    inherited_param: bool,
    inherited_e2e: bool,
) -> None:
    """Record AST guard hits for a single test function or method."""
    if not _is_pytest_collected_function(fn):
        return
    fn_marks: set[str] = set()
    for dec in fn.decorator_list:
        d_names, _ = _inspect_decorator_expr(dec, ctx.mod_env)
        fn_marks.update(d_names)
    is_param = ctx.file_param or inherited_param or ("parametrize" in fn_marks)
    base_is_e2e = ctx.file_e2e or inherited_e2e or ("e2e" in fn_marks)
    node_id = "::".join((ctx.rel_posix, *class_chain, fn.name))
    if is_param:
        sim_calls = _count_param_simulation_calls(
            fn, ctx.non_sim_mods, ctx.non_sim_classes, ctx.mod_non_sim_vars
        )
        if sim_calls > 0:
            snapshot.parametrized_simulations[node_id] = max(
                snapshot.parametrized_simulations.get(node_id, 0), sim_calls
            )
    all_decs = [*ctx.mod_pm_exprs, *enclosing_decs, *fn.decorator_list]
    _record_function_subprocess_results(
        ctx, snapshot, fn, node_id, all_decs, base_is_e2e=base_is_e2e
    )


def _is_skipped_static_if_test(test: ast.AST) -> bool:
    """Return True for dead/type-checking-only guards (``if False:``, ``if TYPE_CHECKING:``)."""
    if isinstance(test, ast.Constant) and test.value is False:
        return True
    if isinstance(test, ast.Name) and test.id == "TYPE_CHECKING":
        return True
    return isinstance(test, ast.Attribute) and test.attr == "TYPE_CHECKING"


def _iter_scope_defs(
    stmts: list[ast.stmt],
) -> list[ast.FunctionDef | ast.AsyncFunctionDef | ast.ClassDef]:
    """Collect function and class defs in ``stmts``, descending into conditional blocks."""
    found: list[ast.FunctionDef | ast.AsyncFunctionDef | ast.ClassDef] = []
    for stmt in stmts:
        if isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            found.append(stmt)
        elif isinstance(stmt, ast.If):
            if not _is_skipped_static_if_test(stmt.test):
                found.extend(_iter_scope_defs(stmt.body))
            if stmt.orelse:
                found.extend(_iter_scope_defs(stmt.orelse))
        elif isinstance(stmt, (ast.For, ast.AsyncFor, ast.While, ast.With, ast.AsyncWith)):
            found.extend(_iter_scope_defs(stmt.body))
            if isinstance(stmt, (ast.For, ast.AsyncFor, ast.While)) and stmt.orelse:
                found.extend(_iter_scope_defs(stmt.orelse))
        elif isinstance(stmt, _TRY_STMT_TYPES):
            found.extend(_iter_scope_defs(getattr(stmt, "body", [])))
            for handler in getattr(stmt, "handlers", []):
                found.extend(_iter_scope_defs(handler.body))
            found.extend(_iter_scope_defs(getattr(stmt, "orelse", [])))
            found.extend(_iter_scope_defs(getattr(stmt, "finalbody", [])))
        elif isinstance(stmt, ast.Match):
            for case in stmt.cases:
                found.extend(_iter_scope_defs(case.body))
    return found


def _extract_class_test_flag(cls: ast.ClassDef) -> bool | None:
    """Return literal ``__test__`` boolean assigned directly on ``cls``, or ``None``."""
    result: bool | None = None
    for stmt in cls.body:
        if isinstance(stmt, ast.Assign):
            for target in stmt.targets:
                if (
                    isinstance(target, ast.Name)
                    and target.id == "__test__"
                    and isinstance(stmt.value, ast.Constant)
                    and isinstance(stmt.value.value, bool)
                ):
                    result = stmt.value.value
        elif (
            isinstance(stmt, ast.AnnAssign)
            and isinstance(stmt.target, ast.Name)
            and stmt.target.id == "__test__"
            and isinstance(stmt.value, ast.Constant)
            and isinstance(stmt.value.value, bool)
        ):
            result = stmt.value.value
    return result


def _is_testcase_base_expr(base: ast.AST) -> bool:
    """Return True when class base expression names ``TestCase`` or ``*.TestCase``."""
    if isinstance(base, ast.Name):
        return base.id == "TestCase"
    return isinstance(base, ast.Attribute) and base.attr == "TestCase"


def _c3_merge(seqs: list[list[ast.ClassDef]]) -> list[ast.ClassDef] | None:
    """Perform Python C3 linearisation merge over non-empty class MRO sequences."""
    result: list[ast.ClassDef] = []
    pending = [list(seq) for seq in seqs if seq]
    while pending:
        candidate: ast.ClassDef | None = None
        for seq in pending:
            head = seq[0]
            if not any(head.name in {c.name for c in other[1:]} for other in pending):
                candidate = head
                break
        if candidate is None:
            return None
        result.append(candidate)
        pending = [[c for c in seq if c.name != candidate.name] for seq in pending]
        pending = [seq for seq in pending if seq]
    return result


def _resolve_local_mro(
    cls: ast.ClassDef,
    class_registry: Mapping[str, ast.ClassDef],
    *,
    seen: frozenset[str] = frozenset(),
) -> list[ast.ClassDef]:
    """Return Python C3 linearisation MRO over local classes (avoiding cycles)."""
    next_seen = seen | {cls.name}
    local_bases: list[ast.ClassDef] = []
    base_names: set[str] = set()
    for base in cls.bases:
        if (
            isinstance(base, ast.Name)
            and base.id in class_registry
            and base.id not in next_seen
            and base.id not in base_names
        ):
            base_names.add(base.id)
            local_bases.append(class_registry[base.id])
    if not local_bases:
        return [cls]
    parent_mros = [
        _resolve_local_mro(base_cls, class_registry, seen=next_seen)
        for base_cls in local_bases
    ]
    merged = _c3_merge([*parent_mros, local_bases])
    if merged is not None:
        return [cls, *merged]
    fallback: list[ast.ClassDef] = [cls]
    seen_fb = {cls.name}
    for seq in parent_mros:
        for anc in seq:
            if anc.name not in seen_fb:
                seen_fb.add(anc.name)
                fallback.append(anc)
    return fallback


def _is_pytest_collected_class(
    cls: ast.ClassDef,
    mro: list[ast.ClassDef],
) -> bool:
    """Return True when pytest will collect ``cls`` under default collection rules."""
    explicit_test_flag: bool | None = None
    for anc in mro:
        flag = _extract_class_test_flag(anc)
        if flag is not None:
            explicit_test_flag = flag
            break
    if explicit_test_flag is False:
        return False
    is_testcase = any(
        _is_testcase_base_expr(base) for anc in mro for base in anc.bases
    )
    if is_testcase:
        return True
    if explicit_test_flag is None and not cls.name.startswith("Test"):
        return False
    return not any(
        isinstance(defn, (ast.FunctionDef, ast.AsyncFunctionDef))
        and defn.name == "__init__"
        for anc in mro
        for defn in _iter_scope_defs(anc.body)
    )


def _collect_class_mro_marks(
    mro: list[ast.ClassDef],
    mod_env: Mapping[str, ast.AST],
) -> tuple[set[str], list[ast.AST]]:
    """Collect accumulated marker names and decorator/pytestmark AST nodes along ``mro``."""
    cls_marks: set[str] = set()
    cls_decs: list[ast.AST] = []
    for anc in reversed(mro):
        for dec in anc.decorator_list:
            d_names, _ = _inspect_decorator_expr(dec, mod_env)
            cls_marks.update(d_names)
        cls_pm, _ = _extract_scope_pytestmark(anc.body)
        cls_marks.update(cls_pm)
        cls_decs.extend(anc.decorator_list)
        cls_decs.extend(_extract_scope_pytestmark_exprs(anc.body))
    return (cls_marks, cls_decs)


def _visit_ast_guard_class(
    ctx: _FileAstGuardContext,
    snapshot: AstGuardsSnapshot,
    cls: ast.ClassDef,
    class_chain: tuple[str, ...],
    enclosing_decs: list[ast.AST],
    class_registry: Mapping[str, ast.ClassDef],
    *,
    inherited_param: bool,
    inherited_e2e: bool,
) -> None:
    """Inspect a pytest-collected class (including inherited methods and nested classes)."""
    mro = _resolve_local_mro(cls, class_registry)
    if not _is_pytest_collected_class(cls, mro):
        return

    cls_marks, cls_decs = _collect_class_mro_marks(mro, ctx.mod_env)
    next_chain = (*class_chain, cls.name)
    next_decs = [*enclosing_decs, *cls_decs]
    next_param = inherited_param or ("parametrize" in cls_marks)
    next_e2e = inherited_e2e or ("e2e" in cls_marks)

    nested_defs = _iter_scope_defs(cls.body)
    nested_registry = {
        **class_registry,
        **{item.name: item for item in nested_defs if isinstance(item, ast.ClassDef)},
    }

    seen_methods: set[str] = set()
    for anc in mro:
        anc_methods = [
            defn
            for defn in _iter_scope_defs(anc.body)
            if isinstance(defn, (ast.FunctionDef, ast.AsyncFunctionDef))
        ]
        for method in anc_methods:
            if method.name not in seen_methods:
                _visit_ast_guard_function(
                    ctx,
                    snapshot,
                    method,
                    next_chain,
                    next_decs,
                    inherited_param=next_param,
                    inherited_e2e=next_e2e,
                )
        seen_methods.update(m.name for m in anc_methods)

    for item in nested_defs:
        if isinstance(item, ast.ClassDef):
            _visit_ast_guard_class(
                ctx,
                snapshot,
                item,
                next_chain,
                next_decs,
                nested_registry,
                inherited_param=next_param,
                inherited_e2e=next_e2e,
            )


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

    ctx = _FileAstGuardContext(rel_posix, tree, extra_markers)
    snapshot = AstGuardsSnapshot()
    top_defs = _iter_scope_defs(tree.body)
    class_registry: dict[str, ast.ClassDef] = {
        defn.name: defn for defn in top_defs if isinstance(defn, ast.ClassDef)
    }
    for defn in top_defs:
        if isinstance(defn, (ast.FunctionDef, ast.AsyncFunctionDef)):
            _visit_ast_guard_function(
                ctx, snapshot, defn, (), [], inherited_param=False, inherited_e2e=False
            )
        elif isinstance(defn, ast.ClassDef):
            _visit_ast_guard_class(
                ctx,
                snapshot,
                defn,
                (),
                [],
                class_registry,
                inherited_param=False,
                inherited_e2e=False,
            )
    for (_node_id, cli), count in ctx.per_node_e2e_counts.items():
        snapshot.e2e_command_nodes[cli] = (
            snapshot.e2e_command_nodes.get(cli, 0) + count
        )
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
        combined.unresolvable_e2e_parametrize.update(
            file_snap.unresolvable_e2e_parametrize
        )
        combined.subprocess_analysis_overflow.update(
            file_snap.subprocess_analysis_overflow
        )
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
    combined.unresolvable_e2e_parametrize = dict(
        sorted(combined.unresolvable_e2e_parametrize.items())
    )
    combined.subprocess_analysis_overflow = dict(
        sorted(combined.subprocess_analysis_overflow.items())
    )
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
        "unresolvable_e2e_parametrize": {},
    }


def _is_positive_int(value: object) -> bool:
    """Return True when ``value`` is a positive ``int`` (and not a ``bool``)."""
    return isinstance(value, int) and not isinstance(value, bool) and value > 0


def _validate_cli_map_section(
    raw_map: object,
    section_name: str,
    errors: list[str],
) -> dict[str, list[str]]:
    """Validate a ``{node_id: [cli_cmd, ...]}`` ratchet mapping."""
    clean: dict[str, list[str]] = {}
    if not isinstance(raw_map, dict):
        errors.append(f"baseline {section_name} must be an object")
        return clean
    for node_id, cmds in sorted(raw_map.items()):
        if (
            isinstance(node_id, str)
            and isinstance(cmds, list)
            and cmds
            and all(isinstance(c, str) and c in ALL_SUEWS_CLI_COMMANDS for c in cmds)
        ):
            clean[node_id] = sorted(set(cmds))
        else:
            errors.append(f"invalid {section_name} entry: {node_id!r}: {cmds!r}")
    return clean


def _validate_baseline_sections(
    raw: Mapping[str, object],
    *,
    default_cap: int,
) -> tuple[
    dict[str, int],
    dict[str, list[str]],
    dict[str, int],
    dict[str, list[str]],
    list[str],
]:
    """Validate the ratchet maps inside an AST guards baseline payload."""
    errors: list[str] = []
    param_clean: dict[str, int] = {}
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

    unmarked_clean = _validate_cli_map_section(
        raw.get("unmarked_subprocess_e2e", {}), "unmarked_subprocess_e2e", errors
    )
    unresolvable_clean = _validate_cli_map_section(
        raw.get("unresolvable_e2e_parametrize", {}),
        "unresolvable_e2e_parametrize",
        errors,
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

    return (param_clean, unmarked_clean, caps_clean, unresolvable_clean, errors)


def _parse_ast_guards_baseline_text(
    text: str,
    source_label: str,
    *,
    default_cap: int = DEFAULT_E2E_COMMAND_NODE_CAP,
) -> tuple[dict[str, object] | None, list[str]]:
    """Parse and validate AST guards baseline JSON text."""
    errors: list[str] = []
    try:
        raw = json.loads(text)
    except json.JSONDecodeError as exc:
        return (None, [f"unable to load AST guards baseline {source_label}: {exc}"])

    if not isinstance(raw, dict):
        return (None, ["AST guards baseline must be a JSON object"])

    unknown_keys = sorted(set(raw) - _BASELINE_ALLOWED_KEYS)
    if unknown_keys:
        errors.append(f"unknown baseline keys: {', '.join(unknown_keys)}")

    version = raw.get("schema_version")
    if (
        not isinstance(version, int)
        or isinstance(version, bool)
        or version != AST_GUARDS_BASELINE_SCHEMA_VERSION
    ):
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

    param_clean, unmarked_clean, caps_clean, unresolvable_clean, section_errors = (
        _validate_baseline_sections(raw, default_cap=default_cap)
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
            "unresolvable_e2e_parametrize": unresolvable_clean,
        },
        [],
    )


def load_ast_guards_baseline(
    baseline_path: Path,
    *,
    default_cap: int = DEFAULT_E2E_COMMAND_NODE_CAP,
) -> tuple[dict[str, object] | None, list[str]]:
    """Load and validate an AST guards baseline JSON file."""
    try:
        text = baseline_path.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as exc:
        return (
            None,
            [f"unable to load AST guards baseline {baseline_path.name}: {exc}"],
        )
    return _parse_ast_guards_baseline_text(
        text, baseline_path.name, default_cap=default_cap
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
    payload: dict[str, object] = {
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
    if snapshot.unresolvable_e2e_parametrize:
        payload["unresolvable_e2e_parametrize"] = {
            k: sorted(set(v))
            for k, v in sorted(snapshot.unresolvable_e2e_parametrize.items())
        }
    baseline_path.parent.mkdir(parents=True, exist_ok=True)
    baseline_path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def _node_belongs_to_skipped(node_id: str, skipped_files: frozenset[str]) -> bool:
    """Return True if ``node_id`` belongs to a file in ``skipped_files``."""
    return node_id.split("::", 1)[0] in skipped_files


def _compare_cli_map_section(
    live_map: Mapping[str, list[str]],
    base_map: Mapping[str, list[str]],
    *,
    violation_prefix: str,
    violation_detail: str,
    stale_prefix: str,
    skipped_files: frozenset[str],
) -> tuple[list[str], list[str]]:
    """Compare a ``{node_id: [cli, ...]}`` snapshot map against its baseline map."""
    violations: list[str] = []
    stale: list[str] = []
    for node_id in sorted(set(live_map) | set(base_map)):
        now_cmds = set(live_map.get(node_id, ()))
        then_cmds = set(base_map.get(node_id, ()))
        added = sorted(now_cmds - then_cmds)
        removed = sorted(then_cmds - now_cmds)
        if added:
            violations.append(
                f"{violation_prefix}: {node_id} {violation_detail} "
                f"({', '.join(added)})"
            )
        if removed and not _node_belongs_to_skipped(node_id, skipped_files):
            stale.append(
                f"{stale_prefix} baseline stale: {node_id} "
                f"(removed {', '.join(removed)})"
            )
    return (violations, stale)


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
                f"parametrised-simulation: {node_id} calls from_sample_data()/.run() "
                f"directly inside @pytest.mark.parametrize ({then} -> {now}); "
                "inject a session/module fixture instead"
            )
        elif now < then and not _node_belongs_to_skipped(node_id, skipped_files):
            stale.append(
                f"parametrised-simulation baseline stale: {node_id} ({then} -> {now})"
            )

    base_unmarked: Mapping[str, list[str]] = baseline.get("unmarked_subprocess_e2e", {})  # type: ignore[assignment]
    unm_v, unm_s = _compare_cli_map_section(
        snapshot.unmarked_subprocess_e2e,
        base_unmarked,
        violation_prefix="missing-e2e-marker",
        violation_detail="invokes SUEWS CLI subprocess without @pytest.mark.e2e",
        stale_prefix="unmarked-subprocess-e2e",
        skipped_files=skipped_files,
    )
    violations.extend(unm_v)
    stale.extend(unm_s)

    base_unres: Mapping[str, list[str]] = baseline.get(
        "unresolvable_e2e_parametrize", {}
    )  # type: ignore[assignment]
    unr_v, unr_s = _compare_cli_map_section(
        snapshot.unresolvable_e2e_parametrize,
        base_unres,
        violation_prefix="unresolvable-e2e-parametrize",
        violation_detail=(
            "uses non-static @pytest.mark.parametrize argvalues on an "
            "@pytest.mark.e2e subprocess test"
        ),
        stale_prefix="unresolvable-e2e-parametrize",
        skipped_files=skipped_files,
    )
    violations.extend(unr_v)
    stale.extend(unr_s)

    for node_id, cmds in sorted(snapshot.subprocess_analysis_overflow.items()):
        violations.append(
            f"subprocess-analysis-overflow: {node_id} exceeded static branch "
            f"bound ({_MAX_BRANCH_BINDINGS}) while resolving subprocess command "
            f"({', '.join(cmds)}); simplify command construction"
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


def compare_ast_baselines_monotonic(
    base_baseline: Mapping[str, object],
    current_baseline: Mapping[str, object],
    *,
    default_cap: int = DEFAULT_E2E_COMMAND_NODE_CAP,
) -> list[str]:
    """Return errors if ``current_baseline`` adds or increases debt over ``base_baseline``."""
    errors: list[str] = []
    base_cap = int(base_baseline.get("e2e_default_cap", default_cap))  # type: ignore[arg-type]
    cur_cap = int(current_baseline.get("e2e_default_cap", default_cap))  # type: ignore[arg-type]
    if cur_cap > base_cap:
        errors.append(
            f"AST guards baseline increased e2e_default_cap ({base_cap} -> {cur_cap})"
        )

    base_param: Mapping[str, int] = base_baseline.get("parametrized_simulations", {})  # type: ignore[assignment]
    cur_param: Mapping[str, int] = current_baseline.get("parametrized_simulations", {})  # type: ignore[assignment]
    for node_id, now in sorted(cur_param.items()):
        then = base_param.get(node_id, 0)
        if now > then:
            errors.append(
                f"AST guards baseline increased parametrised-simulation debt: "
                f"{node_id} ({then} -> {now})"
            )

    for section, label in (
        ("unmarked_subprocess_e2e", "unmarked-subprocess-e2e"),
        ("unresolvable_e2e_parametrize", "unresolvable-e2e-parametrize"),
    ):
        base_map: Mapping[str, list[str]] = base_baseline.get(section, {})  # type: ignore[assignment]
        cur_map: Mapping[str, list[str]] = current_baseline.get(section, {})  # type: ignore[assignment]
        for node_id, now_cmds in sorted(cur_map.items()):
            added = sorted(set(now_cmds) - set(base_map.get(node_id, ())))
            if added:
                errors.append(
                    f"AST guards baseline added {label} debt: {node_id} "
                    f"({', '.join(added)})"
                )

    base_caps: Mapping[str, int] = base_baseline.get("e2e_command_caps", {})  # type: ignore[assignment]
    cur_caps: Mapping[str, int] = current_baseline.get("e2e_command_caps", {})  # type: ignore[assignment]
    for cmd, now_cap in sorted(cur_caps.items()):
        then_eff = base_caps.get(cmd, base_cap)
        if now_cap > then_eff:
            errors.append(
                f"AST guards baseline increased e2e-subprocess-cap for `{cmd}` "
                f"({then_eff} -> {now_cap})"
            )
    return errors


def _walk_components_for_links(
    start_root: Path,
    rel_parts: tuple[str, ...],
    *,
    skip_darwin_system_roots: bool = False,
) -> Path | None:
    """Walk ``start_root / rel_parts`` component-by-component; return first link/junction."""
    cur = start_root
    for part in rel_parts:
        cur /= part
        if skip_darwin_system_roots and cur in _DARWIN_SYSTEM_LINK_ROOTS:
            continue
        if _is_link_or_junction(cur):
            return cur
    return None


def _validate_in_repo_baseline_path(
    baseline_path: Path,
    in_repo_root: Path,
    resolved_root: Path,
    abs_baseline: Path,
    *,
    allow_external: bool,
) -> str | None:
    """Validate symlink/junction safety for a baseline path lexically under ``repo_root``."""
    bad = _walk_components_for_links(
        in_repo_root, abs_baseline.relative_to(in_repo_root).parts
    )
    if bad is not None:
        rel_label = bad.relative_to(in_repo_root).as_posix()
        return (
            f"AST guards baseline path traverses a symlink or junction: {rel_label}"
        )
    if allow_external:
        return None
    try:
        if not baseline_path.resolve(strict=False).is_relative_to(resolved_root):
            return (
                "AST guards baseline resolved path escapes repository root: "
                f"{baseline_path.as_posix()}"
            )
    except (OSError, RuntimeError) as exc:
        return f"unable to resolve AST guards baseline path: {exc}"
    return None


def _validate_baseline_path(
    baseline_path: Path,
    repo_root: Path,
    *,
    allow_external: bool,
) -> str | None:
    """Validate symlink/junction safety and repository containment for a baseline path."""
    if ".." in baseline_path.parts:
        return (
            "AST guards baseline path must not contain parent traversal ('..'): "
            f"{baseline_path.as_posix()}"
        )
    if _is_link_or_junction(baseline_path):
        return (
            "AST guards baseline path must not be a symlink or junction: "
            f"{baseline_path.as_posix()}"
        )

    abs_baseline = Path(os.path.abspath(baseline_path))
    abs_root = Path(os.path.abspath(repo_root))
    resolved_root = repo_root.resolve()

    for candidate_root in (resolved_root, abs_root):
        if abs_baseline.is_relative_to(candidate_root):
            return _validate_in_repo_baseline_path(
                baseline_path,
                candidate_root,
                resolved_root,
                abs_baseline,
                allow_external=allow_external,
            )

    if not allow_external:
        return (
            "AST guards baseline path escapes repository root: "
            f"{baseline_path.as_posix()}"
        )

    try:
        common = Path(os.path.commonpath([str(abs_baseline), str(resolved_root)]))
    except ValueError:
        common = Path(abs_baseline.anchor)
    skip_darwin = sys.platform == "darwin" and common == Path("/")
    bad = _walk_components_for_links(
        common,
        abs_baseline.relative_to(common).parts,
        skip_darwin_system_roots=skip_darwin,
    )
    if bad is not None:
        return (
            "AST guards baseline path traverses a symlink or junction: "
            f"{bad.as_posix()}"
        )
    return None


def _git_ref_exists(repo_root: Path, ref: str) -> bool:
    """Return True if ``ref`` resolves to a commit in ``repo_root``."""
    try:
        res = subprocess.run(
            ["git", "-C", str(repo_root), "rev-parse", "--verify", "--quiet", f"{ref}^{{commit}}"],
            capture_output=True,
            text=True,
            encoding="utf-8",
            check=False,
        )
    except OSError:
        return False
    return res.returncode == 0


def _read_git_baseline_at_ref(
    repo_root: Path,
    ref: str,
    rel_posix: str,
    *,
    default_cap: int,
) -> tuple[dict[str, object] | None, list[str]]:
    """Read and validate ``rel_posix`` at git ``ref``; return ``(None, [])`` if absent there."""
    try:
        res = subprocess.run(
            ["git", "-C", str(repo_root), "show", f"{ref}:{rel_posix}"],
            capture_output=True,
            text=True,
            encoding="utf-8",
            check=False,
        )
    except OSError as exc:
        return (None, [f"unable to run git show for base ref {ref!r}: {exc}"])
    if res.returncode != 0:
        stderr = (res.stderr or "").strip()
        if (
            "does not exist in" in stderr
            or "exists on disk, but not in" in stderr
            or "path '" in stderr
        ):
            return (None, [])
        return (None, [f"unable to read baseline at git ref {ref!r}: {stderr}"])
    return _parse_ast_guards_baseline_text(
        res.stdout, f"{ref}:{rel_posix}", default_cap=default_cap
    )


_SAFE_GIT_REF_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._/-]*$")


def _is_git_repo(repo_root: Path) -> bool:
    """Return True if ``repo_root`` lies inside a git work tree."""
    try:
        res = subprocess.run(
            ["git", "-C", str(repo_root), "rev-parse", "--is-inside-work-tree"],
            capture_output=True,
            text=True,
            encoding="utf-8",
            check=False,
        )
    except OSError:
        return False
    return res.returncode == 0 and res.stdout.strip() == "true"


def _try_fetch_origin_base_ref(repo_root: Path, base_branch: str) -> bool:
    """Best-effort shallow fetch of ``origin/<base_branch>`` for shallow CI checkouts."""
    if not _SAFE_GIT_REF_RE.fullmatch(base_branch) or ".." in base_branch:
        return False
    env = {**os.environ, "GIT_TERMINAL_PROMPT": "0"}
    try:
        res = subprocess.run(
            [
                "git",
                "-C",
                str(repo_root),
                "fetch",
                "--no-tags",
                "--depth=1",
                "origin",
                f"+refs/heads/{base_branch}:refs/remotes/origin/{base_branch}",
            ],
            capture_output=True,
            text=True,
            encoding="utf-8",
            env=env,
            timeout=15,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return False
    return res.returncode == 0 and _git_ref_exists(repo_root, f"origin/{base_branch}")


def _read_github_event_base_branch() -> str | None:
    """Extract target base branch from ``GITHUB_EVENT_PATH`` for PR/merge_group events."""
    event_path = os.environ.get("GITHUB_EVENT_PATH", "").strip()
    if not event_path:
        return None
    try:
        payload = json.loads(Path(event_path).read_text(encoding="utf-8"))
    except (OSError, UnicodeError, ValueError):
        return None
    if not isinstance(payload, dict):
        return None
    mg = payload.get("merge_group")
    if isinstance(mg, dict):
        base_ref = mg.get("base_ref")
        if isinstance(base_ref, str) and base_ref:
            return base_ref.removeprefix("refs/heads/").strip() or None
    pr = payload.get("pull_request")
    if isinstance(pr, dict):
        base = pr.get("base")
        if isinstance(base, dict) and isinstance(base.get("ref"), str):
            return base["ref"].strip() or None
    return None


def _discover_ci_base_ref(
    repo_root: Path, base_candidates: tuple[str, ...], context_label: str
) -> tuple[str | None, str | None]:
    """Resolve an independent trusted base ref for CI PR/merge_group checkouts."""
    if not _is_git_repo(repo_root):
        return (None, None)
    for branch in base_candidates:
        for ref in (f"origin/{branch}", branch):
            if _git_ref_exists(repo_root, ref):
                return (ref, None)
    if _git_ref_exists(repo_root, "HEAD^2") and _git_ref_exists(repo_root, "HEAD^1"):
        return ("HEAD^1", None)
    for branch in base_candidates:
        if _try_fetch_origin_base_ref(repo_root, branch):
            return (f"origin/{branch}", None)
    joined = ", ".join(f"origin/{b}" for b in base_candidates)
    return (
        None,
        f"unable to resolve independent trusted base ref for {context_label} "
        f"(shallow checkout without base history); fetch {joined} "
        "or pass --base-ref / --base-ast-baseline",
    )


def _discover_trusted_base_ref(repo_root: Path) -> tuple[str | None, str | None]:
    """Discover a trusted git base revision; fail closed in shallow CI checkouts."""
    env_base = os.environ.get("GITHUB_BASE_REF", "").strip()
    if env_base:
        return _discover_ci_base_ref(
            repo_root, (env_base,), f"GITHUB_BASE_REF={env_base!r}"
        )
    event_name = os.environ.get("GITHUB_EVENT_NAME", "").strip()
    if event_name in {"pull_request", "pull_request_target", "merge_group"}:
        event_base = _read_github_event_base_branch()
        branches = (event_base,) if event_base else ("master", "main")
        return _discover_ci_base_ref(
            repo_root, branches, f"GITHUB_EVENT_NAME={event_name!r}"
        )
    for ref in ("origin/master", "origin/main", "master", "main", "HEAD"):
        if _git_ref_exists(repo_root, ref):
            return (ref, None)
    return (None, None)


def _load_trusted_base_baseline(
    repo_root: Path,
    target_baseline: Path,
    *,
    explicit_base_file: Path | None,
    explicit_base_ref: str | None,
    default_cap: int,
) -> tuple[dict[str, object] | None, list[str]]:
    """Load the trusted base baseline for checking that the baseline file did not grow."""
    if explicit_base_file is not None:
        return _resolve_explicit_baseline(
            explicit_base_file, repo_root, default_cap=default_cap
        )

    rel_posix = AST_GUARDS_BASELINE_PATH.as_posix()
    if explicit_base_ref is not None:
        if not _git_ref_exists(repo_root, explicit_base_ref):
            return (
                None,
                [f"git base ref {explicit_base_ref!r} not found in repository"],
            )
        return _read_git_baseline_at_ref(
            repo_root, explicit_base_ref, rel_posix, default_cap=default_cap
        )

    default_path = (repo_root / AST_GUARDS_BASELINE_PATH).resolve(strict=False)
    base_ref: str | None = None
    discover_err: str | None = None
    if target_baseline.resolve(strict=False) == default_path:
        base_ref, discover_err = _discover_trusted_base_ref(repo_root)
    if discover_err is not None:
        return (None, [discover_err])
    if base_ref is None:
        return (None, [])
    return _read_git_baseline_at_ref(
        repo_root, base_ref, rel_posix, default_cap=default_cap
    )


def _resolve_explicit_baseline(
    explicit_baseline: Path,
    repo_root: Path,
    *,
    default_cap: int,
) -> tuple[dict[str, object] | None, list[str]]:
    """Validate and load an explicitly requested ``--ast-baseline`` file."""
    path_err = _validate_baseline_path(
        explicit_baseline, repo_root, allow_external=True
    )
    if path_err is not None:
        return (None, [path_err])
    if not explicit_baseline.is_file():
        return (
            None,
            [
                "AST guards baseline not found at "
                f"{explicit_baseline.as_posix()}"
            ],
        )
    return load_ast_guards_baseline(explicit_baseline, default_cap=default_cap)


def _resolve_ast_baseline(
    repo_root: Path,
    explicit_baseline: Path | None,
    *,
    default_cap: int = DEFAULT_E2E_COMMAND_NODE_CAP,
) -> tuple[dict[str, object] | None, list[str]]:
    """Resolve and load the AST guards baseline for ``repo_root``."""
    if explicit_baseline is not None:
        return _resolve_explicit_baseline(
            explicit_baseline, repo_root, default_cap=default_cap
        )

    default_path = repo_root / AST_GUARDS_BASELINE_PATH
    path_err = _validate_baseline_path(default_path, repo_root, allow_external=False)
    if path_err is not None:
        return (None, [path_err])
    if default_path.is_file():
        return load_ast_guards_baseline(default_path, default_cap=default_cap)
    if (repo_root / "scripts" / "lint").is_dir():
        return (
            None,
            [
                "missing AST guards baseline "
                f"{AST_GUARDS_BASELINE_PATH.as_posix()}; initialise with "
                "`python scripts/lint/check_test_markers.py --init-ast-baseline`"
            ],
        )
    return (_empty_ast_guards_baseline(default_cap=default_cap), [])


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
            "[X] gh#2349 AST guard lint: invalid, missing, or regressed AST guards baseline:",
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


def _check_monotonic_write_allowed(
    repo_root: Path,
    target_baseline: Path,
    ast_snapshot: AstGuardsSnapshot,
    *,
    init_mode: bool,
    default_cap: int,
    explicit_base_file: Path | None,
    explicit_base_ref: str | None,
) -> list[str]:
    """Verify baseline creation or downward-only regeneration preconditions."""
    if ast_snapshot.subprocess_analysis_overflow:
        return [
            "refusing to write AST guards baseline while subprocess analysis "
            "overflow errors exist:",
            *sorted(ast_snapshot.subprocess_analysis_overflow),
        ]
    exists = target_baseline.exists()
    if init_mode:
        return (
            [
                f"baseline already exists at {target_baseline.as_posix()}; "
                "use --write-ast-baseline to lower existing debt"
            ]
            if exists
            else []
        )
    if not exists:
        return [
            f"baseline not found at {target_baseline.as_posix()}; "
            "use --init-ast-baseline for initial creation"
        ]
    existing, load_errors = load_ast_guards_baseline(
        target_baseline, default_cap=default_cap
    )
    if existing is None:
        return load_errors

    errors: list[str] = []
    increases, _ = compare_ast_guards_baseline(
        ast_snapshot, existing, default_cap=default_cap
    )
    if increases:
        errors.extend([
            "refusing to increase AST guards baseline via --write-ast-baseline "
            "(ratchet is downward-only):",
            *increases,
        ])
    trusted_base, base_errs = _load_trusted_base_baseline(
        repo_root,
        target_baseline,
        explicit_base_file=explicit_base_file,
        explicit_base_ref=explicit_base_ref,
        default_cap=default_cap,
    )
    errors.extend(base_errs)
    if trusted_base is not None:
        base_increases, _ = compare_ast_guards_baseline(
            ast_snapshot, trusted_base, default_cap=default_cap
        )
        if base_increases:
            errors.extend([
                "refusing to increase AST guards baseline above trusted base "
                "revision via --write-ast-baseline:",
                *base_increases,
            ])
    return errors


def _write_baseline_mode(
    repo_root: Path,
    args: argparse.Namespace,
    ast_snapshot: AstGuardsSnapshot,
    *,
    has_incomplete_discovery: bool,
) -> int:
    """Execute ``--init-ast-baseline`` or downward-only ``--write-ast-baseline``."""
    if has_incomplete_discovery:
        print(
            "[X] gh#2349 AST guard lint: cannot write baseline while test "
            "discovery, layout, read, or parse errors are present.",
            file=sys.stderr,
        )
        return 1
    target_baseline = (
        args.ast_baseline
        if args.ast_baseline is not None
        else repo_root / AST_GUARDS_BASELINE_PATH
    )
    path_err = _validate_baseline_path(
        target_baseline, repo_root, allow_external=(args.ast_baseline is not None)
    )
    if path_err is not None:
        print(_to_ascii(f"[X] gh#2349 AST guard lint: {path_err}"), file=sys.stderr)
        return 1

    guard_errors = _check_monotonic_write_allowed(
        repo_root,
        target_baseline,
        ast_snapshot,
        init_mode=args.init_ast_baseline,
        default_cap=args.e2e_cap,
        explicit_base_file=args.base_ast_baseline,
        explicit_base_ref=args.base_ref,
    )
    if guard_errors:
        _print_error_block("[X] gh#2349 AST guard lint:", guard_errors)
        return 1

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
    base_group = parser.add_mutually_exclusive_group()
    base_group.add_argument(
        "--base-ast-baseline",
        type=Path,
        default=None,
        help=(
            "Trusted base AST guards baseline JSON file to verify monotonic "
            "non-increase of the current baseline."
        ),
    )
    base_group.add_argument(
        "--base-ref",
        default=None,
        help=(
            "Trusted git ref (e.g. origin/master) from which to read the prior "
            "AST guards baseline for monotonic non-increase verification."
        ),
    )
    mode_group = parser.add_mutually_exclusive_group()
    mode_group.add_argument(
        "--write-ast-baseline",
        action="store_true",
        help=(
            "Lower an existing AST guards baseline JSON from the current tree "
            "(downward-only ratchet) and exit 0."
        ),
    )
    mode_group.add_argument(
        "--init-ast-baseline",
        action="store_true",
        help=(
            "Create an initial AST guards baseline JSON when none exists yet "
            "and exit 0."
        ),
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

    target_path = (
        args.ast_baseline
        if args.ast_baseline is not None
        else repo_root / AST_GUARDS_BASELINE_PATH
    )
    trusted_base, base_errs = _load_trusted_base_baseline(
        repo_root,
        target_path,
        explicit_base_file=args.base_ast_baseline,
        explicit_base_ref=args.base_ref,
        default_cap=args.e2e_cap,
    )
    baseline_errors.extend(base_errs)
    if trusted_base is not None:
        baseline_errors.extend(
            compare_ast_baselines_monotonic(
                trusted_base, baseline, default_cap=args.e2e_cap
            )
        )

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

    if args.write_ast_baseline or args.init_ast_baseline:
        return _write_baseline_mode(
            repo_root,
            args,
            ast_snapshot,
            has_incomplete_discovery=bool(lane_errors or read_errors or ast_skipped),
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
